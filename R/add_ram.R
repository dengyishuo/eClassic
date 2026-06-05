#' Add Risk-Adjusted Momentum (RAM)
#'
#' Calculate risk-adjusted momentum using return / risk.
#' Uses consistent naming with add_return() and add_vol_std().
#'
#' @param data Standard long data from get_data()
#' @param close_col Close price column
#' @param new_col Prefix for final RAM factor (default: "ram")
#' @param n Lookback periods (default: c(5,10,20))
#' @param type Return type: "continuous" or "discrete"
#' @param risk_type "vol" (std), "VaR", "CVaR"
#' @param p Confidence level for VaR/CVaR (default 0.95)
#' @param na.pad Pad leading NAs
#' @param append Append to data or return clean factors
#' @param output "tibble" or "data.frame"
#'
#' @return Tibble/data.frame with columns:
#'   \item{\code{ret_{n}}}{Period return (same as add_return)}
#'   \item{\code{vol_std_{n}}}{Rolling volatility (same as add_vol_std)}
#'   \item{\code{ram_{n}}}{Risk-adjusted momentum = ret / vol_std}
#'
#' @export
#' @importFrom dplyr group_by mutate ungroup arrange select all_of
#' @importFrom TTR ROC
#' @importFrom zoo rollapply
#' @importFrom tibble as_tibble
#' @importFrom stats sd quantile
#'
#' @examples
#' \dontrun{
#' # Load package and data
#' library(FactorCraft)
#' data(global_core_assets)
#'
#' # Add risk-adjusted momentum (5,10,20 days)
#' dat <- add_ram(global_core_assets, n = c(5, 10, 20))
#' head(dat)
#'
#' # Only return RAM factors (append = FALSE)
#' dat_ram <- add_ram(global_core_assets, n = 10, append = FALSE)
#' head(dat_ram)
#'
#' # Use VaR as risk measure
#' dat_var <- add_ram(global_core_assets, n = 10, risk_type = "VaR")
#' head(dat_var)
#' }
add_ram <- function(data,
                    close_col = "close",
                    new_col = "ram",
                    n = c(5, 10, 20),
                    type = c("continuous", "discrete"),
                    risk_type = c("vol", "VaR", "CVaR"),
                    p = 0.95,
                    na.pad = TRUE,
                    append = TRUE,
                    output = c("tibble", "data.frame")) {
  # Match input arguments to allowed values
  type <- match.arg(type)
  risk_type <- match.arg(risk_type)
  output <- match.arg(output)
  n <- as.integer(n)

  # Group by asset and sort by date
  res <- data %>%
    group_by(code) %>%
    arrange(date, .by_group = TRUE) %>%
    ungroup()

  # Compute 1-day returns for rolling risk calculation
  res <- res %>%
    group_by(code) %>%
    mutate(.ret1 = as.numeric(ROC(
      x = .data[[close_col]],
      n = 1,
      type = type,
      na.pad = na.pad
    ))) %>%
    ungroup()

  # Loop over each lookback period
  for (period in n) {
    # Define consistent column names
    col_ret <- paste0("ret_", period)
    col_risk <- paste0("vol_std_", period)
    col_ram <- paste0(new_col, "_", period)

    # Calculate period return
    res <- res %>%
      group_by(code) %>%
      mutate(!!col_ret := ROC(.data[[close_col]],
        n = period,
        type = type,
        na.pad = na.pad
      )) %>%
      ungroup()

    # Calculate rolling risk (volatility / VaR / CVaR)
    res <- res %>%
      group_by(code) %>%
      mutate(!!col_risk := rollapply(.ret1,
        width = period,
        FUN = function(x) {
          if (risk_type == "vol") {
            return(sd(x, na.rm = TRUE))
          } else if (risk_type == "VaR") {
            q <- quantile(x, 1 - p, na.rm = TRUE)
            return(abs(q))
          } else {
            q <- quantile(x, 1 - p, na.rm = TRUE)
            return(abs(mean(x[x <= q], na.rm = TRUE)))
          }
        },
        fill = NA,
        align = "right"
      )) %>%
      ungroup()

    # Compute risk-adjusted momentum = return / risk
    res <- res %>%
      mutate(!!col_ram := !!sym(col_ret) / !!sym(col_risk))
  }

  # Remove temporary 1-day return column
  res <- res %>% select(-.ret1)

  # Keep only key columns if append = FALSE
  if (!append) {
    keep_cols <- c("date", "code", "name", paste0(new_col, "_", n))
    res <- res %>% select(all_of(keep_cols))
  }

  # Convert to desired output type
  if (output == "data.frame") {
    res <- as.data.frame(res, stringsAsFactors = FALSE)
  } else {
    res <- as_tibble(res)
  }

  return(res)
}
