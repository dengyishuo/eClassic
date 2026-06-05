#' Add Rolling Volatility (Standard Deviation / Variance + Annualized)
#'
#' Professional rolling volatility calculator with:
#' - Return type (log / simple)
#' - Volatility type (std dev / variance)
#' - Annualized volatility option
#' Automatically computes 1-day returns internally.
#'
#' @param data Standard long data from get_data()
#' @param close_col Close price column
#' @param new_col Prefix for output columns
#' @param n Lookback periods (vector allowed: c(5,10,20))
#' @param ret_type Return type: "continuous" (log) or "discrete" (simple)
#' @param vol_type Volatility type: "sd" (standard deviation) or "var" (variance)
#' @param annualized Logical. If TRUE, annualize volatility (252 trading days)
#' @param trade_days Numeric. Annual trading days (default 252)
#' @param na.pad Pad leading NAs
#' @param append Append to data or return only factors
#' @param output "tibble" or "data.frame"
#'
#' @return Data frame with professional volatility factors
#' @export
#' @importFrom dplyr group_by mutate ungroup arrange select all_of
#' @importFrom TTR ROC
#' @importFrom zoo rollapply
#' @importFrom tibble as_tibble
#' @importFrom stats sd var
#'
#' @examples
#' \dontrun{
#' library(FactorCraft)
#' data(global_core_assets)
#'
#' # Standard 20-day rolling std (not annualized)
#' dat1 <- add_volatility(global_core_assets, n = 20, vol_type = "sd")
#'
#' # 20-day annualized volatility (institutional standard)
#' dat2 <- add_volatility(global_core_assets, n = 20, annualized = TRUE)
#'
#' # Rolling variance
#' dat3 <- add_volatility(global_core_assets, vol_type = "var")
#' }
add_volatility <- function(data,
                           close_col = "close",
                           new_col = "vol",
                           n = c(5, 10, 20),
                           ret_type = c("continuous", "discrete"),
                           vol_type = c("sd", "var"),
                           annualized = FALSE,
                           trade_days = 252,
                           na.pad = TRUE,
                           append = TRUE,
                           output = c("tibble", "data.frame")) {
  ret_type <- match.arg(ret_type)
  vol_type <- match.arg(vol_type)
  output <- match.arg(output)
  n <- as.integer(n)

  # Select volatility function
  vol_fun <- if (vol_type == "sd") stats::sd else stats::var

  # Create clean column name prefix
  vol_str <- ifelse(vol_type == "sd", "sd", "var")
  ann_str <- ifelse(annualized, "_ann", "")
  col_base <- paste0(new_col, "_", vol_str, ann_str)

  # Sort data properly
  res <- data %>%
    group_by(code) %>%
    arrange(date, .by_group = TRUE) %>%
    ungroup()

  # ------------------------------
  # Internal 1-day return calculation
  # ------------------------------
  res <- res %>%
    group_by(code) %>%
    mutate(
      .ret_tmp = as.numeric(ROC(
        x = .data[[close_col]],
        n = 1,
        type = ret_type,
        na.pad = na.pad
      ))
    ) %>%
    ungroup()

  # ------------------------------
  # Calculate rolling volatility
  # ------------------------------
  for (period in n) {
    colname <- paste0(col_base, "_", period)

    res <- res %>%
      group_by(code) %>%
      mutate(
        !!colname := rollapply(
          .ret_tmp,
          width = period,
          FUN = vol_fun,
          fill = NA,
          align = "right"
        )
      ) %>%
      ungroup()

    # ------------------------------
    # Annualize if needed
    # ------------------------------
    if (annualized) {
      if (vol_type == "sd") {
        res[[colname]] <- res[[colname]] * sqrt(trade_days)
      } else {
        res[[colname]] <- res[[colname]] * trade_days
      }
    }
  }

  # Remove temp return column
  res <- res %>% select(-.ret_tmp)

  # Slim output
  if (!append) {
    keep_cols <- c("date", "code", "name", paste0(col_base, "_", n))
    res <- res %>% select(all_of(keep_cols))
  }

  # Output format
  if (output == "data.frame") {
    res <- as.data.frame(res, stringsAsFactors = FALSE)
  } else {
    res <- as_tibble(res)
  }

  return(res)
}
