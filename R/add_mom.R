#' Add Momentum Factor(s)
#'
#' Calculate momentum factors using TTR::ROC for multiple periods, grouped by stock.
#' Accepts n as a single value OR a vector (e.g., n = c(2,5,10)).
#'
#' @param data Standard long data from get_data()
#' @param close_col Close price column
#' @param new_col Prefix for auto-naming (default: "mom")
#' @param n Lookback period(s) (single value or vector, e.g., c(2,5,10))
#' @param type "continuous" (log) or "discrete" (simple)
#' @param na.pad Pad leading NAs
#' @param append If TRUE, append to data; if FALSE, return date+code+name+factors
#' @param output "tibble" or "data.frame"
#'
#' @return Data frame or tibble with momentum factor(s)
#' @export
#' @importFrom dplyr group_by mutate ungroup arrange select across all_of
#' @importFrom TTR ROC
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' # ------------------------------
#' # Demo 1: Basic usage
#' # ------------------------------
#' library(eClassic)
#'
#' # Create ETF info data frame
#' etf_info <- data.frame(
#'   category = c("CSI300", "Nasdaq100", "Gold"),
#'   name = c("CSI300ETF", "NDQ100ETF", "AUETF"),
#'   code = c("510300", "513100", "518880"),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Get data
#' dat <- get_data(etf_info, "2024-01-01", "2025-12-31")
#'
#' # Add momentum factors (5, 10, 20 days)
#' dat_mom <- add_mom(dat, n = c(5, 10, 20))
#' head(dat_mom)
#'
#'
#' # ------------------------------
#' # Demo 2: Only return factors (append = FALSE)
#' # ------------------------------
#' dat_mom_only <- add_mom(dat, n = 10, append = FALSE)
#' head(dat_mom_only)
#'
#'
#' # ------------------------------
#' # Demo 3: Simple return (discrete)
#' # ------------------------------
#' dat_mom_discrete <- add_mom(dat, n = 5, type = "discrete")
#' head(dat_mom_discrete)
#' }
add_mom <- function(data,
                    close_col = "close",
                    new_col = "mom",
                    n = c(2, 5, 10),
                    type = c("continuous", "discrete"),
                    na.pad = TRUE,
                    append = TRUE,
                    output = c("tibble", "data.frame")) {
  type <- match.arg(type)
  output <- match.arg(output)
  n <- as.integer(n)

  res <- data %>%
    dplyr::group_by(code) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::ungroup()

  for (period in n) {
    colname <- paste0(new_col, "_", period)
    res <- res %>%
      dplyr::group_by(code) %>%
      dplyr::mutate(
        !!colname := as.numeric(TTR::ROC(
          x = .data[[close_col]],
          n = period,
          type = type,
          na.pad = na.pad
        ))
      ) %>%
      dplyr::ungroup()
  }

  if (!append) {
    keep_cols <- c("date", "code", "name", paste0(new_col, "_", n))
    res <- res %>% dplyr::select(dplyr::all_of(keep_cols))
  }

  if (output == "data.frame") {
    res <- as.data.frame(res, stringsAsFactors = FALSE)
  } else {
    res <- tibble::as_tibble(res)
  }

  return(res)
}
