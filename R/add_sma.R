#' Add Simple Moving Average (SMA) Price Factor
#'
#' Calculate rolling simple moving average of close price.
#' Directly computed on price series (standard SMA).
#'
#' @param data Standard long data from get_data()
#' @param close_col Close price column, default "close"
#' @param new_col Prefix for output SMA columns, default "sma"
#' @param n Rolling window size vector, default c(5,10,20)
#' @param na.pad Not used in SMA, kept for parameter consistency
#' @param append TRUE = append to data; FALSE = return clean factors
#' @param output "tibble" or "data.frame"
#'
#' @return Data frame with SMA factors
#' @export
#' @importFrom dplyr group_by mutate ungroup arrange select all_of
#' @importFrom TTR SMA
#' @importFrom tibble as_tibble
add_sma <- function(data,
                    close_col = "close",
                    new_col = "sma",
                    n = c(5, 10, 20),
                    na.pad = TRUE,
                    append = TRUE,
                    output = c("tibble", "data.frame")) {
  output <- match.arg(output)
  n <- as.integer(n)

  # Safe time-series sorting
  res <- data %>%
    dplyr::group_by(code) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::ungroup()

  # Direct SMA calculation on close price (clean & efficient)
  for (period in n) {
    colname <- paste0(new_col, "_", period)
    res <- res %>%
      dplyr::group_by(code) %>%
      dplyr::mutate(
        !!colname := TTR::SMA(.data[[close_col]], n = period)
      ) %>%
      dplyr::ungroup()
  }

  # Clean factor output
  if (!append) {
    keep_cols <- c("date", "code", "name", paste0(new_col, "_", n))
    res <- res %>% dplyr::select(dplyr::all_of(keep_cols))
  }

  # Format output
  if (output == "data.frame") {
    res <- as.data.frame(res, stringsAsFactors = FALSE)
  } else {
    res <- tibble::as_tibble(res)
  }

  return(res)
}
