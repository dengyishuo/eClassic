#' Add Periodic Returns Using TTR::ROC (Multiple Periods)
#'
#' Wrapper for TTR::ROC to calculate returns over multiple periods,
#' grouped by stock code. Automatically generates columns like ret_1, ret_5.
#'
#' @param data Standard long data from get_data()
#' @param close_col Close price column
#' @param new_col Prefix for output columns (default: "ret")
#' @param n Vector of periods (e.g., c(1,5,10))
#' @param type "continuous" (log) or "discrete" (simple)
#' @param na.pad Pad leading NAs
#' @param append If TRUE, append to data; if FALSE, return date+code+name+returns
#' @param output "tibble" or "data.frame"
#'
#' @return Data frame or tibble with return columns
#' @export
#' @importFrom dplyr group_by mutate ungroup arrange select all_of
#' @importFrom TTR ROC
#' @importFrom tibble as_tibble
add_return <- function(data,
                       close_col = "close",
                       new_col = "ret",
                       n = c(1, 5, 10),
                       type = c("continuous", "discrete"),
                       na.pad = TRUE,
                       append = TRUE,
                       output = c("tibble", "data.frame")) {
  # Match official TTR::ROC arguments
  type <- match.arg(type)
  output <- match.arg(output)
  n <- as.integer(n)

  # Initialize result with sorted data
  res <- data %>%
    dplyr::group_by(code) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::ungroup()

  # Calculate returns for each period in n
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

  # When append = FALSE: return date + code + name + return columns
  if (!append) {
    keep_cols <- c("date", "code", "name", paste0(new_col, "_", n))
    res <- res %>% dplyr::select(dplyr::all_of(keep_cols))
  }

  # Convert to desired output type
  if (output == "data.frame") {
    res <- as.data.frame(res, stringsAsFactors = FALSE)
  } else {
    res <- tibble::as_tibble(res)
  }

  return(res)
}
