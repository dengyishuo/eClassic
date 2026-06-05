#' Add Rolling Beta Factor
#'
#' Calculate rolling beta (slope coefficient) from a linear regression of stock
#' returns on market returns (or any user-provided independent variable). Beta
#' measures the sensitivity of a stock's return to the market return, commonly
#' used in asset pricing and risk management.
#'
#' @param data A data.frame or tibble in long format (required columns: `date`,
#'   `code`, `name`, and the variables used in regression). Typically obtained
#'   via `get_data()` and `add_return()`, then enriched with benchmark returns
#'   using `add_benchmark()`.
#' @param y_col Character. Name of the dependent variable column (e.g., `"ret_1"`,
#'   the stock's daily return). Default is `"ret_1"`.
#' @param x_col Character. Name of the independent variable column (e.g.,
#'   `"benchmark_ret"`, the market index return). Default is `"benchmark_ret"`.
#' @param new_col Character. Prefix for the output beta columns. The actual
#'   column names will be `paste0(new_col, "_", n)`. Default is `"beta"`.
#' @param n Integer vector. Rolling window lengths (lookback periods) in days.
#'   Default is `c(60, 120, 250)` (approximately 3, 6, 12 months).
#' @param min_obs Integer. Minimum number of non-NA observation pairs required
#'   to compute beta in a rolling window. Default is 2L.
#' @param append Logical. If `TRUE`, append the new beta columns to the
#'   original data and return the full data set. If `FALSE`, return only
#'   the columns `date`, `code`, `name`, and the beta columns.
#' @param output Character. Either `"tibble"` (default) or `"data.frame"`.
#'   Defines the class of the returned object.
#'
#' @return A tibble or data.frame with added rolling beta columns named
#'   `beta_n` (or custom prefix).
#'
#' @details
#' The function computes rolling ordinary least squares regression of
#' `y_col ~ x_col` for each stock (grouped by `code`) after sorting by `date`.
#' The slope coefficient is the beta. The implementation uses `roll::roll_lm`
#' for efficiency.
#'
#' Missing values are handled by `min_obs` in `roll_lm`: only windows with
#' at least `min_obs` non‑missing observation pairs produce a numeric beta;
#' otherwise `NA` is returned.
#'
#' @seealso \code{\link{add_return}}, \code{\link{add_benchmark}}, \code{\link{add_slope}}
#' @export
#' @importFrom dplyr group_by mutate ungroup arrange select all_of
#' @importFrom rlang := .data
#' @importFrom roll roll_lm
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' library(FactorCraft)
#' library(dplyr)
#'
#' stock_df <- data.frame(
#'   code = c("000001.SZ", "000002.SZ", "000300.SS"),
#'   name = c("Ping An Bank", "Vanke A", "CSI300")
#' )
#'
#' dat <- get_data(stock_df, start = "2024-01-01", end = "2024-12-31") |>
#'   add_return(n = 1, type = "discrete") |>
#'   add_benchmark(market_code = "000300.SS")
#'
#' beta_data <- add_beta(dat, y_col = "ret_1", x_col = "benchmark_ret", n = 60)
#'
#' head(beta_data)
#' }
add_beta <- function(data,
                     y_col = "ret_1",
                     x_col = "benchmark_ret",
                     new_col = "beta",
                     n = c(60, 120, 250),
                     min_obs = 2L,
                     append = TRUE,
                     output = c("tibble", "data.frame")) {
  output <- match.arg(output)
  n <- as.integer(n)
  min_obs <- as.integer(min_obs)

  # Ensure data is sorted by date within each stock
  data_sorted <- data %>%
    dplyr::group_by(code) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::ungroup()

  res <- data_sorted

  # Compute rolling beta for each window length
  for (period in n) {
    colname <- paste0(new_col, "_", period)

    res <- res %>%
      dplyr::group_by(code) %>%
      dplyr::mutate(
        !!colname := {
          y_vec <- .data[[y_col]]
          x_vec <- .data[[x_col]]
          x_mat <- as.matrix(x_vec)

          fit <- roll::roll_lm(
            x = x_mat, y = y_vec, width = period,
            min_obs = min_obs
          )
          # Coefficients: column 1 = intercept, column 2 = slope (beta)
          beta_vec <- fit$coefficients[, 2]
          beta_vec
        }
      ) %>%
      dplyr::ungroup()
  }

  # Optionally keep only factor columns
  if (!append) {
    keep_cols <- c("date", "code", "name", paste0(new_col, "_", n))
    res <- res %>% dplyr::select(dplyr::all_of(keep_cols))
  }

  # Convert output class
  if (output == "data.frame") {
    res <- as.data.frame(res, stringsAsFactors = FALSE)
  } else {
    res <- tibble::as_tibble(res)
  }

  return(res)
}
