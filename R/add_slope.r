#' Add Rolling Slope Factor
#'
#' Calculate rolling linear regression slope (beta) for A-share stocks using
#' efficient rolling ordinary least squares. Slope reflects the linear trend
#' strength of a variable over a rolling window, or the sensitivity of `y` to `x`.
#'
#' @param data A data.frame or tibble in long format (required columns: `date`,
#'   `code`, `name`, and the variables used in regression). Typically obtained
#'   via `get_data()`, but can be any similarly structured data.
#' @param y_col Character. Name of the dependent variable column (e.g.,
#'   `"close"`, `"ret_1"`).
#' @param x_col Character. Name of the independent variable column. If `NULL`
#'   (default), the function uses the global observation index (1:N) as the
#'   independent variable. Because the index increments by 1 each period,
#'   the slope in each rolling window is equivalent to the time trend slope
#'   using window‑local indices 1:window. If `x_col` is provided, the model
#'   becomes `y ~ x` and the slope measures the sensitivity of `y` to `x`.
#' @param new_col Character. Prefix for the output slope columns. The actual
#'   column names will be `paste0(new_col, "_", n)`. Default is `"slope"`.
#' @param n Integer vector. Rolling window lengths (lookback periods). Default
#'   is `c(5, 10, 20)`.
#' @param na.pad Logical. If `TRUE` (default), the output slope columns have
#'   the same number of rows as the input data, with leading `NA`s padded.
#'   `roll::roll_lm` always pads with `NA` for incomplete windows, so this
#'   argument is respected.
#' @param append Logical. If `TRUE`, append the new slope columns to the
#'   original data and return the full data set. If `FALSE`, return only
#'   the columns `date`, `code`, `name`, and the slope columns.
#' @param output Character. Either `"tibble"` (default) or `"data.frame"`.
#'   Defines the class of the returned object.
#'
#' @return A tibble or data.frame containing the original data (if
#'   `append = TRUE`) or the subset of factor columns (if `append = FALSE`),
#'   with added rolling slope columns named `slope_n`.
#'
#' @details
#' The function computes rolling slopes for each stock (grouped by `code`)
#' after sorting by `date`. The underlying workhorse is `roll::roll_lm`,
#' which is significantly faster than rolling `lm()` calls.
#'
#' When `x_col = NULL`, the model is `y ~ global_index`, where
#' `global_index = 1:length(y)`. Because the global index is strictly increasing,
#' the estimated slope within any window is identical to using `1:window` as
#' the regressor (the intercept changes but the slope is invariant to translation).
#'
#' Missing values are handled by setting `min_obs = 2L` in `roll_lm`: only
#' windows with at least two non‑missing observations produce a numeric slope;
#' otherwise `NA` is returned.
#'
#' @seealso \code{\link{add_return}}, \code{\link{add_volatility}},
#'   \code{\link{add_mom}}
#'
#' @export
#'
#' @importFrom dplyr group_by mutate ungroup arrange select all_of
#' @importFrom rlang := .data
#' @importFrom roll roll_lm
#' @importFrom tibble as_tibble
#'
#' @examples
#' # ---------------------------
#' # Example 1: Simulated price data (no internet required)
#' # ---------------------------
#' library(dplyr)
#' set.seed(123)
#' sim_data <- expand.grid(
#'   date = seq.Date(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day"),
#'   code = c("000001", "000002"),
#'   stringsAsFactors = FALSE
#' ) %>%
#'   group_by(code) %>%
#'   mutate(
#'     name = ifelse(code == "000001", "Ping An", "Wan Ke"),
#'     close = cumsum(rnorm(n(), mean = 0.1, sd = 1)) + 10
#'   ) %>%
#'   ungroup()
#'
#' # Compute rolling slope of price (trend) with windows 5 and 10
#' result <- add_slope(sim_data, y_col = "close", n = c(5, 10))
#' print(result)
#'
#' # Return only factor columns
#' slope_only <- add_slope(sim_data, y_col = "close", n = 10, append = FALSE)
#' head(slope_only)
#'
#' # ---------------------------
#' # Example 2: Using a custom independent variable (x_col)
#' # ---------------------------
#' # Add lagged return and compute sensitivity of current return to lagged return
#' sim_data <- sim_data %>%
#'   group_by(code) %>%
#'   mutate(ret_1 = c(NA, diff(log(close)))) %>%
#'   mutate(ret_1_lag1 = lag(ret_1)) %>%
#'   ungroup()
#'
#' # Rolling beta of ret_1 ~ ret_1_lag1 (window 10)
#' beta_data <- add_slope(sim_data,
#'   y_col = "ret_1", x_col = "ret_1_lag1",
#'   n = 10, new_col = "beta"
#' )
#' head(beta_data)
#'
#' # ---------------------------
#' # Example 3: Multiple windows and different output types
#' # ---------------------------
#' # Compute slopes for windows 5, 10, 20, output as data.frame
#' result_df <- add_slope(sim_data,
#'   y_col = "close", n = c(5, 10, 20),
#'   output = "data.frame"
#' )
#' class(result_df)
#' head(result_df)
#'
#' # ---------------------------
#' # Example 4: Use with real A‑share data (requires network)
#' # ---------------------------
#' \dontrun{
#' library(FactorCraft)
#' dat <- get_data(
#'   stock_list = c("000001", "000002"),
#'   start = "2023-01-01",
#'   end = "2024-12-31"
#' )
#' # Rolling slope of close price (trend)
#' dat <- add_slope(dat, y_col = "close", n = c(5, 10, 20))
#' # Rolling slope of daily return on lagged return (mean reversion check)
#' dat <- add_return(dat, n = 1)
#' dat <- add_slope(dat, y_col = "ret_1", x_col = "ret_1_lag1", n = 10)
#' }
#'
#' # ---------------------------
#' # Example 5: Handling missing values
#' # ---------------------------
#' sim_data_na <- sim_data
#' sim_data_na$close[c(1:3, 100:102)] <- NA # introduce NAs
#' result_na <- add_slope(sim_data_na, y_col = "close", n = 5)
#' # Windows with fewer than 2 valid observations will have NA slope
#' sum(is.na(result_na$slope_5))
#'
#' # ---------------------------
#' # Example 6: Append = FALSE for factor matrix only
#' # ---------------------------
#' factor_matrix <- add_slope(sim_data,
#'   y_col = "close", n = c(5, 20),
#'   append = FALSE
#' )
#' # Contains only date, code, name, and slope columns
#' print(colnames(factor_matrix))
#'
#' # ---------------------------
#' # Example 7: Single window length
#' # ---------------------------
#' slope_10 <- add_slope(sim_data, y_col = "close", n = 10)
#' head(slope_10)
add_slope <- function(data,
                      y_col = "close",
                      x_col = NULL,
                      new_col = "slope",
                      n = c(5, 10, 20),
                      na.pad = TRUE,
                      append = TRUE,
                      output = c("tibble", "data.frame")) {
  # Match arguments
  output <- match.arg(output)
  n <- as.integer(n)
  use_index <- is.null(x_col)

  # Ensure data is sorted by date within each stock
  data_sorted <- data %>%
    dplyr::group_by(code) %>%
    dplyr::arrange(date, .by_group = TRUE) %>%
    dplyr::ungroup()

  # Result container
  res <- data_sorted

  # Compute rolling slope for each window length
  for (period in n) {
    colname <- paste0(new_col, "_", period)

    res <- res %>%
      dplyr::group_by(code) %>%
      dplyr::mutate(
        !!colname := {
          y_vec <- .data[[y_col]]

          if (use_index) {
            # Use global observation index as regressor.
            # The slope from regressing y on global index is identical to
            # using window‑local indices 1:period because translation of x
            # does not change the slope.
            idx_all <- seq_along(y_vec)
            x_mat <- as.matrix(idx_all)
            # roll_lm automatically adds an intercept (default intercept = TRUE)
            # Note: 'complete_obs' argument is no longer supported in current roll package.
            fit <- roll::roll_lm(
              x = x_mat, y = y_vec, width = period,
              min_obs = 2L
            )
            # Coefficients: column 1 = intercept, column 2 = slope
            slope_vec <- fit$coefficients[, 2]
            slope_vec
          } else {
            # Use user‑provided x_col as regressor
            x_vec <- .data[[x_col]]
            x_mat <- as.matrix(x_vec)
            fit <- roll::roll_lm(
              x = x_mat, y = y_vec, width = period,
              min_obs = 2L
            )
            slope_vec <- fit$coefficients[, 2]
            slope_vec
          }
        }
      ) %>%
      dplyr::ungroup()
  }

  # Optionally keep only factor columns
  if (!append) {
    keep_cols <- c("date", "code", "name", paste0(new_col, "_", n))
    res <- res %>% dplyr::select(dplyr::all_of(keep_cols))
  }

  # Convert output class if requested
  if (output == "data.frame") {
    res <- as.data.frame(res, stringsAsFactors = FALSE)
  } else {
    res <- tibble::as_tibble(res)
  }

  return(res)
}
