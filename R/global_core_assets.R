#' Global Core Assets Daily Data
#'
#' Daily price data for 3 major ETFs: CSI300, Nasdaq100, and Gold.
#'
#' @format A data frame with ~1450 rows and 9 variables:
#' \describe{
#'   \item{date}{Trading date}
#'   \item{code}{ETF code with exchange suffix}
#'   \item{name}{Short ETF name}
#'   \item{open}{Opening price}
#'   \item{high}{Highest price}
#'   \item{low}{Lowest price}
#'   \item{close}{Closing price}
#'   \item{adjusted}{Adjusted closing price}   # <-- 加这一行
#'   \item{volume}{Trading volume}
#' }
#' @source Yahoo Finance
#' @usage data(global_core_assets)
#' @examples
#' data(global_core_assets)
#' head(global_core_assets)
"global_core_assets"
