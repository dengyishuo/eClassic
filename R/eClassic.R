#' eClassic: Efficient Classic Financial Factors Toolkit
#'
#' A fast and lightweight package for computing classic quantitative
#' factors such as size, value, momentum, profitability, quality,
#' volatility, and liquidity.
#'
#' @docType package
#' @author Yishuo Deng <dengyishuo@163.com>
#' @keywords internal
#' @importFrom magrittr %>%
#' @importFrom rlang sym
#' @export
"_PACKAGE"

# Export pipe
magrittr::`%>%`

# Global variables for dplyr/NSE
utils::globalVariables(c(
  "code", "name", "date", "close", "return", "industry", "cap",
  "mom_5", "mom_20", "vol_10", "quantile_group", "ret", "direction",
  ".ret1", ".ret_tmp", "future", "ret_label", "abs_ret", "sym", ".bench_ret"
))
