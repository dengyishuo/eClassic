#------------------------------------------------------------------------------
# eClassic R Package - Initialization Script
# Create standard R package structure safely (no new session, no overwrites)
# Author: Deng Yishuo
#------------------------------------------------------------------------------

library(usethis)

# Create core package directories
use_directory("R") # Main R functions
use_directory("tests") # Unit tests
use_directory("data") # Package example data
use_directory("data-raw") # Raw data processing scripts
use_directory("demo") # Demo code and examples

# Initialize test framework (testthat)
use_testthat()

# Create project-level .Rprofile
usethis::edit_r_profile(scope = "project")

# Configure .Rbuildignore (files to exclude when building the package)
use_build_ignore(
  c(
    "^\\.Rprofile$",
    "^\\.gitignore$",
    "^.*\\.Rproj$",
    "^README\\.Rmd$",
    "^docs$",
    "^demo$",
    "dev",
    "eClassic.Rproj",
    ".Rprofile",
    ".gitignore",
    "README.Rmd",
    "demo"
  )
)

# Ignore local .Rprofile in Git
usethis::use_git_ignore(".Rprofile")

# Set up MIT license (CRAN compliant)
# Remove existing LICENSE file created by GitHub
file.remove("LICENSE")
# Create standard LICENSE + LICENSE.md
use_mit_license(copyright_holder = "Deng Yishuo")

#------------------------------------------------------------------------------
# End of package initialization
#------------------------------------------------------------------------------


usethis::use_r("utils")
usethis::use_r("eClassic")
usethis::use_r("global")



etf_info <- data.frame(
  category = c("CSI300", "Nasdaq100", "Gold"),
  name = c("CSI300ETF", "NDQ100ETF", "AUETF"),
  code = c("510300.SS", "513100.SS", "518880.ss"),
  full_name = c(
    "Huatai-PineBridge CSI 300 ETF",
    "Guotai NASDAQ-100 ETF",
    "Huaan Gold ETF"
  ),
  exchange = c("SSE", "SSE", "SSE"),
  stringsAsFactors = FALSE
)

library(FactorCraft)

dat_mkt <- get_data(stock_df = etf_info, start_date = "2024-01-01", end_date = "2025-12-31")

dat_mom_discrete <- add_mom(dat_mkt, n = 5, type = "discrete")



# 1. 创建 data-raw 文件夹（标准做法）
usethis::use_data_raw()

# 2. 创建数据生成脚本：creat_global_core_assets.R
usethis::use_data_raw(name = "creat_global_core_assets")



usethis::use_r("global_core_assets")
