# Create global_core_assets: Core ETF Dataset for FactorCraft
# This script generates the built-in package data

library(dplyr)
library(tibble)
library(FactorCraft)

# Create ETF info
etf_info <- data.frame(
  category = c("CSI300", "Nasdaq100", "Gold"),
  name = c("CSI300ETF", "NDQ100ETF", "AUETF"),
  code = c("510300.SS", "513100.SS", "518880.SS"),
  stringsAsFactors = FALSE
)

# Download data
dat_mkt <- get_data(
  stock_df = etf_info,
  start_date = "2024-01-01",
  end_date = "2025-12-31"
)

# Rename to final dataset
global_core_assets <- dat_mkt

# Save as package data
usethis::use_data(global_core_assets, overwrite = TRUE, compress = "xz")
