# global.R —— Global Constants & Package Settings
# Only include: constants, default parameters, global options
# DO NOT include functions or core calculation logic

# ------------------------------
# Directory Paths
# ------------------------------
DATA_DIR <- here::here("data")
RAW_DATA_DIR <- file.path(DATA_DIR, "raw")
CLEAN_DATA_DIR <- file.path(DATA_DIR, "clean")
OUTPUT_DIR <- here::here("output")

# ------------------------------
# Default Parameters for Factor Calculation
# ------------------------------
DEFAULT_WINDOW <- 252 # Trading days in one year
MIN_OBS <- 60 # Minimum valid observations required
FEE_RATE <- 0.0003 # Transaction fee rate
SLIPPAGE <- 0.0005 # Slippage cost

# ------------------------------
# Global R Options
# ------------------------------
options(
  stringsAsFactors = FALSE,
  scipen = 999,
  dplyr.summarise.inform = FALSE
)
