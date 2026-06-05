# eClassic:Efficient Classic and Traditional Factors Toolkit for Quantitative Research in R.


A lightweight, professional R package for computing classic quantitative factors.  
Designed for ETF & stock backtesting with a clean, consistent API.

---

##  Installation

Install from GitHub:

```r
pak::pak("dengyishuo/eClassic")
```

## Quick Start

```r
library(eClassic)

# Load built-in core assets (CSI300 / Nasdaq100 / Gold ETFs)
data(global_core_assets)

# Compute factors
dat <- global_core_assets %>%
  add_mom(n = c(5, 10, 20)) %>%
  add_volatility(n = 20, annualized = TRUE) %>%
  add_ram()

head(dat)
```

## Available Factors

- add_mom() — Momentum
- add_volatility() — Volatility (std / variance / annualized)
- add_ram() — Risk-Adjusted Momentum
- add_beta() — Market Beta
- add_benchmark() — Align to market benchmark
- add_return() — Periodic returns

## Built-in Data

`global_core_assets`

Daily prices for 3 major core ETFs:

- 510300.SS (CSI 300 ETF)
- 513100.SS (Nasdaq 100 ETF)
- 518880.SS (Gold ETF)

## Author

Yishuo Deng

GitHub: dengyishuo
