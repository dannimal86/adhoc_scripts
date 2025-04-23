# Asian Equity Index Futures Calendar Spread Calculator

A robust R script for calculating fair values of Asian equity index futures calendar spreads using Bloomberg data.

## Features

- Dynamic contract pair generation based on current date
- Support for multiple Asian indices:
  - Hang Seng Index (HSI)
  - Hang Seng China Enterprise Index (HSCEI)
  - FTSE China A50 (XIN9I)
  - FTSE Taiwan (TWSE)
  - TOPIX (TPX)
  - Nikkei 225 (NKY)
  - MSCI Singapore (SIMSCI)
  - NIFTY (SGX GIFT)

- Comprehensive spread calculations including:
  - Regular dividend yields (adjusted for withholding taxes)
  - Special dividends
  - Index rebalancing events
  - Cost of carry
  - Roll yield

## Requirements

- R
- Bloomberg Terminal with Rblpapi
- Required R packages:
  - Rblpapi
  - dplyr
  - lubridate

## Installation

1. Clone this repository
2. Install required R packages:
```R
install.packages(c("Rblpapi", "dplyr", "lubridate"))
```
3. Ensure you have an active Bloomberg terminal connection

## Usage

```R
# Source the script
source("futures_spread_calculator.R")

# Generate contract pairs based on current date
contract_pairs <- generate_contract_pairs()

# Calculate spreads
spread_results <- calculate_multiple_spreads(contract_pairs)

# Save results to timestamped file
results_file <- save_spread_results(spread_results)
```

## Output

Results are saved to a timestamped text file in the current working directory:
- Filename format: `spread_results_YYYYMMDD_HHMMSS.txt`
- Includes run information and detailed spread calculations

## License

MIT License
