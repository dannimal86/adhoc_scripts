# Asian Equity Index Futures Calendar Spread Fair Value Calculator
# This script calculates the fair value of Asian equity index futures calendar spreads using Bloomberg data
#
# The fair value calculation considers:
# 1. Regular dividend yields (adjusted for withholding taxes)
# 2. Special dividends during the holding period
# 3. Index rebalancing events
# 4. Market-specific tax rates and regulations
# 5. Cost of carry based on risk-free rate minus total dividend yield

library(Rblpapi)  # Bloomberg API interface
library(dplyr)    # Data manipulation
library(lubridate)  # Date handling

#' Validate Bloomberg connection
#' @return TRUE if connection is valid, throws error otherwise
validate_bloomberg_connection <- function() {
  if (!blpConnect()) {
    stop("Failed to connect to Bloomberg terminal")
  }
  # Test connection with a simple query
  tryCatch({
    bdp("SPX Index", "PX_LAST")
    return(TRUE)
  }, error = function(e) {
    stop("Bloomberg connection error: ", e$message)
  })
}

#' Validate date inputs
#' @param date Date to validate
#' @param min_date Minimum allowed date (default: current date)
#' @param max_date Maximum allowed date (default: 2 years from now)
#' @return Validated date
validate_date <- function(date, min_date = Sys.Date(), max_date = Sys.Date() + years(2)) {
  date <- as.Date(date)
  if (is.na(date)) stop("Invalid date format")
  if (date < min_date) stop("Date is before minimum allowed date")
  if (date > max_date) stop("Date is beyond maximum allowed date")
  return(date)
}

#' Validate index code
#' @param index_code Index code to validate
#' @return Validated index code
validate_index_code <- function(index_code) {
  valid_indices <- c("HSI", "HSCEI", "XIN9I", "TWSE", "TPX", "NKY", "SIMSCI", "NIFTY")
  if (!index_code %in% valid_indices) {
    stop(sprintf("Invalid index code: %s. Valid codes are: %s", 
                index_code, paste(valid_indices, collapse = ", ")))
  }
  return(index_code)
}

#' Validate contract code format
#' @param contract Contract code to validate
#' @return TRUE if valid, throws error otherwise
validate_contract_code <- function(contract) {
  valid_months <- c("F", "G", "H", "J", "K", "M", "N", "Q", "U", "V", "X", "Z")
  
  # Check basic format (e.g., "HIH4")
  if (!grepl("^[A-Z]{2,3}[A-Z][0-9]$", contract)) {
    stop(sprintf("Invalid contract code format: %s. Expected format: PREFIX + MONTH + YEAR (e.g., HIH4)", contract))
  }
  
  # Extract and validate month code
  month_code <- substr(contract, nchar(contract)-1, nchar(contract)-1)
  if (!month_code %in% valid_months) {
    stop(sprintf("Invalid month code in %s. Valid month codes are: %s", 
                contract, paste(valid_months, collapse = ", ")))
  }
  
  return(TRUE)
}

#' Check if contract is listed on Bloomberg
#' @param contract Contract code to check
#' @return TRUE if listed, FALSE if not listed or error
is_contract_listed <- function(contract) {
  tryCatch({
    result <- bdp(paste0(contract, " Index"), "FUTURES_CHAIN_STATUS")
    return(!is.na(result) && result == "Listed")
  }, error = function(e) {
    warning(sprintf("Contract listing check failed for %s: %s", contract, e$message))
    return(FALSE)
  })
}

#' Safe Bloomberg data retrieval
#' @param securities Vector of securities
#' @param fields Vector of fields
#' @return Data frame with results
safe_bdp <- function(securities, fields) {
  validate_bloomberg_connection()
  
  result <- bdp(securities, fields)
  if (is.null(result) || nrow(result) == 0) {
    stop("No data returned from Bloomberg for securities: ", 
         paste(securities, collapse = ", "))
  }
  
  # Check for missing values in critical fields
  missing_data <- apply(result, 2, function(x) any(is.na(x)))
  if (any(missing_data)) {
    warning("Missing data in fields: ", 
            paste(names(missing_data)[missing_data], collapse = ", "))
  }
  
  return(result)
}

#' Index-specific configuration
#' Contains market-specific parameters for each supported index:
#' - tax_rate: Applicable dividend withholding tax rate
#' - rebalance_months: Months when index rebalancing occurs
#' - dividend_field: Bloomberg field for regular dividend yield
#' - special_div_field: Bloomberg field for special dividend dates
#' - contract_frequency: Monthly or Quarterly contracts
#' @return List of index-specific parameters
get_index_config <- function(index_code) {
  # Configuration for each supported index
  # Tax rates and rebalancing schedules are based on current market rules
  # Update these when regulations change
  configs <- list(
    "HSI" = list(  # Hang Seng Index
      tax_rate = 0,  # No withholding tax for Hong Kong stocks
      rebalance_months = c(2, 5, 8, 11),  # Quarterly rebalancing
      dividend_field = "EQY_DVD_YLD_EST",  # Bloomberg field for dividend yield
      special_div_field = "DVD_EX_DT",  # Field for special dividend ex-dates
      contract_frequency = "monthly"  # Monthly contracts available
    ),
    "HSCEI" = list(  # Hang Seng China Enterprise Index
      tax_rate = 0.10,  # 10% withholding tax on mainland companies
      rebalance_months = c(2, 5, 8, 11),  # Same schedule as HSI
      dividend_field = "EQY_DVD_YLD_EST",
      special_div_field = "DVD_EX_DT",
      contract_frequency = "monthly"
    ),
    "XIN9I" = list(  # FTSE China A50
      tax_rate = 0.10,
      rebalance_months = c(3, 6, 9, 12),  # Quarterly review
      dividend_field = "EQY_DVD_YLD_EST",
      special_div_field = "DVD_EX_DT",
      contract_frequency = "monthly"
    ),
    "TWSE" = list(  # FTSE Taiwan
      tax_rate = 0.21,  # Taiwan dividend withholding tax
      rebalance_months = c(3, 6, 9, 12),
      dividend_field = "EQY_DVD_YLD_EST",
      special_div_field = "DVD_EX_DT",
      contract_frequency = "quarterly"
    ),
    "TPX" = list(  # TOPIX
      tax_rate = 0.15,  # Japanese withholding tax rate
      rebalance_months = c(3, 6, 9, 12),  # Quarterly rebalancing
      dividend_field = "EQY_DVD_YLD_EST",
      special_div_field = "DVD_EX_DT",
      contract_frequency = "quarterly"  # Mar,Jun,Sep,Dec
    ),
    "NKY" = list(  # Nikkei 225
      tax_rate = 0.15,
      rebalance_months = c(3, 9),  # Semi-annual review
      dividend_field = "EQY_DVD_YLD_EST",
      special_div_field = "DVD_EX_DT",
      contract_frequency = "quarterly"  # Mar,Jun,Sep,Dec
    ),
    "SIMSCI" = list(  # MSCI Singapore
      tax_rate = 0,  # No withholding tax on Singapore dividends
      rebalance_months = c(2, 5, 8, 11),  # Quarterly rebalancing
      dividend_field = "EQY_DVD_YLD_EST",
      special_div_field = "DVD_EX_DT",
      contract_frequency = "quarterly"
    ),
    "GIFT" = list(  # SGX GIFT Nifty
      tax_rate = 0,  # Tax handled at source
      rebalance_months = c(3, 6, 9, 12),  # Quarterly rebalancing
      dividend_field = "EQY_DVD_YLD_EST",
      special_div_field = "DVD_EX_DT",
      contract_frequency = "quarterly"
    )
  )
  
  if (!index_code %in% names(configs)) {
    stop(paste("Unsupported index:", index_code))
  }
  
  return(configs[[index_code]])
}

#' Extract index code from Bloomberg ticker
#' Maps futures contract codes to their underlying index codes
#' Examples:
#' - "HIH4" -> "HSI" (Hang Seng Index)
#' - "HCH4" -> "HSCEI" (Hang Seng China Enterprise Index)
#' @param contract Bloomberg contract code
#' @return Index code
get_index_code <- function(contract) {
  # Mapping between Bloomberg futures tickers and index codes
  # Key = pattern to match in futures code
  # Value = corresponding index identifier
  index_mapping <- c(
    "^HI" = "HSI",      # Hang Seng Index Future
    "^HC" = "HSCEI",    # HSCEI Future
    "^XU" = "XIN9I",    # FTSE China A50 Future
    "^TWN" = "TWSE",    # FTSE Taiwan Future
    "^TP" = "TPX",      # TOPIX Future
    "^NK" = "NKY",      # Nikkei 225 Future
    "^QZ" = "SIMSCI",   # MSCI Singapore Future
    "^JGS" = "GIFT"     # SGX GIFT Nifty Future
  )
  
  # Try to match the contract code against known patterns
  for (pattern in names(index_mapping)) {
    if (grepl(pattern, contract, ignore.case = TRUE)) {
      return(index_mapping[pattern])
    }
  }
  stop(paste("Unknown index for contract:", contract))
}

#' Check for special dividends between dates
#' Queries Bloomberg for any special dividend events occurring between
#' the front and back contract dates
#' @param index_code Index identifier
#' @param start_date Start date
#' @param end_date End date
#' @return Total special dividends amount (adjusted for index divisor)
get_special_dividends <- function(index_code, start_date, end_date) {
  config <- get_index_config(index_code)
  
  # Query Bloomberg for special dividend information
  # Returns both ex-dates and amounts for the period
  special_div_data <- bdp(paste0(index_code, " Index"),
                         c(config$special_div_field, "DVD_AMT"),
                         start_date = start_date,
                         end_date = end_date)
  
  # Return 0 if no special dividends found
  if (nrow(special_div_data) == 0) return(0)
  
  # Sum up all special dividends in the period
  total_special_div <- sum(special_div_data$DVD_AMT)
  return(total_special_div)
}

#' Check if rebalancing occurs between dates
#' Determines if any scheduled index rebalancing events fall
#' between the front and back contract dates
#' @param index_code Index identifier
#' @param start_date Start date
#' @param end_date End date
#' @return Boolean indicating if rebalancing occurs
has_rebalancing <- function(index_code, start_date, end_date) {
  config <- get_index_config(index_code)
  
  # Generate sequence of all months between the dates
  months_between <- seq(start_date, end_date, by = "month")
  months_between <- month(months_between)
  
  # Check if any rebalancing months fall in this period
  return(any(months_between %in% config$rebalance_months))
}

#' Calculate equity index futures calendar spread fair value
#' @param front_contract Bloomberg code for the front contract
#' @param back_contract Bloomberg code for the back contract
#' @param fields Vector of Bloomberg fields to retrieve
#' @param risk_free_rate Annual risk-free rate as decimal
#' @return Data frame with spread calculations and analysis
calculate_spread_fair_value <- function(front_contract, back_contract, 
                                      fields = c("PX_LAST", "FUT_CUR_GEN_ROLL_YIELD",
                                               "FUT_CONT_DT", "DAYS_TO_EXPIRY"),
                                      risk_free_rate = 0.05) {
  
  # Input validation
  validate_contract_code(front_contract)
  validate_contract_code(back_contract)
  if (!is.numeric(risk_free_rate) || risk_free_rate < 0) {
    stop("Invalid risk-free rate. Must be a non-negative number.")
  }
  
  # Get index code and validate
  index_code <- get_index_code(front_contract)
  validate_index_code(index_code)
  
  # Get configuration
  config <- get_index_config(index_code)
  
  # Prepare Bloomberg query fields
  fields <- unique(c(fields, config$dividend_field))
  
  # Fetch market data safely
  contracts <- c(front_contract, back_contract)
  data <- safe_bdp(paste0(contracts, " Index"), fields)
  
  # Validate dates
  front_date <- as.Date(data[front_contract, "FUT_CONT_DT"])
  back_date <- as.Date(data[back_contract, "FUT_CONT_DT"])
  validate_date(front_date)
  validate_date(back_date)
  if (front_date >= back_date) {
    stop("Front contract date must be before back contract date")
  }
  
  # Calculate holding period
  holding_period_years <- as.numeric(back_date - front_date) / 365
  
  # Process dividend information with error checking
  div_yield <- tryCatch({
    as.numeric(data[front_contract, config$dividend_field]) / 100
  }, error = function(e) {
    stop("Failed to process dividend yield: ", e$message)
  })
  
  if (is.na(div_yield) || div_yield < 0) {
    warning("Invalid dividend yield. Using 0.")
    div_yield <- 0
  }
  
  # Calculate after-tax dividend yield
  after_tax_div_yield <- div_yield * (1 - config$tax_rate)
  
  # Get special dividends with error handling
  special_divs <- tryCatch({
    get_special_dividends(index_code, front_date, back_date)
  }, error = function(e) {
    warning("Failed to get special dividends: ", e$message)
    return(0)
  })
  
  # Rest of calculations with error checking
  spot_price <- as.numeric(data[front_contract, "PX_LAST"])
  if (is.na(spot_price) || spot_price <= 0) {
    stop("Invalid spot price for front contract")
  }
  
  special_div_yield <- (special_divs / spot_price) * (1 - config$tax_rate)
  total_div_yield <- after_tax_div_yield + special_div_yield
  
  # Check for rebalancing
  has_rebal <- has_rebalancing(index_code, front_date, back_date)
  
  # Calculate carry and fair value
  carry_rate <- risk_free_rate - total_div_yield
  total_carry_cost <- spot_price * carry_rate * holding_period_years
  theoretical_fair_value <- spot_price + total_carry_cost
  
  # Calculate actual spread
  back_price <- as.numeric(data[back_contract, "PX_LAST"])
  if (is.na(back_price) || back_price <= 0) {
    stop("Invalid price for back contract")
  }
  
  actual_spread <- back_price - spot_price
  
  # Compile results with input validation
  result <- data.frame(
    index = index_code,
    front_contract = front_contract,
    back_contract = back_contract,
    front_price = spot_price,
    back_price = back_price,
    actual_spread = actual_spread,
    theoretical_fair_value = theoretical_fair_value,
    total_carry_cost = total_carry_cost,
    risk_free_rate = risk_free_rate,
    gross_dividend_yield = div_yield,
    after_tax_dividend_yield = after_tax_div_yield,
    special_dividend_yield = special_div_yield,
    total_dividend_yield = total_div_yield,
    net_carry_rate = carry_rate,
    roll_yield = mean(c(data[front_contract, "FUT_CUR_GEN_ROLL_YIELD"],
                       data[back_contract, "FUT_CUR_GEN_ROLL_YIELD"]), na.rm = TRUE),
    market_condition = ifelse(actual_spread > 0, "Contango", "Backwardation"),
    mispricing = actual_spread - total_carry_cost,
    holding_period_years = holding_period_years,
    has_rebalancing = has_rebal,
    withholding_tax_rate = config$tax_rate,
    timestamp = Sys.time()
  )
  
  return(result)
}

#' Calculate spreads for multiple pairs of contracts
#' Wrapper function to process multiple contract pairs in batch
#' @param contract_pairs List of contract pairs, each containing front and back month codes
#' @return Data frame with spread calculations for all pairs
calculate_multiple_spreads <- function(contract_pairs) {
  results <- do.call(rbind, lapply(contract_pairs, function(pair) {
    calculate_spread_fair_value(pair[1], pair[2])
  }))
  return(results)
}

#' Generate Bloomberg futures contract code
#' @param index_code Index identifier (e.g., "HSI", "HSCEI")
#' @param date Date for the contract
#' @return Bloomberg contract code
generate_contract_code <- function(index_code, date) {
  # Month codes in Bloomberg format
  month_codes <- c(
    "1" = "F", "2" = "G", "3" = "H", "4" = "J", "5" = "K", "6" = "M",
    "7" = "N", "8" = "Q", "9" = "U", "10" = "V", "11" = "X", "12" = "Z"
  )
  
  # Get month code
  month_code <- month_codes[as.character(month(date))]
  
  # Get year code (last digit)
  year_code <- substr(year(date), 4, 4)
  
  # Get the appropriate prefix based on index
  prefix <- switch(index_code,
    "HSI" = "HI",
    "HSCEI" = "HC",
    "XIN9I" = "XU",
    "TWSE" = "TWN",
    "TPX" = "TP",
    "NKY" = "NK",
    "SIMSCI" = "QZ",
    "NIFTY" = "JGS",
    stop("Unknown index code")
  )
  
  # Combine to form contract code
  return(paste0(prefix, month_code, year_code))
}

#' Get next valid contract month
#' @param date Starting date
#' @param frequency Contract frequency ("monthly" or "quarterly")
#' @return Date of next valid contract month
get_next_contract_month <- function(date, frequency) {
  current_month <- month(date)
  
  if (frequency == "monthly") {
    # For monthly contracts, simply move to next month
    return(date %m+% months(1))
  } else if (frequency == "quarterly") {
    # For quarterly contracts, find next quarter month (Mar, Jun, Sep, Dec)
    quarter_months <- c(3, 6, 9, 12)
    next_quarter <- quarter_months[which(quarter_months > current_month)[1]]
    
    if (is.na(next_quarter)) {
      # If we're past December, move to March of next year
      return(ymd(paste0(year(date) + 1, "-03-01")))
    } else {
      return(ymd(paste0(year(date), "-", sprintf("%02d", next_quarter), "-01")))
    }
  }
  stop("Invalid frequency")
}

#' Generate dynamic contract pairs based on current date
#' @param indices Vector of index codes to generate pairs for
#' @param base_date Base date for generation (defaults to current date)
#' @return List of contract pairs
generate_contract_pairs <- function(indices = c("HSI", "HSCEI", "XIN9I", "TWSE", "TPX", "NKY", "SIMSCI", "NIFTY"),
                                  base_date = Sys.Date()) {
  # Validate inputs
  base_date <- validate_date(base_date)
  indices <- sapply(indices, validate_index_code)
  
  pairs <- list()
  failed_pairs <- character(0)
  
  for (index in indices) {
    tryCatch({
      # Get index configuration
      config <- get_index_config(index)
      
      # Get front contract month (next available contract from base date)
      front_date <- get_next_contract_month(base_date, config$contract_frequency)
      
      # Get back contract month (next available contract after front)
      back_date <- get_next_contract_month(front_date, config$contract_frequency)
      
      # Generate contract codes
      front_contract <- generate_contract_code(index, front_date)
      back_contract <- generate_contract_code(index, back_date)
      
      # Validate contract codes
      validate_contract_code(front_contract)
      validate_contract_code(back_contract)
      
      # Check if contracts are listed
      if (!is_contract_listed(front_contract)) {
        stop("Front contract not listed: ", front_contract)
      }
      if (!is_contract_listed(back_contract)) {
        stop("Back contract not listed: ", back_contract)
      }
      
      # Add to pairs list
      pairs[[index]] <- c(front_contract, back_contract)
      
    }, error = function(e) {
      failed_pairs <- c(failed_pairs, index)
      warning(sprintf("Failed to generate pair for %s: %s", index, e$message))
    })
  }
  
  if (length(pairs) == 0) {
    stop("Failed to generate any valid contract pairs")
  }
  
  if (length(failed_pairs) > 0) {
    warning("Failed to generate pairs for indices: ", 
            paste(failed_pairs, collapse = ", "))
  }
  
  return(pairs)
}

#' Save spread results to file
#' @param results Data frame with spread calculation results
#' @param prefix Prefix for the output file name
#' @return Path to the saved file
save_spread_results <- function(results, prefix = "spread_results") {
  # Generate timestamp for filename
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  filename <- file.path(getwd(), paste0(prefix, "_", timestamp, ".txt"))
  
  # Format results for output
  # Add header with run information
  header <- paste0(
    "Spread Calculator Results\n",
    "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
    "Working Directory: ", getwd(), "\n",
    "Number of Spreads: ", nrow(results), "\n",
    paste(rep("-", 80), collapse = ""), "\n\n"
  )
  
  # Write results to file
  tryCatch({
    # Write header
    cat(header, file = filename)
    
    # Write results in a clean format
    capture.output(
      print(results, row.names = FALSE, right = FALSE, width = 200),
      file = filename,
      append = TRUE
    )
    
    message("Results saved to: ", filename)
    return(filename)
    
  }, error = function(e) {
    stop("Failed to save results to file: ", e$message)
  })
}

# Example usage with dynamic contract pairs and result saving
if (FALSE) {  # Set to TRUE to run example
  # Generate dynamic contract pairs based on current date
  contract_pairs <- generate_contract_pairs()
  
  # Calculate spreads for all pairs
  spread_results <- calculate_multiple_spreads(contract_pairs)
  
  # Save results to file
  results_file <- save_spread_results(spread_results)
  
  # Display results in console
  print(spread_results)
} 