# memo.R
# 
# This script creates part of a helper Excel file to be published on the FIM 
# website. It contains extra information on the breakdown of the effects of the 
# OBBBA and tariffs on the various components of the FIM

# NOTE: Run this after running the fiscal_impact_BETA script 

# PART 1: Set variables ------------------

Sys.setenv(TZ = 'UTC') # Set the default time zone to UTC (Coordinated Universal Time)

# Load packages
packages <- c(
  "tidyverse", "tsibble", "lubridate", "glue", 
  "TimTeaFan/dplyover", "zoo", "TTR", "fs", "gt", 
  "openxlsx", "snakecase", "rlang", "BrookingsInstitution/ggbrookings"
)
librarian::shelf(packages)

# Load all functions in package
devtools::load_all() 

options(digits = 4) # Limit number of digits
options(scipen = 20)# Turn off scientific notation under 20 digits 

# Set the value of 'month_year' to the current month and year (in the format "mm-yyyy")
last_month_year <- glue('{format.Date(today() %m-% months(1), "%m")}-{year(today() %m-% months(1))}')
month_year <- glue('{format.Date(today() - 7, "%m")}-{year(today())}')
print(month_year)

# Calculate the current date minus 7 days
current_date <- today() - dweeks(1)
# Calculate the previous month date, handling wraparound (i.e. previous
# month to January ("01") is December ("12"), not month "0")
last_month_date <- current_date %m-% months(1)
# Extract and format the month as a two-digit string
last_month_2digit <- sprintf("%02d", month(last_month_date))
# Extract the year from the last_month_date
last_year <- year(last_month_date)
# Create last_month_year string for file naming
last_month_year <- glue('{last_month_2digit}-{last_year}')

# Create folder for current update in the results directory
dir_create(glue('memo/{month_year}')) 


# PART 2: Create variables ------------------

purchases_contribution <- data.frame(
  date = date_test$date, 
  data_series = federal_purchases_contribution + 
    state_purchases_contribution)

supply_side_contribution <- data.frame(
  date = date_test$date, 
  data_series = supply_side_ira_contribution + 
  federal_student_loans_contribution) 

# Combine all the contributions into a data frame
memo_df <- data.frame(
  date,
  purchases_contribution, 
  federal_purchases_contribution,
  state_purchases_contribution,
  taxes_contribution, 
  transfers_contribution,
  uncertainty_test,
  fiscal_impact_measure
) %>%
  as_tsibble(index = date) %>%
  filter_index(as.character(current_quarter - 1) ~ as.character(current_quarter + 8))

# Rename for Excel file
# colnames(memo_df) <- ("Quarter", "Purchases", "Federal")

# Export



