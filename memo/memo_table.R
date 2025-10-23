# memo_table.R
#
# This script creates combined tables for the FIM Memo by importing and merging
# various fiscal impact measures.

################################################################################
# Import functions 
################################################################################

import_personal_income_taxes <- function(placeholder_nas) {
  readxl::read_xlsx('data/forecast.xlsx', sheet = 'Taxes') %>%
    slice(c(5, 6, 9)) %>%
    select(-c(1:2)) %>%
    t() %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("year", "quarter", "personal_income")) %>%
    fill(year) %>%
    filter(!is.na(quarter)) %>%
    mutate(
      date = yearquarter(paste(year, quarter)),
      personal_income_taxes = as.numeric(personal_income)
    ) %>%
    select(date, personal_income_taxes) %>%
    filter(!is.na(personal_income_taxes))
}

import_tariff_uncertainty <- function() {
  readxl::read_xlsx('data/forecast.xlsx', sheet = 'Uncertainty') %>%
    slice(c(5, 6, 7)) %>%
    select(-c(1:2)) %>%
    t() %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("year", "quarter", "tariff_uncertainty")) %>%
    fill(year) %>%
    filter(!is.na(quarter)) %>%
    mutate(
      date = yearquarter(paste(year, quarter)),
      tariff_uncertainty = as.numeric(tariff_uncertainty)
    ) %>%
    select(date, tariff_uncertainty) %>%
    filter(!is.na(tariff_uncertainty))
}

import_supply_side_obbba <- function() {
  readxl::read_xlsx('data/forecast.xlsx', sheet = 'Uncertainty') %>%
    slice(c(5, 6, 8)) %>%
    select(-c(1:2)) %>%
    t() %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("year", "quarter", "supply_side")) %>%
    fill(year) %>%
    filter(!is.na(quarter)) %>%
    mutate(
      date = yearquarter(paste(year, quarter)),
      supply_side_obbba = as.numeric(supply_side)
    ) %>%
    select(date, supply_side_obbba) %>%
    filter(!is.na(supply_side_obbba))
}

import_sl_construction <- function() {
  readxl::read_xlsx('data/forecast.xlsx', sheet = 'Federal and State Purchases') %>%
    slice(c(65, 69)) %>%
    select(-c(1:2)) %>%
    t() %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("date", "construction")) %>%
    filter(!is.na(date)) %>%
    mutate(
      date = yearquarter(as.character(date)),  # Convert to yearquarter HERE
      sl_construction = as.numeric(construction)
    ) %>%
    select(date, sl_construction) %>%
    filter(!is.na(sl_construction))
}

import_sl_employment <- function() {
  readxl::read_xlsx('data/forecast.xlsx', sheet = 'Federal and State Purchases') %>%
    slice(c(65, 68)) %>%
    select(-c(1:2)) %>%
    t() %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("date", "sl_employment")) %>%
    filter(!is.na(date)) %>%
    mutate(
      date = yearquarter(as.character(date)),  # Convert to yearquarter HERE
      sl_employment = as.numeric(sl_employment)
    ) %>%
    select(date, sl_employment) %>%
    filter(!is.na(sl_employment))
}

################################################################################
# Create individual dataframes with proper column names
################################################################################

federal_purchases <- create_federal_purchases(
  national_accounts, 
  forecast, 
  create_placeholder_nas()
) %>%
  rename(federal_purchases = data_series)

state_purchases <- create_state_purchases(
  national_accounts,
  forecast,
  create_placeholder_nas()
) %>%
  rename(state_purchases = data_series)

personal_income_taxes <- import_personal_income_taxes(create_placeholder_nas())

transfers <- tibble(
  date = date_test$date,
  transfers = federal_social_benefits + state_social_benefits +
    rebate_checks + rebate_checks_arp + 
    federal_ui + state_ui + 
    federal_subsidies + federal_aid_to_small_businesses_arp + 
    federal_other_direct_aid_arp + federal_other_vulnerable_arp + 
    federal_student_loans + state_subsidies +
    federal_health_outlays + state_health_outlays
)

health <- tibble(
  date = date_test$date,
  health = federal_health_outlays + state_health_outlays
)

non_health_grants <- tibble(
  date = date_test$date,
  non_health_grants = consumption_grants + investment_grants
)

tariff_uncertainty <- import_tariff_uncertainty()

supply_side_obbba <- import_supply_side_obbba()

date_uncertainty <- date_test %>%
  filter(date >= yearquarter("2020 Q1") & date <= yearquarter("2027 Q2"))
  
uncertainty <- tibble(
  date = date_uncertainty$date,
  uncertainty = tariff_uncertainty$tariff_uncertainty + supply_side_obbba$supply_side_obbba
)

ira <- tibble(
  date = date_test$date,
  ira = supply_side_ira_test$data_series
)

student_loans <- tibble(
  date = date_test$date,
  student_loans = federal_student_loans
)

sl_construction <- import_sl_construction()

sl_employment <- import_sl_employment()

federal_purchases_deflator <- tibble(
  date = date_test$date,
  federal_purchases_deflator = ( (federal_purchases_deflator_growth + 1)^4 - 1 ) * 100
)

consumption_grants_deflator <- tibble(
  date = date_test$date,
  consumption_grants_deflator = ( (consumption_grants_deflator_growth + 1)^4 - 1 ) * 100
)

investment_grants_deflator <- tibble(
  date = date_test$date,
  investment_grants_deflator = ( (investment_grants_deflator_growth + 1)^4 - 1 ) * 100
)

state_purchases_deflator <- tibble(
  date = date_test$date,
  state_purchases_deflator = ( (state_purchases_deflator_growth + 1)^4 - 1 ) * 100
)

consumption_deflator <- tibble(
  date = date_test$date,
  consumption_deflator = ( (consumption_deflator_growth + 1)^4 - 1 ) * 100
)

################################################################################
# Combine all dataframes
################################################################################

memo_df <- list(
  federal_purchases,
  state_purchases,
  personal_income_taxes,
  transfers,
  health,
  non_health_grants,
  tariff_uncertainty,
  supply_side_obbba,
  uncertainty, 
  ira,
  student_loans,
  sl_construction,
  sl_employment,
  federal_purchases_deflator,
  consumption_grants_deflator,
  investment_grants_deflator,
  state_purchases_deflator,
  consumption_deflator
) %>%
  reduce(full_join, by = "date") %>%
  mutate(date = yearquarter(date)) %>%  # Ensure consistent date type
  arrange(date) %>%                      # Sort by date
  as_tsibble(index = date) %>%
  filter_index(as.character(current_quarter - 8) ~ as.character(current_quarter + 8))

################################################################################
# Calculate quarter over quarter changes
################################################################################

memo_changes_df <- memo_df %>%
  mutate(across(where(is.numeric), ~difference(.), .names = "{.col}"))

################################################################################
# Export
################################################################################

# Create folder to save workbook
dir_create(glue('memo/{month_year}')) 

# Create a workbook
wb <- openxlsx::createWorkbook()

# Add sheets
openxlsx::addWorksheet(wb, "Levels")
openxlsx::addWorksheet(wb, "QoQ Changes")

# Write data to each sheet
openxlsx::writeData(wb, sheet = "Levels", x = memo_df)
openxlsx::writeData(wb, sheet = "QoQ Changes", x = memo_changes_df)

# Identify deflator columns (columns ending with "_deflator")
deflator_cols <- grep("_deflator$", names(memo_df))
non_deflator_cols <- setdiff(2:ncol(memo_df), deflator_cols)  # Exclude date column (1) and deflator columns

# Create styles
style_0_decimals <- openxlsx::createStyle(numFmt = "0")
style_2_decimals <- openxlsx::createStyle(numFmt = "0.00")

# Apply formatting to "Levels" sheet
# Non-deflator columns: 0 decimals
if (length(non_deflator_cols) > 0) {
  openxlsx::addStyle(wb, sheet = "Levels", 
                     style = style_0_decimals,
                     rows = 2:(nrow(memo_df) + 1), 
                     cols = non_deflator_cols,
                     gridExpand = TRUE)
}

# Deflator columns: 2 decimals
if (length(deflator_cols) > 0) {
  openxlsx::addStyle(wb, sheet = "Levels", 
                     style = style_2_decimals,
                     rows = 2:(nrow(memo_df) + 1), 
                     cols = deflator_cols,
                     gridExpand = TRUE)
}

# Apply formatting to "QoQ Changes" sheet
# Non-deflator columns: 0 decimals
if (length(non_deflator_cols) > 0) {
  openxlsx::addStyle(wb, sheet = "QoQ Changes", 
                     style = style_0_decimals,
                     rows = 2:(nrow(memo_changes_df) + 1), 
                     cols = non_deflator_cols,
                     gridExpand = TRUE)
}

# Deflator columns: 2 decimals
if (length(deflator_cols) > 0) {
  openxlsx::addStyle(wb, sheet = "QoQ Changes", 
                     style = style_2_decimals,
                     rows = 2:(nrow(memo_changes_df) + 1), 
                     cols = deflator_cols,
                     gridExpand = TRUE)
}

# Save the workbook
openxlsx::saveWorkbook(wb, 
                       file = glue('memo/{month_year}/input-changes-{month_year}.xlsx'), 
                       overwrite = TRUE)