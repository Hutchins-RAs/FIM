
# memo_table.R
#
# This script creates two tables for the FIM Memo. It contains vectors 
# for various FIM inputs. Some of these are taken from the fiscal_impact_BETA.R
# script. Others are calculated and merged here. 

################################################################################
# Create functions 
################################################################################

import_taxes <- function(placeholder_nas) {
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
      data_series = as.numeric(personal_income)
    ) %>%
    select(date, data_series) %>%
    filter(!is.na(data_series)) %>%
    coalesce_join(placeholder_nas, by = 'date') %>%
    mutate(across(everything(), ~ replace_na(., 0)))
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
      date = paste(year, quarter),
      tariff_uncertainty_total = as.numeric(tariff_uncertainty)
    ) %>%
    select(date, tariff_uncertainty_total) %>%
    filter(!is.na(tariff_uncertainty_total))
}

import_supply_side <- function() {
  readxl::read_xlsx('data/forecast.xlsx', sheet = 'Uncertainty') %>%
    slice(c(5, 6, 8)) %>%
    select(-c(1:2)) %>%
    t() %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("year", "quarter", "supply_side")) %>%
    fill(year) %>%
    filter(!is.na(quarter)) %>%
    mutate(
      date = paste(year, quarter),
      supply_side_total = as.numeric(supply_side)
    ) %>%
    select(date, supply_side_total) %>%
    filter(!is.na(supply_side_total))
}

##construction numerbs
import_construction <- function() {
  readxl::read_xlsx('data/forecast.xlsx', sheet = 'Federal and State Purchases') %>%
    slice(c(65, 69)) %>%
    select(-c(1:2)) %>%
    t() %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("date", "construction")) %>%
    filter(!is.na(date)) %>%
    mutate(
      date = as.character(date),
      construction_totals = as.numeric(construction),
    ) %>%
    select(date, construction_totals) %>%
    filter(!is.na(construction_totals))
}

#State and Local Employment numbers
import_sl_employment <- function() {
  readxl::read_xlsx('data/forecast.xlsx', sheet = 'Federal and State Purchases') %>%
    slice(c(65, 68)) %>%
    select(-c(1:2)) %>%
    t() %>%
    as_tibble(.name_repair = "minimal") %>%
    setNames(c("date", "sl_employment")) %>%
    filter(!is.na(date)) %>%
    mutate(
      date = as.character(date),
      sl_employment_totals = as.numeric(sl_employment),
    ) %>%
    select(date, sl_employment_totals) %>%
    filter(!is.na(sl_employment_totals))
}



################################################################################
# Create variables
################################################################################

federal_purchases_test <- create_federal_purchases(
  national_accounts, 
  forecast, 
  create_placeholder_nas()
)

state_purchases_test <- create_state_purchases(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

taxes_test <- import_taxes(create_placeholder_nas())

transfers_test <- data.frame(date = date_test$date, data_series = federal_social_benefits + state_social_benefits +
                               rebate_checks + rebate_checks_arp + 
                               federal_ui + state_ui + 
                               federal_subsidies + federal_aid_to_small_businesses_arp + 
                               federal_other_direct_aid_arp + federal_other_vulnerable_arp  + 
                               federal_student_loans + state_subsidies +
                               federal_health_outlays + state_health_outlays)

health_test <- data.frame(date = date_test$date, data_series = federal_health_outlays + state_health_outlays)

non_health_grants_test <- data.frame(date = date_test$date, data_series = consumption_grants + investment_grants)

tariff_uncertainty_test <- import_tariff_uncertainty()

supply_side_test <- import_supply_side()

ira_test <- data.frame(date = date_test$date, data_series = supply_side_ira)

student_loans_test <- data.frame(date = date_test$date, data_series = federal_student_loans)

construction_test <- import_construction()

sl_employment_test <- import_sl_employment()

federal_purchases_deflator_test <- data.frame(date = date_test$date, data_series = federal_purchases_deflator_growth)

consumption_grants_deflator_test <- data.frame(date = date_test$date, data_series = consumption_grants_deflator_growth)

investment_grants_deflator_test <- data.frame(date = date_test$date, data_series = investment_grants_deflator_growth)

state_purchases_deflator_test <- data.frame(date = date_test$date, data_series = state_purchases_deflator_growth)

consumption_deflator_test <- data.frame(date = date_test$date, data_series = consumption_deflator_growth)


########################################
#creating the compiled dataframe
########################################


memo_df <- data.frame(
  federal_purchases_test,
  state_purchases_test,
  taxes_test,
  transfers_test,
  health_test,
  non_health_grants_test,
  tariff_uncertainty_test,
  supply_side_test,
  ira_test,
  student_loans_test,
  construction_test,
  sl_employment_test,
  federal_purchases_deflator_test,
  consumption_grants_deflator_test,
  investment_grants_deflator_test,
  state_purchases_deflator_test,
  consumption_deflator_test
) %>%
  as_tsibble(index = date) %>%
  filter_index(as.character(current_quarter - 8) ~ as.character(current_quarter + 8))


