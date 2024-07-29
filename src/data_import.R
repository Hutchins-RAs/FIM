# data_import.R
#
# This module contains functions that import all data used in the FIM. 
# It includes chunk names  that
# are used in the technical walkthrough in docs/technical_documentation. Thus,
# it's essential to be careful when editing chunk names to avoid causing an error
# in docs/technical_documentation/index.Rmd

# This section is meant to import the required data series line-by-line. Eventually,
# all data imports will be done here.

# CBO projections
import_projections <- function() { # to load to memory, run load("data/projections.rda")
  x <- fim::projections 
  return(x)
}

# BEA National Accounts
import_national_accounts <- function() { # to load to memory, run load("data/national_accounts.rda")
  x <- fim::national_accounts
  return(x)
}

# Forecast spreadsheet
import_forecast <- function() {
  readxl::read_xlsx('data/forecast.xlsx', sheet = 'forecast') %>%
    select(-name) %>%
    pivot_longer(-variable, names_to = 'date') %>%
    pivot_wider(names_from = 'variable', values_from = 'value') %>%
    mutate(date = yearquarter(date)) %>%
    as_tsibble(index = date)
}

# Historical overrides
import_historical_overrides <- function() {
  readxl::read_xlsx('data/forecast.xlsx', sheet = 'historical overrides') %>%
    select(-name) %>%
    pivot_longer(-variable, names_to = 'date') %>%
    pivot_wider(names_from = 'variable', values_from = 'value') %>%
    mutate(date = yearquarter(date))
}

## Create a data frame of appropriate length populated by NAs
create_placeholder_nas <- function(col_name = "placeholder") {
  dates <- yearquarter(seq(ymd("2022-10-01"), ymd("2034-07-01"), by = "quarter"))
  tsibble(date = dates, !!sym(col_name) := NA, index = date)
}
