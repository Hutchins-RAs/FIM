# data_cleaning.R
#
# This module contains functions that combine data from various FIM sources to 
# construct the input series that are used in the model itself. There are 33 (?)
# functions, one producing each data series, which is a tibble containing a date
# column and a data column. 

# Federal purchases
create_federal_purchases <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Use the BEA data
  national_accounts %>% 
    # Extract the `gf` column, which represents federal purchases
    select(date, gf) %>% 
    # Rename the column for merging
    rename(federal_purchases = gf) %>% 
    # Merge with our forecast, with the historic data taking precedence in the case
    # of any conflicting observations
    coalesce_join(., forecast %>% select(date, federal_purchases), by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
}

## Consumption grants
create_consumption_grants <- function(
    national_accounts, 
    forecast, 
    historical_overrides, 
    placeholder_nas
) {
  # Use the BEA data
  national_accounts %>% 
    # Subtract medicaid grants (gfeghdx) from gross consumption grants (gfeg)
    mutate(consumption_grants = gfeg - gfeghdx) %>%
    # Keep this new consumption_grants column
    select(date, consumption_grants) %>%
    #Overriding historical consumption grants
    mutate_where(
      date >= yearquarter('2020 Q2') & date <= current_quarter,
      consumption_grants = historical_overrides$consumption_grants_override
    ) %>%
    # Merge with our forecast, with the historic data taking precedence in the case
    # of any conflicting observations
    coalesce_join(., forecast, by = "date") %>%
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
}

## Create investment grants
# ... code here