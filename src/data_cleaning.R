#' Data Cleaning Functions for FIM
#'
#' This module contains functions that combine data from various FIM sources to 
#' construct the input series that are used in the model itself. There are 33 (?)
#' functions, each producing a data series, which is a tibble containing a date
#' column and a data_series column.



#' Create Federal Purchases Data Series
#'
#' This function combines federal purchases data from national accounts and forecast sources 
#' into a single data series, filling any gaps with placeholder zeroes extending to 2034 Q3.
#'
#' @param national_accounts A tibble containing national accounts data with a `date` column and a `gf` column for federal purchases.
#' @param forecast A tibble containing forecast data with a `date` column and a `federal_purchases` column.
#' @param placeholder_nas A tibble of appropriate length populated by NAs, extending the date range to 2034 Q3.
#'
#' @return A tibble containing the combined federal purchases data series, with NAs and missing entries replaced by 0s.
#' @export
#'
#' @examples
#' # Assuming `national_accounts`, `forecast`, and `placeholder_nas` are pre-defined tibbles:
#' federal_purchases <- create_federal_purchases(national_accounts, forecast, placeholder_nas)
create_federal_purchases <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, federal_purchases) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = federal_purchases)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # `gf` is the Haver code for federal purchases
    select(date, gf) %>%  
    # Rename data to generic `data_series` for easier merging
    rename(data_series = gf) 
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}



#' Create Consumption Grants Data Series
#'
#' This function combines consumption grants data from national accounts, forecast sources, 
#' and historical overrides into a single data series, filling any gaps with placeholder zeroes extending to 2034 Q3.
#'
#' @param national_accounts A tibble containing national accounts data with a `date` column and `gfeg` and `gfeghdx` columns for gross consumption grants and Medicaid grants respectively.
#' @param forecast A tibble containing forecast data with a `date` column and a `consumption_grants` column.
#' @param historical_overrides A tibble containing historical override data with a `date` column and a `consumption_grants_override` column.
#' @param placeholder_nas A tibble of appropriate length populated by NAs, extending the date range to 2034 Q3.
#'
#' @return A tibble containing the combined consumption grants data series, with NAs and missing entries replaced by 0s.
#' @export
#'
#' @examples
#' # Assuming `national_accounts`, `forecast`, `historical_overrides`, and `placeholder_nas` are pre-defined tibbles:
#' consumption_grants <- create_consumption_grants(national_accounts, forecast, historical_overrides, placeholder_nas)

create_consumption_grants <- function(
    national_accounts, 
    forecast, 
    historical_overrides, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, consumption_grants) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = consumption_grants)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Subtract medicaid grants (gfeghdx) from gross consumption grants (gfeg)
    # and assign to the name "data_series"
    mutate(data_series = gfeg - gfeghdx) %>%
    # Keep the date and the new data_series column
    select(date, data_series) %>%
    # Override historic entries of national_accounts using historical_overrides data
    mutate_where(
      date >= yearquarter('2020 Q2') & date <= current_quarter,
      data_series = historical_overrides$consumption_grants_override
    )
  
  # Merge the national accounts with the forecast using the commonly named `data_series`
  # and `date` columns. The historic (national accounts) data take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(national_accounts, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(placeholder_nas, by = 'date') %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}

## Create investment grants
# ... code here