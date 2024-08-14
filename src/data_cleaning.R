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


#' Create Investment Grants Data Series
#'
#' This function combines investment grants data from national accounts, forecast sources, 
#' and historical overrides into a single data series, filling any gaps with placeholder zeroes extending to 2034 Q3.
#'
#' @param national_accounts A tibble containing national accounts data with a `date` column and a `gfeigx` column for investment grants.
#' @param forecast A tibble containing forecast data with a `date` column and an `investment_grants` column.
#' @param historical_overrides A tibble containing historical override data with a `date` column and an `investment_grants_override` column.
#' @param placeholder_nas A tibble of appropriate length populated by NAs, extending the date range to 2034 Q3.
#'
#' @return A tibble containing the combined investment grants data series, with NAs and missing entries replaced by 0s.
#' @export
#'
#' @examples
#' # Assuming `national_accounts`, `forecast`, `historical_overrides`, and `placeholder_nas` are pre-defined tibbles:
#' investment_grants <- create_investment_grants(national_accounts, forecast, historical_overrides, placeholder_nas)
create_investment_grants <- function(
    national_accounts, 
    forecast, 
    historical_overrides, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, investment_grants) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = investment_grants)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # `gf` is the Haver code for federal purchases
    select(date, gfeigx) %>%  
    # Rename data to generic `data_series` for easier merging
    rename(data_series = gfeigx) %>%
    # Override historic entries of national_accounts using historical_overrides data
    mutate_where(
      date >= yearquarter('2020 Q2') & date <= current_quarter,
      data_series = historical_overrides$investment_grants_override
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



#' Create State Purchases Data Series
#'
#' This function combines state purchases data from national accounts and forecast sources 
#' into a single data series, filling any gaps with placeholder zeroes extending to 2034 Q3.
#'
#' @param national_accounts A tibble containing national accounts data with a `date` column and a `gs` column for state purchases.
#' @param forecast A tibble containing forecast data with a `date` column and a `state_purchases` column.
#' @param placeholder_nas A tibble of appropriate length populated by NAs, extending the date range to 2034 Q3.
#'
#' @return A tibble containing the combined state purchases data series, with NAs and missing entries replaced by 0s.
#' @export
#'
#' @examples
#' # Assuming `national_accounts`, `forecast`, and `placeholder_nas` are pre-defined tibbles:
#' state_purchases <- create_state_purchases(national_accounts, forecast, placeholder_nas)
create_state_purchases <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, state_purchases) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = state_purchases)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # `gf` is the Haver code for federal purchases
    select(date, gs) %>%  
    # Rename data to generic `data_series` for easier merging
    rename(data_series = gs)
  
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



create_federal_non_corporate_taxes <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, federal_non_corporate_taxes) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = federal_non_corporate_taxes)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Calculate federal non corporate taxes as a sum of 3 series
    mutate(data_series = 
             gfrpt + # Haver code for federal personal taxes
             gfrpri + # Haver code for federal production taxes
             gfrs # Haver code for federal payroll taxes
           ) %>%
    # Keep the date and the new data_series column
    select(date, data_series)
  
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


create_state_non_corporate_taxes <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, state_non_corporate_taxes) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = state_non_corporate_taxes)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Calculate federal non corporate taxes as a sum of 3 series
    mutate(data_series = 
             gsrpt + # Haver code for state personal taxes
             gsrpri + # Haver code for state production taxes
             gsrs # Haver code for state payroll taxes
    ) %>%
    # Keep the date and the new data_series column
    select(date, data_series)
  
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


create_federal_corporate_taxes <- function(
    national_accounts, 
    forecast, 
    historical_overrides, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, federal_corporate_taxes) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = federal_corporate_taxes)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # `gf` is the Haver code for federal purchases
    select(date, gfrcp) %>%  
    # Rename data to generic `data_series` for easier merging
    rename(data_series = gfrcp) %>%
    # Corporate taxes come one quarter later than GDP. We overwrite JUST the current
    # quarter using the historical overrides sheet
    mutate_where(
      date == current_quarter,
      data_series = tail(historical_overrides$federal_corporate_taxes_override, n = 1)
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


create_supply_side_ira <- function(
    forecast, 
    historical_overrides, 
    placeholder_nas
) {
  # The only input into supply_side_ira is our historical overrides and our 
  # forecast. This data does not exist anywhere in the national accounts.
  
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, supply_side_ira) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = supply_side_ira)
  
  # Select column of interest from the historical overrides
  historical_overrides <- historical_overrides %>%
    select(date, supply_side_ira_override) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = supply_side_ira_override)
  
  # Merge the historical overrides with the forecast using the commonly named 
  # `data_series` and `date` columns. The historical overrides take precedence in
  # the case of any conflicting observations.
  result <- coalesce_join(historical_overrides, forecast, by = 'date') %>% 
    # Merge with a data frame of NAs extending to 2034 Q3
    coalesce_join(
      create_placeholder_nas(start = "1970-01-01"), 
      by = 'date'
      ) %>%
    # Make sure the rows are in chronological order
    arrange(date) %>%
    # Repopulate the NAs to be 0s
    mutate(across(everything(), ~ replace_na(., 0)))
  
  return(result)
}

create_state_corporate_taxes <- function(
    national_accounts, 
    forecast, 
    historical_overrides, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, state_corporate_taxes) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = state_corporate_taxes)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # `gf` is the Haver code for federal purchases
    select(date, gsrcp) %>%  
    # Rename data to generic `data_series` for easier merging
    rename(data_series = gsrcp) %>%
    # Corporate taxes come one quarter later than GDP. We overwrite JUST the current
    # quarter using the historical overrides sheet
    mutate_where(
      date == current_quarter,
      data_series = tail(historical_overrides$state_corporate_taxes_override, n = 1)
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


create_federal_social_benefits <- function(
    national_accounts, 
    forecast, 
    historical_overrides,
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, federal_social_benefits) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = federal_social_benefits)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Calculate federal non corporate taxes as a sum of 3 series
    mutate(data_series = 
             # The coalesce function is replacing 0s for NAs before subtracting
             coalesce(gftfp, 0) - # Haver code for federal social benefits
             coalesce(yptu, 0) - # Haver code for ui
             coalesce(gftfpe, 0) - # Haver code for rebate checks
             coalesce(yptmr, 0) - # Haver code for Medicare
             coalesce(gftfpv, 0) # Haver code for nonprofit provider relief fund
    ) %>%
    # Keep the date and the new data_series column
    select(date, data_series) %>%
    # Override historic entries of national_accounts using historical_overrides data
    mutate_where(
      date >= yearquarter('2020 Q2') & date <= current_quarter,
      data_series = historical_overrides$federal_social_benefits_override
    ) %>%
    # Further overwrite by adding 203 to 2021 Q1
    mutate_where(
      date == yearquarter('2021 Q1'),
      data_series = data_series + 203
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


create_state_social_benefits <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, state_social_benefits) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = state_social_benefits)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Calculate federal non corporate taxes as a sum of 3 series
    mutate(data_series = 
             gstfp - # Haver code for state social benefits
             yptmd # Haver code for Medicaid
    ) %>%
    # Keep the date and the new data_series column
    select(date, data_series)
  
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


create_rebate_checks <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, rebate_checks) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = rebate_checks)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # The coalesce function turns NAs into 0s and the gftfpe is the Haver code 
    # for rebate checks
    mutate(data_series = coalesce(gftfpe, 0)) %>%
    # Keep the date and the new data_series column
    select(date, data_series) %>%
    # Note from Lorae: We split up rebate checks into rebate_checks and rebate_checks_arp,
    # and this subtracts away some amount from a quarter where we allocated it to
    # rebate_checks_arp. We do that so that we can apply a different MPC. But TBH this
    # process is pretty illogical. Please don't blame me ¯\_(ツ)_/¯
    # TODO: Stop this nonsense of hard coding values and put them in the historical_overrrides
    # tab of the forecast spreadsheet instead
    mutate_where(
      date == yearquarter('2021 Q1'),
      data_series = data_series - 1348.1
    ) %>%
    mutate_where(
      date == yearquarter("2021 Q4"),
      data_series = 0)
  
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


create_rebate_checks_arp <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, rebate_checks_arp) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = rebate_checks_arp)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    # Hard coding one silly value here ¯\_(ツ)_/¯
    mutate(
      data_series = if_else(
        date == yearquarter("2021 Q1"),
        1348.1, # Rebate checks arp are 1348.1 in 2021 Q1
        0) # Otherwise they are 0
      )  %>%
    # Hard coding another silly value
    mutate_where(
      date == yearquarter("2021 Q4"),
      data_series = 14.2
      ) %>%
    select(date, data_series) # Keep only the 2 columns we need
  
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

create_federal_ui <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, federal_ui) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = federal_ui)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
      mutate(
      data_series = 
        # The coalesce turns NAS into zeroes
        coalesce(gftfpu, 0) + # Haver code for ui expansion
        coalesce(yptol, 0) # Haver code for wages lost assistance
    )  %>%
    # Hard coding one silly value here ¯\_(ツ)_/¯
    mutate_where(
      date == yearquarter('2021 Q4'),
      data_series = 11
      ) %>%
    select(date, data_series) # Keep only the 2 columns we need
  
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



create_state_ui <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, state_ui) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = state_ui)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    mutate(
      data_series = 
        # This formula calculates state ui as `ui` - `federal_ui`, where
        # `federal_ui` = `ui_expansion + wages_lost assistance`
        yptu - # Haver code for ui
        # The coalesce turns NAS into zeroes
        coalesce(gftfpu, 0) - # Haver code for ui expansion
        coalesce(yptol, 0) # Haver code for wages lost assistance
    )  %>%
    # Since we set federal ui to equal 11, we're recalculating state ui in this
    # quarter to equal ui - 11
    mutate_where(
      date == yearquarter('2021 Q4'),
      data_series = yptu - 11
    ) %>%
    select(date, data_series) # Keep only the 2 columns we need
  
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


create_federal_subsidies <- function(
    national_accounts, 
    forecast, 
    placeholder_nas
) {
  # Select column of interest from the forecast tibble
  forecast <- forecast %>% 
    select(date, federal_subsidies) %>% 
    # Rename to generic `data_series` for easier merging
    rename(data_series = federal_subsidies)
  
  # Select column of interest from the national accounts tibble
  national_accounts <- national_accounts %>% 
    mutate(data_series = gfsub)  %>% # Haver code for federal subsidies
    select(date, data_series) # Keep only the 2 columns we need
  
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