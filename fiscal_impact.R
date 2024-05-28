
# Miscellaneous: set up for Eli because has a different library directory -----
#Get system information
sys_info <- Sys.info()

# Check if the username is 'easdourian'
if (sys_info['user'] == 'easdourian') {
  .libPaths("C:/Users/easdourian/Documents/library")
  # Other code to run if the user is 'easdourian'
} else {
  print("The code is not being run by 'easdourian'")
}

#Section A: prep for new update ----------------------

Sys.setenv(TZ = 'UTC') # Set the default time zone to UTC (Coordinated Universal Time)

librarian::shelf(tidyverse, tsibble, lubridate, glue, TimTeaFan/dplyover, zoo, TTR, fs, gt, openxlsx, 
                 snakecase, rlang, BrookingsInstitution/ggbrookings) # Load packages
devtools::load_all() # Load all functions in package

options(digits = 4) # Limit number of digits
options(scipen = 20)# Turn off scientific notation under 20 digits 

#are we running this after a cbo baseline and pre-bea update?
post_cbo_baseline<- FALSE

if(post_cbo_baseline == TRUE){
  month_year <- glue('{format.Date(today() - 7, "%m")}-{year(today())}-post-cbo')
}else{
  # Set the value of 'month_year' to the current month and year (in the format "mm-yyyy")
  month_year <- glue('{format.Date(today() - 7, "%m")}-{year(today())}')
}



# If the month of the previous month is less than 10, set the value of 'last_month_year' to the previous month and year (in the format "0m-yyyy")
# Otherwise, set the value of 'last_month_year' to the previous month and year (in the format "mm-yyyy")
if((month(today() - 7 
         -months(1)) < 10)){
  last_month_year <- glue('0{month(today() - 7 -months(1))}-{year(today() - dmonths(1) - dweeks(1))}')
} else{
  last_month_year <- glue('{month(today() - 7 -months(1))}-{year(today() - dmonths(1) - dweeks(1))}')
  
}

#setting our reference period to be the post-cbo files if we've already produced fim output incorporating the cbo update
if(file.exists(glue('results/{month_year}-post-cbo'))){
  last_month_year<- glue('{month_year}-post-cbo')
}

# Create updatglibe folders

update_in_progress <- TRUE #set this to false if you're not running the code for a new month

if(update_in_progress == TRUE){
  dir_create(glue('results/{month_year}')) # Create folder for current update in the results directory
  dir_create(glue('results/{month_year}/input_data')) # Folder to store forecast sheet from current update
  
  # Copy the file 'forecast.xlsx' from the 'data' directory to the 'input_data' directory
  # This is the copy we keep for the current update
  file_copy(path = 'data/forecast.xlsx', new_path = glue('results/{month_year}/input_data/forecast_{month_year}.xlsx'), overwrite = TRUE)
}


#### Section B.0: Read in raw data ---------------------------------------------
# Load in national accounts
fim::national_accounts # this is the literal df
load("data/national_accounts.rda") # this loads in a df named national_accounts

# Load in projections
fim::projections # this is the literal df
load("data/projections.rda") # this loads in a df named projections

# Read in historical overrides from data/forecast.xlsx
# Since BEA put all CARES act grants to S&L in Q2 2020 we need to
# override the historical data and spread it out based on our best guess
# for when the money was spent.
historical_overrides <- readxl::read_xlsx('data/forecast.xlsx',
                               sheet = 'historical overrides') %>% # Read in historical_overrides
  select(-name) %>% # Remove longer name since we don't need it
  pivot_longer(-variable,
               names_to = 'date') %>% # Reshape so that variables are columns and dates are rows
  pivot_wider(names_from = 'variable',
              values_from = 'value') %>% 
  mutate(date = yearquarter(date))

# Read in deflator overrides from data/forecast.xlsx
deflator_overrides <- readxl::read_xlsx('data/forecast.xlsx',
                                        sheet = 'deflators_override') %>% # Read in overrides for deflators
  select(-name) %>% # Remove longer name since we don't need it
  pivot_longer(-variable,
               names_to = 'date') %>% # Reshape so that variables are columns and dates are rows
  pivot_wider(names_from = 'variable',
              values_from = 'value') %>% 
  mutate(date = yearquarter(date))


# TODO: This current quarter should be calculated at the top, for the entirety
# of the FIM, not buried down here.
# Save current quarter for later
current_quarter <- historical_overrides %>% slice_max(date) %>% pull(date) 

#### Section B.1: Manipulate projections dataframe------------------------------

projections <- projections %>% 
  # Generate a row of quarterly annualized growth rates for the CPI-U series
  # called `cpiu_g` by applying the qagr function
  mutate(cpiu_g = qagr(cpiu)) %>%
  # Calculate 'cola_rate' for each row: If the quarter is Q1, set 'cola_rate' to
  # the annualized CPI-U growth rate from two quarters ago. Otherwise, set to 
  # NA. This represents the cost-of-living adjustment rate applicable only in 
  # the first quarter of each year, based on CPI-U growth at the end of the 
  # prior year.
  mutate(cola_rate = if_else(lubridate::quarter(date) == 1,
                             lag(cpiu_g, 2),
                             NA)) %>%
  # Fills missing `cola_rate` values forward. Once a `cola_rate` is specified 
  # for Q1, it is carried forward to Q2, Q3, and Q4, applying the same rate 
  # throughout the year until a new rate is defined in the following Q1. 
  tidyr::fill(cola_rate) %>%
  # Smooth budget series
  # Applies a rolling mean over a 4-quarter window to smooth federal taxes, 
  # health outlays, and unemployment insurance data. For each selected column, 
  # the rolling mean is calculated using the current and previous three 
  # quarters' data, aligning the window to the current quarter. If less than 
  # four observations are available (at the data series start), the mean of 
  # available observations is used instead, ensuring no initial data is left 
  # without a smoothed value.
  mutate(across(all_of(c('gfrpt', 'gfrpri', 'gfrcp', 'gfrs', # federal taxes
                         'yptmd', 'yptmr', # health outlays
                         'yptu')), # unemployment insurance
                ~ zoo::rollapply(.x, 
                                 width = 4, 
                                 mean, 
                                 fill = NA,
                                 min_obs = 1, 
                                 align = 'right'))) %>%
  # Implicit price deflators
  mutate(jgf =  gf/gfh,
         jgs = gs/gsh,
         jc = c/ch) %>%
  # Growth rates
  # adds 32 new columns
  mutate(
    across(
      .cols = c("gftfp", "gfrpt", "gfrpri", "gfrcp", "gfrs", "yptmr", 
                "yptmd", "yptu", "state_ui", "federal_ui", 
                "gdp", "gdph", "gdppothq", "gdppotq", "dc", "jgdp", "c", "ch", 
                "gh", "gfh", "gsh", "g", "gf", "gs", "cpiu", 
                "unemployment_rate", 
                "jgf", "jgs", "jc"),
      # Equivalent criteria to the above, but the above is more explicit about
      # which columns are added for later refactoring
      #.cols = where(is.numeric) & !ends_with('_growth'),
      # Calculate quarter growth rate using qgr() function, equal to x/lag(x), 
      # then subtract 1.
      .fns = ~ qgr(.) - 1,
      .names = "{.col}_growth"
    ) 
  ) %>%
  # Turn date into time series
  mutate(date = tsibble::yearquarter(date)) %>%
  # reorder the id column before the date column
  relocate(id, .before = date) %>%
  # convert the projections df into a tsibble data frame type
  tsibble::as_tsibble(key = id, index = date) %>%
  # TODO: as you can see from the select function, many columns are not kept.
  # Perhaps the code can be refactored to exclude the data processing steps 
  # in the first place.
  # These are the columns that are explicitly dropped in this step:
  # c("fy", "gftfp", "gfrpt", "gfrpri", "gfrcp", "gfrs", "yptmr", "yptmd", 
  # "yptu", "federal_ui_timing", "gh", "gfh", "gsh", "g", "gf", "gs", 
  # "cpiu_g", "cola_rate", "health_ui")
  select(id, date, gdp, gdph, gdppothq, gdppotq, starts_with('j'), 
         dc, c, ch ,ends_with('growth'), cpiu, federal_ui, state_ui, 
         unemployment_rate)

## Testing section
projections <- projections 


# TODO: coalesce_join() is a crazy complex function for what looks to be simple
# (append some cols to a data frame). Will have to refactor this function.
# Step 3: Combine these two data frames.
usna1 <- coalesce_join(x = national_accounts,
              y = projections,
              by = 'date') %>%
  as_tsibble(key = id, index = date)


#### Section B.2: to be refactored
usna2 <- usna1 %>%
  gdp_cbo_growth_rate()%>% #grows current data according to cbo growth rate: gdp and gdph 
  define_variables() %>%  # Rename Haver codes for clarity
  as_tsibble(key = id, index = date) %>% # Specifies the time series structure of the data, with the id column as the key and the date column as the index.
  
  mutate_where(id == 'historical',  # Calculate GDP growth for data 
               real_potential_gdp_growth = q_g(real_potential_gdp)) %>% 
  
  #Define FIM variables 
  mutate( 
    # Net out unemployment insurance, rebate checks, and Medicare to apply different MPC's
    federal_social_benefits_gross = federal_social_benefits, # Save original value
    federal_social_benefits = federal_social_benefits - ui - rebate_checks - medicare - nonprofit_provider_relief_fund,
    state_social_benefits = state_social_benefits - medicaid,
    social_benefits = federal_social_benefits + state_social_benefits,
    consumption_grants = gross_consumption_grants - medicaid_grants,
  ) %>% 
  
  mutate(rebate_checks_arp = if_else(date == yearquarter("2021 Q1"), #hardcoding arp rebate checks for one period
                                     1348.1,
                                     0)) %>%
  
  #Set rebate checks ARP to NA because we don't have a projection for them
  mutate_where(id == 'projection',
               rebate_checks_arp = NA,
               federal_ui = NA,
               state_ui = NA) %>%
  
  ##Adjusting data in 2021 because of arp(?)
  mutate_where(date == yearquarter('2021 Q1'),
               rebate_checks = rebate_checks - rebate_checks_arp,
               federal_social_benefits = federal_social_benefits + 203
  ) %>% 
  mutate_where(date == yearquarter("2021 Q4"),
               rebate_checks_arp = 14.2,
               rebate_checks = 0) %>% 
  mutate(consumption_grants = gross_consumption_grants - medicaid_grants,
         
         # Aggregate taxes
         corporate_taxes = federal_corporate_taxes + state_corporate_taxes,
         non_corporate_taxes = federal_non_corporate_taxes + state_non_corporate_taxes) %>% 
  
  ##Set the grants deflator the same as state purchases deflator (the same is done in the forecast/deflators sheet)
  mutate_where(id == 'projection',
               consumption_grants_deflator_growth = state_purchases_deflator_growth,
               investment_grants_deflator_growth = state_purchases_deflator_growth) %>% 
  
  #Overriding historical consumption and investment grant 
  mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
               consumption_grants = historical_overrides$consumption_grants_override) %>% 
  mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter, 
               investment_grants = historical_overrides$investment_grants_override) %>%
  
  #For the full period of the forecast (8 quarters out), replace CBO deflators with the ones from
  #the deflator overrides sheet
  mutate_where(date>current_quarter & date<=max(deflator_overrides$date), 
               consumption_deflator_growth = deflator_overrides$consumption_deflator_growth_override,
               federal_purchases_deflator_growth =deflator_overrides$federal_purchases_deflator_growth_override,
               state_purchases_deflator_growth = deflator_overrides$state_purchases_deflator_growth_override,
               consumption_grants_deflator_growth = deflator_overrides$consumption_grants_deflator_growth_override,
               investment_grants_deflator_growth = deflator_overrides$investment_grants_deflator_growth_override)

# Redefine usna to be integrated back into the FIM
usna <- usna2
# check against old usna file
load("usna_old.RData")
all.equal(usna, usna_old)

# Section C: Forecast ----------------------------------------------------------------
forecast <- # Read in sheet with our forecasted values from the data folder
  readxl::read_xlsx('data/forecast.xlsx',
                    sheet = 'forecast') %>% 
  select(-name) %>% #Remove the 'name' column from the data.
  pivot_longer(-variable,
               names_to = 'date') %>%  #reshape the data 
  pivot_wider(names_from = 'variable',
              values_from = 'value') %>% 
  mutate(date = yearquarter(date)) #convert date to year-quarter format 


projections <- # Merge forecast w BEA + CBO on the 'date' column, 
  #filling in NA values with the corresponding value from the other data frame
  coalesce_join(usna, forecast, by = 'date') %>%  
  
  mutate(# Coalesce NA's to 0 for all numeric values 
    across(where(is.numeric),
           ~ coalesce(.x, 0))) %>%
  
  #Define FIM variables 
  mutate(
    health_outlays = medicare + medicaid,
    federal_health_outlays = medicare + medicaid_grants,
    state_health_outlays = medicaid - medicaid_grants
  ) %>% 
  
  #apply historical_overrides for ARP 
  mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
               federal_other_direct_aid_arp = historical_overrides$federal_other_direct_aid_arp_override,
               federal_other_vulnerable_arp = historical_overrides$federal_other_vulnerable_arp_override,
               federal_social_benefits = historical_overrides$federal_social_benefits_override,
               federal_aid_to_small_businesses_arp = historical_overrides$federal_aid_to_small_businesses_arp_override) %>% 
  mutate_where(date == current_quarter & is.na(federal_corporate_taxes) & is.na(state_corporate_taxes),
               federal_corporate_taxes = tail(historical_overrides$federal_corporate_taxes_override, n = 1),
               state_corporate_taxes = tail(historical_overrides$state_corporate_taxes_override, n = 1)) %>% 
  mutate_where(date == yearquarter("2021 Q1"),
               federal_social_benefits = federal_social_benefits + 203) %>% 
  # FIXME: Figure out why wrong number was pulled from Haver (like 400)
  mutate_where(date == yearquarter('2021 Q4'),
               federal_ui = 11, 
               state_ui = ui - federal_ui) %>%
  #apply historical_overrides for Supply Side IRA
  mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
               supply_side_ira = historical_overrides$supply_side_ira_override) %>%
  #apply historical_overrides for Federal Student Loans
  mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
               federal_student_loans = historical_overrides$federal_student_loans_override)

# Section C.1: Data validation -------------------------------------------------------
## TODO: One would suppose that federal_social_benefits + state_social_benefits
## = social_benefits, but it does not. This divergence starts in 2021 Q1, which
## is row 205. Investigate. 
cbind(projections$social_benefits, 
      projections$federal_social_benefits + projections$state_social_benefits, 
      projections$federal_social_benefits, 
      projections$state_social_benefits)

# Section D: Consumption -------------------------------------------------------------
# Generate the data frame which maps mpcs to specific FIM data time series 
# (subsidies, taxes, transfers, etc).
# Running this module creates variables `mpc_series` and `mpc_list`
n_periods <- nrow(projections) #total number of periods in the data
source("src/map_mpc_time_series.R")

# Initialize a list of unique data series to which minus neutral and mpc will be
# applied to.
# TODO: This list will be universal, from FIM start to FIM finish, so give it a 
# more intuitive name, and don't base it off mpc series: base it off something 
# established earlier. This step should come much earlier in the final, 
# refactored script.
data_series <- names(mpc_series)

# we only keep this section for the purpose of integrating the refactored section
# with the larger code.
consumption_pt1 <-
  projections %>%
  get_real_levels() 

# Section D.0: Real levels -------------------------------------------------------------
# For now, this code is a remnant of the old FIM. But eventually, it will use a list
# of the 24 data series manipulated by the FIM, and take projections as an input and
# output real_df
# TODO: refactor this section
real_df <-
  consumption_pt1

# Section D.1: Minus Neutral -------------------------------------------------------------
# Using the list of 24 data_series that are manipulated by the FIM, we produce a 
# minus_neutral_df, which contains the results of each of these 24 data_series
# after they have been manipulated by this observation.
minus_neutral_df <- apply(
  # consumption_pt1 contains many columns, so we isolate to only the entries
  # that exist in data_series, which are those which we want to calculate 
  # minus_neutral values for.
                          X = real_df[data_series], # eventually we will not need
                          # to subset real_df by data_series, once real_df only
                          # contains columns from data_series.
                          MARGIN = 2, # apply the function to the columns (1 = rows)
                          FUN = minus_neutral, # User-defined minus_neutral function
                          # found in /R/minus_neutral.R
                          rpgg = real_df$real_potential_gdp_growth, #arg from minus_neutral
                          cdg = real_df$consumption_deflator_growth) %>% #arg from minus_neutral
  as.data.frame()

# Section D.2: Post MPC -------------------------------------------------------------
# TODO: put vectorized operations here instead to cut computing time. Separate
# matrix multiplication step from matrix production step.

# Initialize a list to temporarily hold the data before converting it to a dataframe
post_mpc_list <- list()

for (series in data_series) {
  print(series)
  # Generate the MPC matrix for the current series
  mpc_matrix <- comp_mpc_matrix(mpc_vector_list = mpc_list,
                                mpc_series = mpc_series[[series]]) 
  # Formatting the data as a vertical column matrix is not strictly necessary;
  # but it reinforces the point that this is matrix multiplication
  data_vector <- matrix(minus_neutral_df[[{series}]], ncol = 1)
  # ensuring proper NA handling by converting to zeroes
  # TODO: Make only the first value of NA equal to 0. Keep the other NAs as NA
  data_vector[is.na(data_vector)] <- 0
  post_mpc_series <- mpc_matrix %*% data_vector
  
  # Add the result to the list, using the series as the list name. Convert 
  # matrix result to vector if necessary.
  post_mpc_list[[series]] <- as.vector(post_mpc_series) 
}
# Convert the list of vectors into a dataframe
post_mpc_df <- as.data.frame(post_mpc_list)

# Section D.3: Interface refactored code with rest of FIM--------------------------------
# This section is temporary and meant to interface the new workflow of section D
# back into the main FIM by making data structures align. In particular, the main
# FIM uses one giant data frame with all the data_series ("federal_ui", "state_ui", etc)
# appended with all sorts of _minus_neutral and _post_mpc column name suffixes,
# producing a very large data frame. The new refactored code from this section instead
# produces small, nimble data frames called _real_df, _minus_neutral, and _post_mpc,
# which each contain only the 24 columns of data_series names with their respective
# values.
# In order to integrate this code back into the main FIM, we take each of these 
# data frames, append back those suffixes, combine them together, and reorder the
# columns so they exactly match the original FIM.
# As downstream sections are refactored, this will no longer be necessary. This is a
# temporary solution.

# minus_neutral_renamed_df
minus_neutral_renamed_df <- minus_neutral_df
colnames(minus_neutral_renamed_df) <- c(glue::glue('{data_series}_minus_neutral'))
# Append the new _minus_neutral columns to the consumption_pt1 df
consumption_pt2 <- dplyr::bind_cols(consumption_pt1, minus_neutral_renamed_df)

# post_mpc_renamed_df
post_mpc_renamed_df <- post_mpc_df
colnames(post_mpc_renamed_df) <- c(glue::glue('{data_series}_post_mpc'))
# Append the new _post_mpc columns to consumption_pt2 df 
consumption_pt3 <- bind_cols(consumption_pt2, post_mpc_renamed_df)

# Apply column order to perfectly match old version of fim. These lines can be
# deleted later if column order turns out to be irrelevant.
load("TEMP_consumption_newnames.RData")
load("TEMP_consumption_newnames_column_order.RData")
consumption_new <- consumption_pt3 %>%
  .[, consumption_newnames_column_order]
end <- nrow(consumption_newnames)
# Comparing all but the first row of the two consumption data frames:
# consumption_newnames is the original FIM data frame from main branch with the
# colnames edited slightly to match this standardized version; consumption_new
# is the new df generated here. Their first rows do not match because of the
# _minus_neutral function, which used to have a defauly lag = 0 for some pandemic-
# era programs. Lorae has since standardized this, causing the slight difference
# in NA entries. Thus, comparing the second entry onward is a more appropriate
# check.
old_data <- consumption_newnames[2:end,] %>%
  select(-ui_minus_neutral) # remove this column since we eliminated while refactoring
new_data <- consumption_new[2:end,]
# check if newly refactored result matches old result
all.equal(old_data, new_data)

# Assign result to the consumption df, so rest of code runs smoothly.
# consumption_new is just a temporary feature to compare between new and old fims
consumption <- consumption_new

# Section E: Contribution ------------------------------------------------------------

contributions_pt1 <- # Calculate contributions
  consumption %>%
  purchases_contributions() %>% 
  
  mutate(across(ends_with("post_mpc"),
                #multiplies each value for all post_mpc cols by 400 and divides by the corresponding value in the "gdp" column from the previous row 
                #The resulting values are given new column names, adding "{.col}_contribution" to the original column names
                ~ 400 * .x / lag(gdp),
                .names = "{.col}_contribution" 
  )) %>%
  # TODO: Standardize this section so that retroactively renaming post_mpc_contribution
  # columns to _contribution columns is unnecessary
  rename_with(~ str_replace(.x, "post_mpc_contribution", "contribution")) %>% 
  sum_transfers_contributions() #%>% 

contributions_pt2 <- contributions_pt1 %>%
  #Define FIM variables for grants and purchases
  mutate(
    grants_contribution = consumption_grants_contribution + investment_grants_contribution,
    federal_contribution = federal_purchases_contribution + grants_contribution,
    state_contribution = state_purchases_contribution - grants_contribution
  ) %>%
  
  #Define FIM variables for social benefits 
  mutate(social_benefits_contribution = federal_social_benefits_contribution + 
           state_social_benefits_contribution) %>%
  
  #Define FIM variables for taxes 
  mutate(non_corporate_taxes_contribution = federal_non_corporate_taxes_contribution + 
           state_non_corporate_taxes_contribution) %>%
  mutate(federal_corporate_taxes_contribution = federal_corporate_taxes_contribution +
           supply_side_ira_contribution) %>%
  mutate(taxes_contribution = non_corporate_taxes_contribution + 
           corporate_taxes_contribution) %>%
  
  #Define FIM variables for taxes and transfers  
  mutate(
    transfers_contribution = federal_social_benefits_contribution + 
      state_social_benefits_contribution +
      rebate_checks_contribution + 
      rebate_checks_arp_contribution + 
      federal_ui_contribution + 
      state_ui_contribution +
      federal_subsidies_contribution + 
      federal_aid_to_small_businesses_arp_contribution +  
      state_subsidies_contribution + 
      federal_health_outlays_contribution +
      state_health_outlays_contribution + 
      federal_other_direct_aid_arp_contribution + 
      federal_other_vulnerable_arp_contribution +
      federal_student_loans_contribution,
    
    taxes_contribution = federal_non_corporate_taxes_contribution + 
      state_non_corporate_taxes_contribution +
      federal_corporate_taxes_contribution + 
      state_corporate_taxes_contribution
  ) %>%
  
  #Add student loans to federal transfers 
  # mutate(federal_transfers_contribution = federal_transfers_contribution +
  #   federal_student_loans_contribution)%>%
  
  #Define FIM subsidies 
  mutate(subsidies = federal_subsidies + state_subsidies,
         subsidies_contribution = federal_subsidies_contribution + state_subsidies_contribution) %>% 
  
  #Calculate the FIM for the defined variables 
  get_fiscal_impact() %>%
  
  #Do the same aggregation of the real levels 
  mutate(
    grants_real = consumption_grants_real + investment_grants_real,
    federal_real = federal_purchases_real + grants_real,
    state_real = state_purchases_real - grants_real
  ) %>%
  mutate(social_benefits_real = federal_social_benefits_real + state_social_benefits_real) %>%
  mutate(non_corporate_taxes_real = federal_non_corporate_taxes_real + state_non_corporate_taxes_real) %>%
  mutate(taxes_real = non_corporate_taxes_real + corporate_taxes_real) %>%
  mutate(
    transfers_real = federal_social_benefits_real + state_social_benefits_real +
      rebate_checks_real + rebate_checks_arp_real + federal_ui_real + state_ui_real +
      federal_subsidies_real + federal_aid_to_small_businesses_arp_real +  state_subsidies_real + federal_health_outlays_real +
      state_health_outlays_real + federal_other_direct_aid_arp_real + federal_other_vulnerable_arp_real +federal_student_loans_real,
    taxes_real = federal_non_corporate_taxes_real + state_non_corporate_taxes_real +
      federal_corporate_taxes_real + state_corporate_taxes_real
  ) %>% 
  mutate( subsidies_real = federal_subsidies_real + state_subsidies_real)

# check if newly refactored result matches old result
load("TEMP_contributions.RData") #old contributions
contributions_new <- contributions_pt2 # new contributions

old_data <- contributions[2:end,] %>%
  select(-ui_minus_neutral) # remove this column since we eliminated while refactoring
new_data <- contributions_new[2:end,]

all.equal(old_data, new_data)

# for later use in this code
contributions <- contributions_new

#openxlsx::write.xlsx: This function writes an R data object to an .xlsx file (an Excel spreadsheet).
#file = glue('results/{month_year}/fim-{month_year}.xlsx'): This specifies the file path and name of the .xlsx file that the data will be written to. The glue() function is being used to dynamically create the file path and name using the month_year variable.
#overwrite = TRUE: This specifies that if the .xlsx file already exists, it should be overwritten with the new data.
#write_rds(contributions, file = 'data/contributions.rds'): This function writes the contributions data object to an .rds file (an R binary file) with the file path and name specified by the file argument.
#usethis::use_data(contributions, overwrite = TRUE): This function writes the contributions data object to a .RData file in the current working directory and makes it available in the global environment. The overwrite = TRUE argument specifies that if a .RData file with the same name already exists, it should be overwritten with the new data.

openxlsx::write.xlsx(contributions, file = glue('results/{month_year}/fim-{month_year}.xlsx'), overwrite = TRUE)
write_rds(contributions, file = 'data/contributions.rds')
usethis::use_data(contributions, overwrite = TRUE)


# Section F: Web materials  -------------------------------------------------------------

# Interactive data
# Generate interactive data frame from contributions
interactive <- 
  contributions %>% 
  # Filter rows of contributions by date, keeping only those between 1999 Q4 and current quarter + 8
  filter_index('1999 Q4' ~ as.character(current_quarter + 8)) %>% 
  
  # Create new columns in data frame:
  # - consumption: sum of transfers_contribution and taxes_contribution
  # - recession: recode recession column such that -1 is replaced with 0
  # - recession: replace NA values in recession column with 0
  # - id: recode id column such that "historical" is replaced with 0 and "projection" is replaced with 1
  mutate(consumption = transfers_contribution + taxes_contribution,
         recession = recode(recession, `-1` = 0),
         recession = replace_na(recession, 0),
         id = recode(id, 
                     historical = 0,
                     projection = 1)) %>% 
  
  # Select only specific columns:
  # date, impact (renamed from fiscal_impact_moving_average), recession, total (renamed from fiscal_impact), 
  # federal (renamed from federal_contribution), state_local (renamed from state_contribution), 
  # consumption, projection (renamed from id)
  select(date, 
         impact = fiscal_impact_moving_average,
         recession,
         total = fiscal_impact,
         federal = federal_contribution,
         state_local = state_contribution,
         consumption,
         projection = id) %>% 
  
  # Split date column into year and quarter columns
  separate(date, c('year', 'quarter'))

# Write interactive data frame to CSV file
readr::write_csv(interactive,  file = glue('results/{month_year}/interactive-{month_year}.csv'))

# Figures for website
rmarkdown::render('Fiscal-Impact.Rmd',
                  # Render R Markdown document to PDF file
                  output_file = 'Fiscal-Impact.html',
                  clean = TRUE,
                  params = list(start = yearquarter('1999 Q4'), end = current_quarter + 8))

# Copy html file to new location, overwriting any existing file
file_copy(path = 'Fiscal-Impact.html',
         new_path = glue('results/{month_year}/Fiscal-Impact-{month_year}.html'),
         overwrite = TRUE)

# Section G: Comparison ------------------------------------------------------------

source('scripts/revision_figures.R')
source('scripts/revision_table.R')
source('scripts/revision_deflators.R')

rmarkdown::render(input = 'update-comparison.Rmd',
                  output_file = glue('results/{month_year}/update-comparison-{month_year}'),
                  clean = TRUE)

file_copy(
  path = glue('results/{month_year}/update-comparison-{month_year}.html'),
  new_path = 'index.html',
  overwrite = TRUE
)

#nasiha-added-temp
ns_hist<- comparison_wide[, 1:10] 
ns_proj<- comparison_wide[, c(1:3, 11:20)]

ns_hist <- ns_hist %>% filter(id == "historical") %>% select(-"id")
ns_proj <- ns_proj %>% filter(id == "projection")%>% select(-"id")

ns_comparison<- left_join(ns_hist, ns_proj, by = c("name", "source")) %>% arrange(name)
readr::write_csv(ns_comparison,  file = glue('results/{month_year}/ns_comparison-{month_year}.csv'))


# # State and local employment ------------------------------------------------------------------
# 
# # In order to use the API, you first need to create an account here: http://api.stlouisfed.org/api_key.html. 
# # Once you have the account, store the API key in your R environment. This protects your API key so it's not shared on GitHub or anywhere else. You can store your key by running usethis::edit_r_environ() to open your .Renviron file and typing FRED_API_KEY=YOUR-API-KEY. Finally save and close the .Renviron file and restart your R Session. 
# # You can also use the hutchinsras@gmail.com FRED account's API key. 
# # For more information see http://sboysel.github.io/fredr/articles/fredr.html
# 
# # Data comes from: 
# # - State govt employment: https://fred.stlouisfed.org/series/CES9092000001
# # - Local govt employment: https://fred.stlouisfed.org/series/CES9093000001
# fredr_series_search_tags(
#   series_search_text = "All Employees, Local Government",
#   limit = 100L
# ) %>% View()
# 
# # Calculate percentage change in state + local employment relative to February 2020 (pre-pandemic)
# map_dfr(c("CES9092000001", "CES9093000001"), fredr, frequency = 'm', observation_start = as_date('2020-02-01')) %>% 
#   select(date, series_id, value) %>% 
#   group_by(date) %>% 
#   summarise(employment = sum(value), .groups = 'drop') %>% 
#   filter(date == first(date) | date == last(date)) %>% 
#   summarise(employment_growth = scales::percent((employment / lag(employment) - 1), accuracy = 0.01)) %>% 
#   drop_na()


