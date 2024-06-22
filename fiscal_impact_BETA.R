
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

print(last_month_year)


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
                "gh", "gfh", "gsh", "g", "gf", "gs",
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
  # "cpiu_g", "cola_rate", "health_ui", "cpiu") (and maybe a few others)
  select(id, date, gdp, gdph, gdppothq, gdppotq, starts_with('j'), 
         dc, c, ch ,ends_with('growth'), federal_ui, state_ui, 
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
######################################################################################
# This is the point where we go from generating a data frame to actually calculating the FIM
######################################################################################
# This is the data we will use to calculate the FIM
projections

# Generate the data frame which maps mpcs to specific FIM data time series 
# (subsidies, taxes, transfers, etc).
# Running this module creates variables `mpc_series` and `mpc_list`
n_periods <- nrow(projections) #total number of periods in the data
source("src/map_mpc_time_series.R")

### Define accessory variables. These are non-core data series that are still
# required to calculate the FIM.
federal_purchases_deflator_growth <- projections$federal_purchases_deflator_growth
consumption_grants_deflator_growth <- projections$consumption_grants_deflator_growth
investment_grants_deflator_growth <- projections$investment_grants_deflator_growth
state_purchases_deflator_growth <- projections$state_purchases_deflator_growth
real_potential_gdp_growth <- projections$real_potential_gdp_growth
consumption_deflator_growth <- projections$consumption_deflator_growth
gdp <- projections$gdp

### CALCULATE THE FIM variable-by-variable ########################################
# Source functions from src/calculate_contributions.R that are used to convert
# the 24 raw data series + 7 accessory series into 24 output contributions series
source("src/calculate_contributions.R")

# Define FIM inputs
federal_purchases <- projections$federal_purchases
consumption_grants <- projections$consumption_grants
investment_grants <- projections$investment_grants
state_purchases <- projections$state_purchases


test <- state_purchases_contribution(x = state_purchases,
                                       spdg = state_purchases_deflator_growth,
                                       rpgg = real_potential_gdp_growth,
                                       gdp = gdp) %>%
  as.data.frame()
write.table(test, "clipboard", sep="\t", row.names=FALSE, col.names=FALSE)


