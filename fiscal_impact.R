
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


#Section B: Wrangle data ------------------------------------------------------------

# Since BEA put all CARES act grants to S&L in Q2 2020 we need to
# override the historical data and spread it out based on our best guess
# for when the money was spent.
overrides <- readxl::read_xlsx('data/forecast.xlsx',
                               sheet = 'historical overrides') %>% # Read in overrides
  select(-name) %>% # Remove longer name since we don't need it
  pivot_longer(-variable,
               names_to = 'date') %>% # Reshape so that variables are columns and dates are rows
  pivot_wider(names_from = 'variable',
              values_from = 'value') %>% 
  mutate(date = yearquarter(date))

current_quarter <- overrides %>% slice_max(date) %>% pull(date) # Save current quarter for later

##
deflators_override <- readxl::read_xlsx('data/forecast.xlsx',
                                        sheet = 'deflators_override') %>% # Read in overrides for deflators
  select(-name) %>% # Remove longer name since we don't need it
  pivot_longer(-variable,
               names_to = 'date') %>% # Reshape so that variables are columns and dates are rows
  pivot_wider(names_from = 'variable',
              values_from = 'value') %>% 
  mutate(date = yearquarter(date))

##
usna <-
  read_data() %>% # Load raw BEA data from Haver and CBO projections
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
               consumption_grants = overrides$consumption_grants_override) %>% 
  mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter, 
               investment_grants = overrides$investment_grants_override) %>%
  
  #For the full period of the forecast (8 quarters out), replace CBO deflators with the ones from
  #the deflators override sheet
  mutate_where(date>current_quarter & date<=max(deflators_override$date), 
               consumption_deflator_growth = deflators_override$consumption_deflator_growth_override,
               federal_purchases_deflator_growth =deflators_override$federal_purchases_deflator_growth_override,
               state_purchases_deflator_growth = deflators_override$state_purchases_deflator_growth_override,
               consumption_grants_deflator_growth = deflators_override$consumption_grants_deflator_growth_override,
               investment_grants_deflator_growth = deflators_override$investment_grants_deflator_growth_override)


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
  
  #apply overrides for ARP 
  mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
               federal_other_direct_aid_arp = overrides$federal_other_direct_aid_arp_override,
               federal_other_vulnerable_arp = overrides$federal_other_vulnerable_arp_override,
               federal_social_benefits = overrides$federal_social_benefits_override,
               federal_aid_to_small_businesses_arp = overrides$federal_aid_to_small_businesses_arp_override) %>% 
  mutate_where(date == current_quarter & is.na(federal_corporate_taxes) & is.na(state_corporate_taxes),
               federal_corporate_taxes = tail(overrides$federal_corporate_taxes_override, n = 1),
               state_corporate_taxes = tail(overrides$state_corporate_taxes_override, n = 1)) %>% 
  mutate_where(date == yearquarter("2021 Q1"),
               federal_social_benefits = federal_social_benefits + 203) %>% 
  # FIXME: Figure out why wrong number was pulled from Haver (like 400)
  mutate_where(date == yearquarter('2021 Q4'),
               federal_ui = 11, 
               state_ui = ui - federal_ui) %>%
  #apply overrides for Supply Side IRA
  mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
               supply_side_ira = overrides$supply_side_ira_override) %>%
  #apply overrides for Federal Student Loans
  mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
               federal_student_loans = overrides$federal_student_loans_override)

# Section D: Consumption -------------------------------------------------------------
 
# generalized minus_neutral function
minus_neutral <- function(x, # the data in question
                          rpgg, # real_potential_gdp_growth,
                          cdg # consumption deflator growth
                          ) {
  output <- x - lag(x, default = 0) * (1 + rpgg + cdg)
  return(output)
}

# Isolating the first part of consumption, which I will not attempt
# to refactor (for now)
consumption_pt1 <- # Compute consumption out of transfers (apply MPC's)
  projections %>%
  get_real_levels() %>%
  taxes_transfers_minus_neutral() 

## NOTE: So, one would suppose that federal_social_benefits + state_social_benefits
## = social_benefits, but it does not. TODO: Investigate later.
logical_vector <- consumption_pt1$federal_social_benefits + consumption_pt1$state_social_benefits == consumption_pt1$social_benefits
any_false <- any(!logical_vector)
print(any_false)
# as you can see, there are false elements in this vector comparing the two values
# try subtracting or using all.equal() function - it's possible the differences
# are miniscule.

# Second part of consumption, which will be refactored
consumption_pt2 <-
  consumption_pt1 %>%
  
  # Ok, so this is how the social benefits work. As an input, you take either the
  # column social_benefits_minus_neutral, federal_social_benefits_minus_neutral, or 
  # state_social_benefits_minus_neutral. Those are your inputs. You throw those into the
  # function mpc_social_benefits(x) as the x variable. What you get as an output
  # is then labelled as social_benefits_post_mpc, federal_social_benefits_post_mpc,
  # or state_social_benefits_post_mpc. Those are appended to the given data frame.
  
  # In this next section of code, I've replaced that section with some code that
  # appends new columns using my custom mpc_lorae() function.
  
  # Creating the social_benefits_post_mpc column using mpc_lorae formula
  mutate(social_benefits_post_mpc = mpc_lorae(x = social_benefits_minus_neutral,
                                              mpc = c(0.225, 0.225, 0.225, 0.225))) %>%
  # Doing the same thing, this time w federal_social_benefits_post_mpc
  mutate(federal_social_benefits_post_mpc = mpc_lorae(x = federal_social_benefits_minus_neutral,
                                              mpc = c(0.225, 0.225, 0.225, 0.225))) %>%
  # Doing the same thing, this time w state_social_benefits_post_mpc
  mutate(state_social_benefits_post_mpc = mpc_lorae(x = state_social_benefits_minus_neutral,
                                                      mpc = c(0.225, 0.225, 0.225, 0.225))) %>%
  
  # TODO: REBATE CHECKS ARE TRICKY SO LEAVE REFACTORING TO END.
  mutate(rebate_checks_post_mpc = mpc_rebate_checks(rebate_checks_minus_neutral)) %>%
  
  # subsidies, federal subsidies, and state subsidies
  mutate(subsidies_post_mpc = if_else(date < yearquarter("2021 Q2"),
                                      mpc_lorae(subsidies_minus_neutral, 0.45 * c(0.11, 0.095, 0.09, 0.085, 0.075, 0.075, 0.075, 0.075, 0.06, 0.06, 0.06, 0.06, 0.02, 0.02, 0.02, 0.02)),
                                      mpc_lorae(subsidies_minus_neutral, 0.45 * c(0.11, 0.095, 0.09, 0.085, 0.075, 0.075, 0.075, 0.075, 0.06, 0.06, 0.06, 0.06, 0.02, 0.02, 0.02, 0.02)))) %>%
  mutate(federal_subsidies_post_mpc = if_else(date < yearquarter("2021 Q2"),
                                              mpc_lorae(federal_subsidies_minus_neutral, 0.45 * c(0.11, 0.095, 0.09, 0.085, 0.075, 0.075, 0.075, 0.075, 0.06, 0.06, 0.06, 0.06, 0.02, 0.02, 0.02, 0.02)),
                                              mpc_lorae(federal_subsidies_minus_neutral, 0.45 * c(0.11, 0.095, 0.09, 0.085, 0.075, 0.075, 0.075, 0.075, 0.06, 0.06, 0.06, 0.06, 0.02, 0.02, 0.02, 0.02)))) %>%
  mutate(state_subsidies_post_mpc = if_else(date < yearquarter("2021 Q2"),
                                      mpc_lorae(state_subsidies_minus_neutral, 0.45 * c(0.11, 0.095, 0.09, 0.085, 0.075, 0.075, 0.075, 0.075, 0.06, 0.06, 0.06, 0.06, 0.02, 0.02, 0.02, 0.02)),
                                      mpc_lorae(state_subsidies_minus_neutral, 0.45 * c(0.11, 0.095, 0.09, 0.085, 0.075, 0.075, 0.075, 0.075, 0.06, 0.06, 0.06, 0.06, 0.02, 0.02, 0.02, 0.02)))) %>%
  
  # Health outlays work the same way as social benefits. Process:
  # health_outlays_minus_neutral -> {apply mpc_health_outlays()} -> health_outlays_post_mpc
  # federal_health_outlays_minus_neutral -> {apply mpc_health_outlays()} -> federal_health_outlays_post_mpc
  # state_health_outlays_minus_neutral -> {apply mpc_health_outlays()} -> state_health_outlays_post_mpc
  # 
  # When I compare my mpc_lorae function to the original outputs, using the
  # mpc_health_outlays function, I get a miniscule difference, on the order of 
  # 10*(-15), which is the result of computer rounding - not any
  # theoretical / mathematical differences.
  
  # Creating the health_outlays_post_mpc column using mpc_lorae formula
  mutate(health_outlays_post_mpc = mpc_lorae(x = health_outlays_minus_neutral,
                                            mpc = c(0.225, 0.225, 0.225, 0.225))) %>%
  # Doing the same thing, this time w federal_health_outlays_post_mpc
  mutate(federal_health_outlays_post_mpc = mpc_lorae(x = federal_health_outlays_minus_neutral,
                                                      mpc = c(0.225, 0.225, 0.225, 0.225))) %>%
  # Doing the same thing, this time w state_social_benefits_post_mpc
  mutate(state_health_outlays_post_mpc = mpc_lorae(x = state_health_outlays_minus_neutral,
                                                    mpc = c(0.225, 0.225, 0.225, 0.225))) %>%
  
  # Corporate taxes work the same as health outlays and social benefits.
  # When I compare my mpc_lorae function to the original outputs, using the
  # I get a miniscule difference, on the order of 10*(-15), which is the result 
  # of computer rounding - not any theoretical / mathematical differences.
  mutate(corporate_taxes_post_mpc = mpc_lorae(x = corporate_taxes_minus_neutral,
                                             mpc = rep(-0.0333333333333333, 12))) %>%
  mutate(federal_corporate_taxes_post_mpc = mpc_lorae(x = federal_corporate_taxes_minus_neutral,
                                                     mpc = rep(-0.0333333333333333, 12))) %>%
  mutate(state_corporate_taxes_post_mpc = mpc_lorae(x = state_corporate_taxes_minus_neutral,
                                                   mpc = rep(-0.0333333333333333, 12))) %>%
  
  # Non-corporate taxes. As before, tiny difference that's near 0
  mutate(non_corporate_taxes_post_mpc = mpc_lorae(x = non_corporate_taxes_minus_neutral,
                                              mpc = c(-0.12, -0.12, -0.06, -0.06, -0.06, -0.06, -0.06, -0.06))) %>%
  mutate(federal_non_corporate_taxes_post_mpc = mpc_lorae(x = federal_non_corporate_taxes_minus_neutral,
                                                      mpc = c(-0.12, -0.12, -0.06, -0.06, -0.06, -0.06, -0.06, -0.06))) %>%
  mutate(state_non_corporate_taxes_post_mpc = mpc_lorae(x = state_non_corporate_taxes_minus_neutral,
                                                    mpc = c(-0.12, -0.12, -0.06, -0.06, -0.06, -0.06, -0.06, -0.06))) %>%
  
  # Calculate pandemic-adjusted MPC values for federal and state UI benefits
  mutate(across(c(federal_ui_minus_neutral, state_ui_minus_neutral),
                .fns = ~ if_else(date < yearquarter("2021 Q2"),
                                 # Use MPC function for dates before 2021 Q2
                                 mpc_ui(.x),
                                 # Use MPC_ARP function for dates on or after 2021 Q2
                                 mpc_ui_arp(.x)),
                .names = '{.col}_post_mpc'))  %>% 
  
  # Unlike in the above sections, we must first generate xxxx_minus_neutral
  # before we can feed it into the mpc function to generate xxx_post_mpc.
  # So each line item in this section will have two new lines of code: one to
  # generate xxxx_minus_neutral, and one to generate xxx_post_mpc.
  # Let's start with rebate_checks_arp.
  # generate rebate_checks_arp_minus_neutral
  mutate(rebate_checks_arp_minus_neutral = minus_neutral(x = rebate_checks_arp, 
                                                         rpgg = real_potential_gdp_growth, 
                                                         cdg = consumption_deflator_growth)) %>%
  # generate rebate_checks_arp_post_mpc
  # note that the code I'm refactoring generates rebate_checks_arp_minus_neutral_post_mpc
  # which breaks the convention of just calling it rebate_checks_arp_post_mpc.
  # but I'm keeping it this way for now because I don't want to unintentionally 
  # break downstream code.
  # TODO: rename this variable to follow convention and update downstream code
  mutate(rebate_checks_arp_minus_neutral_post_mpc = mpc_lorae(x = rebate_checks_arp_minus_neutral, 
                                                              mpc = c(0.14, 0.1, 0.1, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.03, 0.03, 0.03, 0.025, 0.02, 0.015, 0.01, 0.005))) %>%
  # generate federal_other_direct_aid_arp  _minus_neutral and _minus_neutral_post_mpc
  mutate(federal_other_direct_aid_arp_minus_neutral = minus_neutral(x = federal_other_direct_aid_arp, 
                                                         rpgg = real_potential_gdp_growth, 
                                                         cdg = consumption_deflator_growth)) %>%
  mutate(federal_other_direct_aid_arp_minus_neutral_post_mpc = mpc_lorae(x = federal_other_direct_aid_arp_minus_neutral, 
                                                              mpc = c(0.14, 0.1, 0.1, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.03, 0.03, 0.03, 0.025, 0.02, 0.015, 0.01, 0.005))) %>%
  # generate federal_other_vulnerable_arp _minus_neutral and _minus_neutral_post_mpc
  mutate(federal_other_vulnerable_arp_minus_neutral = minus_neutral(x = federal_other_vulnerable_arp, 
                                                                    rpgg = real_potential_gdp_growth, 
                                                                    cdg = consumption_deflator_growth)) %>%
  mutate(federal_other_vulnerable_arp_minus_neutral_post_mpc = mpc_lorae(x = federal_other_vulnerable_arp_minus_neutral, 
                                                                         mpc = c(0.2, 0.17, 0.16, 0.15, 0.09, 0.05, 0.05, 0.04))) %>%
  # generate federal_aid_to_small_businesses_arp _minus_neutral and _minus_neutral_post_mpc
  mutate(federal_aid_to_small_businesses_arp_minus_neutral = minus_neutral(x = federal_aid_to_small_businesses_arp, 
                                                                    rpgg = real_potential_gdp_growth, 
                                                                    cdg = consumption_deflator_growth)) %>%
  mutate(federal_aid_to_small_businesses_arp_minus_neutral_post_mpc = mpc_lorae(x = federal_aid_to_small_businesses_arp_minus_neutral, 
                                                                         mpc = c(0.04, 0.04, 0.017, 0.017, 0.017, 0.017, 0.017, 0.017, 0.017, 0.017, 0.017, 0.017))) %>%
  # generate federal_student_loans _minus_neutral and _minus_neutral_post_mpc
  mutate(federal_student_loans_minus_neutral = minus_neutral(x = federal_student_loans, 
                                                                           rpgg = real_potential_gdp_growth, 
                                                                           cdg = consumption_deflator_growth)) %>%
  mutate(federal_student_loans_minus_neutral_post_mpc = mpc_lorae(x = federal_student_loans_minus_neutral, 
                                                                                mpc = c(0.04, 0.04, 0.017, 0.017, 0.017, 0.017, 0.017, 0.017, 0.017, 0.017, 0.017, 0.017))) %>%
  # generate supply_side_ira _minus_neutral and _minus_neutral_post_mpc
  mutate(supply_side_ira_minus_neutral = minus_neutral(x = supply_side_ira, 
                                                             rpgg = real_potential_gdp_growth, 
                                                             cdg = consumption_deflator_growth)) %>%
  mutate(supply_side_ira_minus_neutral_post_mpc = mpc_lorae(x = supply_side_ira_minus_neutral, 
                                                                  mpc = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0))) %>%

# assign result to the consumption df, so rest of code runs smoothly
consumption <- consumption_pt2

# Section E: Contribution ------------------------------------------------------------

contributions <- # Calculate contributions
  consumption %>%
  purchases_contributions() %>% 
  
  mutate(across(ends_with("post_mpc"),
                #multiplies each value for all post_mpc cols by 400 and divides by the corresponding value in the "gdp" column from the previous row 
                #The resulting values are given new column names, adding "{.col}_contribution" to the original column names
                ~ 400 * .x / lag(gdp),
                .names = "{.col}_contribution" 
  )) %>%
  rename_with(~ str_replace(.x, "_minus_neutral_post_mpc_contribution", "_contribution")) %>% 
  rename_with(~ str_replace(.x, "minus_neutral_post_mpc", "post_mpc")) %>% 
  rename_with(~ str_replace(.x, "post_mpc_contribution", "contribution")) %>% 
  sum_transfers_contributions() %>% 
  
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


