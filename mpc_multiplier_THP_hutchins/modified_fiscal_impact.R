#Section A: prep for new update ----------------------

Sys.setenv(TZ = 'UTC') # Set the default time zone to UTC (Coordinated Universal Time)

librarian::shelf(tidyverse, tsibble, lubridate, glue, TimTeaFan/dplyover, zoo, TTR, fs, gt, openxlsx, 
                 snakecase, rlang, fredr, BrookingsInstitution/ggbrookings) # Load packages
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
  gdp_cbo_growth_rate() %>% #grows current data according to cbo growth rate: gdp and gdph 
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
               state_ui = ui - federal_ui)

# Section D: MPS----------------------------------------------------------------
  ## SUBPART A: read in the data and create MPC and MPS tables
  mpc <- readxl::read_xlsx('data/forecast.xlsx', 
                            sheet = 'mpc', 
                            skip = 1) %>%
    select(-1) %>%
    select(-`Total over 3 years`)
  
  # Get the cumulative MPC
  c_mpc <- mpc %>% # cumulative MPC
    select(-`variable`) %>% # remove the "variable" column so that we can use `apply` function
    apply(.,1,cumsum) %>% # apply cumulative sum
    t() %>% # transpose it back to our original formation
    cbind(mpc[, "variable", drop = FALSE], .) # add back variable names
  
  # Get the cumulative MPS
  c_mps <- c_mpc %>%
    select(-`variable`) %>% # remove the "variable" column so that we can use `apply` function
    mutate(1 - .) %>%
    cbind(mpc[, "variable", drop = FALSE], .) # add back variable names

# # An alternative MPS that is suitable as input into the mps_lorae function (as of 4/20/2023)
alt_mps <- mpc %>% # cumulative MPC
  #select(-`variable`) %>% # remove the "variable" column so that we can use `apply` function
  mutate(`1` = 1 - `1`) %>%
  mutate_at(vars(`2`:`12`), ~ . * -1)

# Rather than defining a separate mpc function for each category of FIM input, we 
# (Lorae and Nasiha) propose (as of 4/19/2023) that we create one MPC function with the type
# of input as an argument. That input then calls the correct row of the MPC table
# with our MPC assumptions.
# see mpc_lorae.R within the R folder for more information.

  ## SUBPART B: Try a practice MPS calculation. Let's use the ui_arp MPS.
  test_mps <- c_mps[which(alt_mps$variable == "ui_arp"),] %>%
    select(-variable) %>%
    unlist()
  # Let's suppose spending looks like this: 
  test_spending <- c(100, 100, 300, 50, 0, 0, 0, 0)
  
  # Applying the MPS function should give us our stream of savings.
  saving_stream <- mps_lorae(x = test_spending, mps = test_mps)
  #Looks good!

  # SUBPART C: Try applying to real data, ignoring the real vs. nominal and "minus
  # neutral" adjustments for now.
  # The columns of the projections data frame called:
  # "federal_ui_arp", "state_ui_arp", "federal_other_vulnerable_arp"
  # are all fed into this function: mpc_vulnerable_arp(.x)
  # [Note: How do we know this? See lines 208 to 233 of fiscal_impact.R
  # The below lines of code are copied from mpc.R.]
  # which has MPC stream:
  mpc_vulnerable_arp <- c(0.2, 0.17, 0.16, 0.15, 0.09, 0.05, 0.05, 0.04)
  # [Note: How do we know this? See line 153 of mpc.R.]
  # These values nicely match what we already have in the "other_vulnerable_arp" row
  # of the mpc data frame. So the c_mps data frame (which was constructed from the
  # mpc data frame) should be good to go for this one. We might have a few MPCs 
  # that don't match between the code and the data frame, but will deal with those 
  # as they occur).
  
  # Let's test run what the spending stream looks like here."ss" stands for "spending
  # stream"
  other_vulnerable_arp_mps <- c_mps[which(alt_mps$variable == "other_vulnerable_arp"),] %>%
    select(-variable) %>%
    unlist()
  ss_federal_ui_arp <- mps_lorae(x = projections$federal_ui, 
                                 mps = other_vulnerable_arp_mps)
  # cool!
  
  # Let's graph using ggplot2.
  df1 <- data.frame(Date = projections$date, Value = projections$federal_ui, Dataset = "Disbursed")
  df2 <- data.frame(Date = projections$date, Value = ss_federal_ui_arp, Dataset = "Implied Spending")
  
  # Combine both data frames
  combined_df <- rbind(df1, df2)
  
  translucent_blue <- rgb(0, 0, 1, alpha = 0.5)
  translucent_orange <- rgb(255/255, 165/255, 0/255, alpha = 0.5)
  
  # Convert Date to a date format using as.Date()
  combined_df$Date <- as.Date(combined_df$Date)
  
  # Set the date range you want to display
  start_date <- as.Date("2019-01-01")
  end_date <- as.Date("2023-05-01")
  
  # Modify the ggplot code to create the overlaid and stacked bar chart with limited date range and adjusted date labels
  bar_chart <- ggplot(combined_df, aes(x = Date, y = Value, fill = Dataset)) + 
    geom_bar(stat = "identity", position = "identity", alpha = 0.8) +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = c(start_date, end_date)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    labs(title = "Federal UI ARP\nDisbursement (using BLS data) versus Implied Spending (using MPC assumptions)",
         x = "Date",
         y = "$ Billions") +
    scale_fill_manual(values = c("Disbursed" = translucent_blue, "Implied Spending" = translucent_orange))
  
  # Display the bar chart
  print(bar_chart)
  
  
  
  
  
  mpc_vulnerable_arp <- c(0.2, 0.17, 0.16, 0.15, 0.09, 0.05, 0.05, 0.04)
  mpc_direct_aid_arp <- mpc(timing = c(0.14, 0.10, 0.1,  rep(0.05, 6), 0.03, 0.03, 0.03))
  mpc_small_businesses_arp <- mpc(timing =  c(rep(0.04, 2), rep(0.017, 10)))
  mpc_ui_arp <-  mpc(timing =  c(0.2, 0.17, 0.16, 0.15,  0.09, rep(0.05, 2), 0.04))
  mpc_non_health_grants_arp <- mpc(timing = c(rep(0.07, 2),
                                              rep(0.049, 10),
                                              rep(0.0475, 7),
                                              0.0375))
  mpc_arp_non_health_grants_dos<- function(x){
    mpc <- 1
    weights <- c(rep(0.07, 2),
                 rep(0.049, 10),
                 rep(0.0475, 7),
                 0.0375)
    
    mpc * roll::roll_sum(x, width = length(weights),
                         weights = rev(weights), online = FALSE,  min_obs = 1)
  }
  

saving <- # Compute saving out of transfers (apply MPS's)
  projections %>%
  get_real_levels() %>%
  taxes_transfers_minus_neutral() %>%
  calculate_mpc("social_benefits") %>%
  #Let's ignore rebate checks for now... they are confusing.
  #mutate(rebate_checks_post_mpc = mpc_rebate_checks(rebate_checks_minus_neutral)) %>%
  calculate_mpc("subsidies") %>%
  calculate_mpc("health_outlays") %>%
  calculate_mpc("corporate_taxes") %>%
  calculate_mpc("non_corporate_taxes") %>% 
  
  # Calculate post-MPC values for federal and state UI benefits
  mutate(across(c(federal_ui_minus_neutral, state_ui_minus_neutral),
                .fns = ~ if_else(date < yearquarter("2021 Q2"),
                                 # Use MPC function for dates before 2021 Q2
                                 mpc_ui(.x),
                                 # Use MPC_ARP function for dates on or after 2021 Q2
                                 mpc_ui_arp(.x)),
                .names = '{.col}_post_mpc'))  %>% 
  
  #doing the same as above but for new variables 
  mutate(across(
    .cols = all_of(
      c(
        "rebate_checks_arp",
        "federal_other_direct_aid_arp",
        "federal_other_vulnerable_arp",
        # "federal_ui_arp",
        #"state_ui_arp",
        "federal_aid_to_small_businesses_arp",
        "federal_student_loans"
      )
    ),
    #Getting the level minus neutral
    .fns = ~ .x - dplyr::lag(.x, default = 0) * (1 + real_potential_gdp_growth + consumption_deflator_growth),
    .names = "{.col}_minus_neutral"
  )) %>% 
  mutate(
    across(
      .cols = any_of(
        c("federal_ui_arp", "state_ui_arp", "federal_other_vulnerable_arp") %>% paste0("_minus_neutral")
      ),
      #getting the post mpc levels for the ARP variables 
      .fns = ~ mpc_vulnerable_arp(.x),
      .names = "{.col}_post_mpc"
    ),
    across(
      .cols = all_of(
        c("rebate_checks_arp", "federal_other_direct_aid_arp", "federal_student_loans") %>% paste0("_minus_neutral")
      ),
      #same as above, applying a different MPC function to these 
      .fns = ~ mpc_direct_aid_arp(.),
      .names = "{.col}_post_mpc"
    ),
    #same as above, applying a different MPC function to this
    federal_aid_to_small_businesses_arp_minus_neutral_post_mpc = 
      mpc_small_businesses_arp((federal_aid_to_small_businesses_arp_minus_neutral))
  )

