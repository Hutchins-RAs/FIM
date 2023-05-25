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
  ##### SUBPART A: read in the data and create MPC and MPS tables #####
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

  ##### SUBPART B: Try a practice MPS calculation. Let's use the ui_arp MPS. #####
  test_mps <- c_mps[which(alt_mps$variable == "ui_arp"),] %>%
    select(-variable) %>%
    unlist()
  # Let's suppose spending looks like this: 
  test_spending <- c(100, 100, 300, 50, 0, 0, 0, 0)
  
  # Applying the MPS function should give us our stream of savings.
  # Note: we now have an mps function which divides by 4
  saving_stream <- mps_lorae(x = test_spending, mps = test_mps)
  #Looks good!

  ##### SUBPART C: Try applying to real data, ignoring the real vs. nominal and "minus
  # neutral" adjustments for now. #####
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
  
  # Let's test run what the saving stream looks like here."ss" stands for "saving
  # stream"
  other_vulnerable_arp_mps <- c_mps[which(alt_mps$variable == "other_vulnerable_arp"),] %>%
    select(-variable) %>%
    unlist()
  ss_federal_ui_arp <- mps_lorae(x = projections$federal_ui, 
                                 mps = other_vulnerable_arp_mps)
  # Let's graph it. We're using the graph_mps function from graph_mps.R
  graph_mps(disbursed = projections$federal_ui, # How much $ was actually disbursed
            ss = ss_federal_ui_arp, # Our best guesses on savings resulting from disbursement
            date = projections$date, # A vector of the dates used in the graph
            start = "2019-01-01", # Graph start date
            end = "2025-01-01", # Graph end date
            title = "Federal UI ARP", # Graph title
            terminal = other_vulnerable_arp_mps[12] %>% as.numeric(), # terminal savings rate
            annualized = TRUE
  )
  
  ##### PART D: Apply to other data. #####
  # It turns out that the above calculation isn't quite right. We use two different
  # MPCs for ARP, depending on whether it was before or after 2021 Q1. I'll start
  # with some more simplistic examples.
  
  
  ## initialize the pdf
  # Start the PDF device driver
  pdf("mpc_multiplier_THP_hutchins/output-2023.05.22.pdf",
      width = 11,
      height = 8.5)
  
    ### Federal other vulnerable ARP
    # The MPCs in the table do match the MPCs in the code
    federal_other_vulnerable_arp <-
      ss_graph_wrapper(disbursed = projections$federal_other_vulnerable_arp, # How much $ was actually disbursed
               mps_name = "other_vulnerable_arp", # Which MPS to use
               date = projections$date, # A vector of the dates used in the graph
               start = "2019-01-01", # Graph start date
               end = "2025-01-01", # Graph end date
               title = "Figure 1A: Federal Other Vulnerable ARP", # Graph title
               annualized = TRUE)
    # Print the plot into the PDF
    print(federal_other_vulnerable_arp$line)
    # Save the plot as a PNG file
    ggsave("mpc_multiplier_THP_hutchins/figures/fig01a.png", plot = federal_other_vulnerable_arp$line, width = 10, height = 8, dpi = 300)
    
    ### Rebate Checks ARP
    # CAUTION: This is another one where MPCs in the table don't match MPCs in the code.
    # Code MPCs:
    mpc_direct_aid_arp
    # implies MPCs are:
    # (0.14, 0.1, 0.1, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.03, 0.03, 0.03)
    # but the table
    mpc[which(mpc$variable == "other_direct_aid_arp"),]
    # implies MPCs are:
    # (0.14, 0.1, 0.1, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0, 0, 0)
    rebate_checks_arp <-
      ss_graph_wrapper(disbursed = projections$rebate_checks_arp, # How much $ was actually disbursed
                       mps_name = "other_direct_aid_arp", # Which MPS to use
                       date = projections$date, # A vector of the dates used in the graph
                       start = "2019-01-01", # Graph start date
                       end = "2025-01-01", # Graph end date
                       title = "Figure 1B: Rebate Checks ARP") # Graph title
    # Print the plot into the PDF
    print(rebate_checks_arp$line)
    # Save the plot as a PNG file
    ggsave("mpc_multiplier_THP_hutchins/figures/fig01b.png", plot = rebate_checks_arp$line, width = 10, height = 8, dpi = 300)
    

    ### Rebate Checks (non ARP?)
    # The MPCs in the table DO NOT match the MPCs in the code
    # Code MPCs:
    mpc_rebate_checks
    # Implies mpcs are:
    # (0.35, 0.15, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08)
    #But the table
    mpc[which(mpc$variable == "rebate_checks"),]
    # implies MPCs are:
    # (0.245, 0.105, 0.056, 0.056, 0.056, 0.056, 0.056, 0.056, 0, 0, 0, 0)
    rebate_checks <-
      ss_graph_wrapper(disbursed = projections$rebate_checks, # How much $ was actually disbursed
                       mps_name = "rebate_checks", # Which MPS to use
                       date = projections$date, # A vector of the dates used in the graph
                       start = "2019-01-01", # Graph start date
                       end = "2025-01-01", # Graph end date
                       title = "Rebate Checks") # Graph title
    print(rebate_checks$line)
    
    ### Federal Other Direct Aid ARP
    # CAUTION: This is another one where MPCs in the table don't match MPCs in the code.
    # Code MPCs:
    mpc_direct_aid_arp
    # implies MPCs are:
    # (0.14, 0.1, 0.1, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.03, 0.03, 0.03)
    # but the table
    mpc[which(mpc$variable == "other_direct_aid_arp"),]
    # implies MPCs are:
    # (0.14, 0.1, 0.1, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0, 0, 0)
    federal_other_direct_aid_arp <-
      ss_graph_wrapper(disbursed = projections$federal_other_direct_aid_arp, # How much $ was actually disbursed
                       mps_name = "other_direct_aid_arp", # Which MPS to use
                       date = projections$date, # A vector of the dates used in the graph
                       start = "2019-01-01", # Graph start date
                       end = "2025-01-01", # Graph end date
                       title = "Federal Other Direct Aid ARP") # Graph title
    print(federal_other_direct_aid_arp$line)
    
    ### Federal Student Loans
    # CAUTION: This is another one where MPCs in the table don't match MPCs in the code.
    federal_student_loans <-
      ss_graph_wrapper(disbursed = projections$federal_student_loans, # How much $ was actually disbursed
                       mps_name = "other_direct_aid_arp", # Which MPS to use
                       date = projections$date, # A vector of the dates used in the graph
                       start = "2019-01-01", # Graph start date
                       end = "2025-01-01", # Graph end date
                       title = "Federal Student Loans") # Graph title
    print(federal_student_loans$line)
    
    ### Federal Aid to Small Businesses ARP
    # The MPCs in the table do match the MPCs in the code
    federal_aid_to_small_businesses_arp <-
      ss_graph_wrapper(disbursed = projections$federal_aid_to_small_businesses_arp, # How much $ was actually disbursed
                       mps_name = "aid_to_small_businesses_arp", # Which MPS to use
                       date = projections$date, # A vector of the dates used in the graph
                       start = "2019-01-01", # Graph start date
                       end = "2025-01-01", # Graph end date
                       title = "Federal Aid to Small Businesses ARP") # Graph title
    print(federal_aid_to_small_businesses_arp$line)
    
    ### Federal UI
    # Needs a special way of calculating because we have pre- and post-COVID MPCs
    # How do we want to apply the UIs
      disbursed <- projections$federal_ui # How much $ was actually disbursed
      date <- projections$date # A vector of the dates used in the graph
      start <- "2019-01-01" # Graph start date
      end <- "2025-01-01" # Graph end date
      # Initialize the MPSs (here we have 2: one pre-Q2 2021 and one post)
      mps_1 <- c_mps[which(alt_mps$variable == "ui"),] %>%
        select(-variable) %>%
        unlist()
      mps_2 <- c_mps[which(alt_mps$variable == "ui_arp"),] %>%
        select(-variable) %>%
        unlist()
      # Get the terminal MPSs
      terminal_1 <- mps_1[length(mps_1)] # last value of the current mps vector
      terminal_2 <- mps_2[length(mps_2)] # last value of the current mps vector
      # Appending terminal rate to the mps vector N times to avoid error of cutting
      # off too early
      mps_1 <- mps_1 %>%
        c(., rep(terminal_1, times = length(date)))
      mps_2 <- mps_2 %>%
        c(., rep(terminal_2, times = length(date)))
      # Next, calculate the savings stream using mps_lorae
      ss <- mps_lorae(x = disbursed, 
                      mps = ifelse(date < yearquarter("2021 Q2"), mps_1, mps_2))
      # Finally, graph the savings stream and the disbursed funds using graph_mps
      federal_ui <- 
        graph_mps(disbursed = disbursed, # How much $ was actually disbursed
                          ss = ss, # Our best guesses on savings resulting from disbursement
                          date = date, # A vector of the dates used in the graph
                          start = start, # Graph start date
                          end = end, # Graph end date
                          title = "Federal UI", # Graph title
                          terminal = terminal_2) # Terminal MPS
      print(federal_ui$line)
      
      ### State UI
      # Needs a special way of calculating because we have pre- and post-COVID MPCs
      # How do we want to apply the UIs
      disbursed <- projections$state_ui # How much $ was actually disbursed
      date <- projections$date # A vector of the dates used in the graph
      start <- "2019-01-01" # Graph start date
      end <- "2025-01-01" # Graph end date
      # Initialize the MPSs (here we have 2: one pre-Q2 2021 and one post)
      mps_1 <- c_mps[which(alt_mps$variable == "ui"),] %>%
        select(-variable) %>%
        unlist()
      mps_2 <- c_mps[which(alt_mps$variable == "ui_arp"),] %>%
        select(-variable) %>%
        unlist()
      # Get the terminal MPSs
      terminal_1 <- mps_1[length(mps_1)] # last value of the current mps vector
      terminal_2 <- mps_2[length(mps_2)] # last value of the current mps vector
      # Appending terminal rate to the mps vector N times to avoid error of cutting
      # off too early
      mps_1 <- mps_1 %>%
        c(., rep(terminal_1, times = length(date)))
      mps_2 <- mps_2 %>%
        c(., rep(terminal_2, times = length(date)))
      # Next, calculate the savings stream using mps_lorae
      ss <- mps_lorae(x = disbursed, 
                      mps = ifelse(date < yearquarter("2021 Q2"), mps_1, mps_2))
      # Finally, graph the savings stream and the disbursed funds using graph_mps
      state_ui <- 
        graph_mps(disbursed = disbursed, # How much $ was actually disbursed
                          ss = ss, # Our best guesses on savings resulting from disbursement
                          date = date, # A vector of the dates used in the graph
                          start = start, # Graph start date
                          end = end, # Graph end date
                          title = "State UI", # Graph title
                          terminal = terminal_2) # Terminal MPS
      print(state_ui$line)
    
      ##### SUBPART E: Continue this output with aggregates #####
      ### Total cumulative disbursed and implied saving graph
      # list of dataframes
      df_list <- list(
        federal_other_vulnerable_arp = federal_other_vulnerable_arp$data,
        rebate_checks_arp = rebate_checks_arp$data,
        federal_other_direct_aid = federal_other_direct_aid_arp$data,
        federal_student_loans = federal_student_loans$data,
        federal_aid_to_small_businesses_arp = federal_aid_to_small_businesses_arp$data,
        federal_ui = federal_ui$data,
        #state_ui$data,#State is high before the pandemic, so we exclude it.
        rebate_checks = rebate_checks$data
      )
      # bind rows from all data frames, and group by Date and Dataset
      # then, summarise by summing the Value
      combined_df <- df_list %>%
        bind_rows() %>%
        group_by(Dataset, Date) %>%
        summarise(Value = sum(Value), .groups = "drop")
      
      # Initialize graph settings
      # Construct the graph title
      start <- "2019-01-01" # Graph start date
      end <- "2025-01-01"
      custom_title <- paste0("TOTAL", 
                             "\nDisbursement (using BLS data) versus Implied Saving (using MPC assumptions)")

      # Set the date range for display
      start_date <- as.Date(start) # first bar is 2019 Q1
      end_date <- as.Date(end) # last bar is 2024 Q4
      # Add an offset to the x-axis positions. This will align the bars so that 
      # the 2021 Q1 bar, for example, will sit to the right of the 2021 tick mark, 
      # rather than be aligned with the center of the tick mark (which is the default
      # result).
      x_offset <- 45
      
      line_df <- combined_df[!(combined_df$Dataset == "Disbursed"), ]
      # Generate bar chart using ggplot2
      total_disbursed_saving <- ggplot(line_df, aes(x = Date + x_offset, y = Value, color = Dataset)) + 
        geom_line(aes(group = Dataset), size = 1) +
        #geom_point(aes(shape = Dataset), size = 3) +
        scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = c(start_date, end_date)) +
        scale_y_continuous(label = scales::dollar_format(suffix = " B")) +
        #theme_classic() +
        theme(axis.text.x = element_text(angle = 0,
                                         vjust = 0.5,
                                         hjust = 0.5),
              panel.grid.major.y = element_line(color = "grey60", linetype = "dashed"),
              plot.caption = element_text(hjust = 0)) + 
        labs(title = "TOTAL Cumulative Disbursements and Savings",
             x = "", 
             y = "", 
             caption = "Sums combine the following categories: Federal Other Vulnerable ARP, Rebate Checks ARP, Federal Other Direct Aid ARP, Federal Student Loans, Federal Aid to Small 
             \nBusinesses ARP, Federal UI, Rebate Checks. Note that State UI is excluded because cumulative savings out of it were substantial even prior to the pandemic.") + 
        scale_color_manual(values = c("Cumulative Disbursed" = "blue", "Cumulative Implied Saving" = "orange")) +
        #scale_shape_manual(values = c("Disbursed" = 20, "Cumulative Implied Saving" = 20)) +
        guides(color = guide_legend(title = NULL), shape = guide_legend(title = NULL))
      
      print(total_disbursed_saving)
      
      ### Total cumulative implied saving line graph
      # bind rows from all data frames, and group by Date and Dataset
      # then, summarise by summing the Value
      
      # Initialize graph x-axis bounds
      start <- "2019-01-01" # Graph start date
      end <- "2025-01-01"
      
      # Set the date range for display
      start_date <- as.Date(start) # first bar is 2019 Q1
      end_date <- as.Date(end) # last bar is 2024 Q4
      # Add a new column to each data frame in the list indicating the source of the data.
      # Use lapply to apply a function to each data frame in the list.
      df_list_named <- lapply(names(df_list), function(x) {
        df_list[[x]] %>%
          filter(Dataset == "Cumulative Implied Saving") %>%
          mutate(Source = x)
      })
      
      # Combine all data frames into a single data frame
      df_combined <- bind_rows(df_list_named)
      
      # Now we can plot our data
      total_saving <- ggplot(df_combined, aes(x = Date, y = Value, fill = Source)) +
        geom_area() +
        scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = c(start_date, end_date)) +
        scale_y_continuous(label = scales::dollar_format(suffix = " B")) +
        labs(x = "Date",
             y = "Cumulative Implied Saving",
             caption = "Sums combine the following categories: Federal Other Vulnerable ARP, Rebate Checks ARP, Federal Other Direct Aid ARP, Federal Student Loans, Federal Aid to Small 
             \nBusinesses ARP, Federal UI, Rebate Checks. Note that State UI is excluded because cumulative savings out of it were substantial even prior to the pandemic.",
             fill = "Program",
             title = "Cumulative Implied Saving by Program Over Time") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")
      
      print(total_saving)
      
      ### Total cumulative disbursed line graph
      # bind rows from all data frames, and group by Date and Dataset
      # then, summarise by summing the Value
      
      # Initialize graph x-axis bounds
      start <- "2019-01-01" # Graph start date
      end <- "2025-01-01"
      
      # Set the date range for display
      start_date <- as.Date(start) # first bar is 2019 Q1
      end_date <- as.Date(end) # last bar is 2024 Q4
      # Add a new column to each data frame in the list indicating the source of the data.
      # Use lapply to apply a function to each data frame in the list.
      df_list_named <- lapply(names(df_list), function(x) {
        df_list[[x]] %>%
          filter(Dataset == "Cumulative Disbursed") %>%
          mutate(Source = x)
      })
      
      # Combine all data frames into a single data frame
      df_combined <- bind_rows(df_list_named)
      
      # Now we can plot our data
      total_disbursed <- ggplot(df_combined, aes(x = Date, y = Value, fill = Source)) +
        geom_area() +
        scale_x_date(date_labels = "%Y", date_breaks = "1 year", limits = c(start_date, end_date)) +
        scale_y_continuous(label = scales::dollar_format(suffix = " B")) +
        labs(x = "Date",
             y = "Cumulative Disbursed",
             caption = "Sums combine the following categories: Federal Other Vulnerable ARP, Rebate Checks ARP, Federal Other Direct Aid ARP, Federal Student Loans, Federal Aid to Small 
             \nBusinesses ARP, Federal UI, Rebate Checks. Note that State UI is excluded because cumulative savings out of it were substantial even prior to the pandemic.",
             fill = "Program",
             title = "Cumulative Disbursed by Program Over Time") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom")
      
      print(total_disbursed)
      
      # Close the device driver
      dev.off()

