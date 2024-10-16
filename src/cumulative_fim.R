# cumulative_fim.R
#
# This script determines cumulative contributions of the FIM and its components as an annualized share of GDP 
# between any two quarters. It calculates cumulative product from the start period,
# generates contributions, and calculates cumuluative contributions. 
# 
# You must run fiscal_impact_BETA.R before running this script.
# Last Updated: 10/7/2024

# Load packages - purr is not used in fiscal_impact_BETA.R
packages <- c(
  "tidyverse", "tsibble", "lubridate", "glue", 
  "TimTeaFan/dplyover", "zoo", "TTR", "fs", "gt", "purr",
  "openxlsx", "snakecase", "rlang", "BrookingsInstitution/ggbrookings"
)

librarian::shelf(packages)

###################
# DEFINE QUARTERS #
###################

# Define and set start and end quarter. The start_quarter and end_quarter
# variables can be modified to determine cumulative contributions of the FIM 
# from any two quarters. 
start_quarter <- yearquarter("2020 Q1")
  start_position <- which(usna$date == start_quarter)
end_quarter <- yearquarter("2026 Q2")
  end_position <- which(usna$date == end_quarter) 
base_quarter <- start_quarter - 1 
  base_position <- which(usna$date == base_quarter) 
  
######################################
# DEFINE CUMULATIVE PRODUCT FUNCTION #
######################################
  
# Generate "cumprod_growth" function. This function takes a deflator data series 
# as its input. It gets the cumulative product of (1+rpgg+dg) beginning in 
# the start_quarter using rpgg from real_potential_gdp_growth_test. 
cumprod_growth <- 
    function(deflator){
      data.frame(real_potential_gdp_growth_test, deflator) %>% 
        mutate(
          sum = ifelse(date >= start_quarter, data_series.1 + data_series, 0), # sum the growth rates beginning in start_quarter, otherwise 0 
          cumprod = ifelse(date >= start_quarter, cumprod(1 + sum) - 1, NA) # get the cumulative product of the summed growth rates starting in start_quarter 
        ) %>%
         select(
           cumprod # keep only the cumulative product 
      )
    }

# Calculate the cumulative product of the growth rates for each of the deflators.

# Consumption Deflator 
cumprod_growth_consumption <- cumprod_growth(consumption_deflator_growth_test)

# Federal Purchases Deflator 
cumprod_growth_federal_purchases <- cumprod_growth(federal_purchases_deflator_growth_test)

# Consumption Grants Deflator 
cumprod_growth_consumption_grants <- cumprod_growth(consumption_grants_deflator_growth_test)

# Investment Grants Deflator 
cumprod_growth_investment_grants <- cumprod_growth(investment_grants_deflator_growth_test)

# State Purchases Deflator 
cumprod_growth_state_purchases <- cumprod_growth(state_purchases_deflator_growth_test)
  

#################################
# DEFINE CONTRIBUTION FUNCTIONS #
#################################

# This section of code generates 4 functions used to calculate the FIM contributions and accumulate them over time. 
# See src/contributions.R for functions used in the FIM 

# Define Minus Neutral Function 
minus_neutral_cumulative <- function(x, #the data in question 
                          cumprod #cumulative product of deflator and real potential gdp growth 
                                  #calculated from cumprod function defined in previous function 
) {
  output <- x - x[base_position] * (cumprod+1)
  return(output)
}

# Define MPC Function
mpc_cumulative <- function(x, mpc_matrix) {
  if (nrow(mpc_matrix) != length(x)) {
    stop("The number of rows in the mpc_matrix must equal the length of the series.")
  }
  vert_x <- matrix(x, ncol = 1)
  vert_x[is.na(vert_x)] <- 0
  output <- mpc_matrix %*% vert_x
  return(output)
}

# Define Scale to GDP Function 
scale_to_gdp_cumulative <- function(x, gdp) {
  output = x / gdp[base_position] #quarterly adjustment to GDP (does not include 400 in the product)
  return(output)
}

# Define Wrapper Function 
contribution_cumulative <- function(x, mpc_matrix = NULL, cumprod, gdp) {
  # Apply MPC Function
  if (!is.null(mpc_matrix)) {
    x <- x %>%
      mpc_cumulative(x = ., mpc_matrix = mpc_matrix)
  }
  
  # Apply the minus_neutral function to x
  result <- x %>%
    minus_neutral_cumulative(x = ., cumprod = cumprod)
  
  # Apply the scale_to_gdp function
  result %>%
    scale_to_gdp_cumulative(x = ., gdp = gdp)
}

#############################
# DEFINE ANNUALIZE FUNCTION #
#############################

# Generate "annualize" function which calculates cumulative contribution at an annualized rate.
annualize <- function(v #the data 
                  ) {
  # Capture the name of the variable passed as 'v'
  v_name <- deparse(substitute(v))
  
  data <- data.frame(v) %>%
    mutate(
      x = row_number(),
      quarters_since_start = x - base_position
    ) %>%
    mutate(
      data_series = 100 * ((1 + v)^(4 / quarters_since_start) - 1) # calculate cumulative FIM at an annualized rate
    ) %>%
    select(
      -x,
      -quarters_since_start,
      -v
    ) %>%
    rename(!!v_name := data_series)  #use !! to unquote and assign the name to the column
                                     #now each column name is the same as the contribution name
  return(data)
}


##############################################
# GET CUMULATIVE CONTRIBUTIONS AND ANNUALIZE #
##############################################

# First, apply the contribution_cumulative wrapper function, which gets each of the FIM contributions relative to the base quarter.
# Second, apply the annualize function, which gets these values at an annual rate. 
# Different cumprod inputs are used for different deflators. 
# Contributions are then aggregated by category and summed for the FIM contribution.

# Federal Purchases 
federal_purchases_contribution_cumulative <- contribution_cumulative(
  x = federal_purchases_test$data_series, 
  mpc_matrix = NULL,
  cumprod = cumprod_growth_federal_purchases$cumprod,
  gdp = gdp_test$data_series 
)

federal_purchases_cumulative <- annualize(federal_purchases_contribution_cumulative)

# Consumption Grants
consumption_grants_contribution_cumulative <- contribution_cumulative(
  x = consumption_grants_test$data_series,
  mpc_matrix = NULL,
  cumprod = cumprod_growth_consumption_grants$cumprod,
  gdp = gdp_test$data_series 
)

consumption_grants_cumulative <- annualize(consumption_grants_contribution_cumulative)

# Investment Grants 
investment_grants_contribution_cumulative <- contribution_cumulative(
  x = investment_grants_test$data_series, 
  mpc_matrix = NULL,
  cumprod = cumprod_growth_investment_grants$cumprod,
  gdp = gdp_test$data_series 
)

investment_grants_cumulative <- annualize(investment_grants_contribution_cumulative)

# State Purchases
state_purchases_contribution_cumulative <- contribution_cumulative(
  x = state_purchases_test$data_series, 
  mpc_matrix = NULL,
  cumprod = cumprod_growth_state_purchases$cumprod, 
  gdp = gdp_test$data_series 
)

state_purchases_cumulative <- annualize(state_purchases_contribution_cumulative) 

# Federal Non Corporate Taxes
federal_non_corporate_taxes_contribution_cumulative <- contribution_cumulative(
  x = federal_non_corporate_taxes_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_non_corporate_taxes.rds"), 
  cumprod = cumprod_growth_consumption$cumprod, 
  gdp = gdp_test$data_series 
)

federal_non_corporate_taxes_cumulative <- annualize(federal_non_corporate_taxes_contribution_cumulative)

# State Non Corporate Taxes
state_non_corporate_taxes_contribution_cumulative <- contribution_cumulative(
  x = state_non_corporate_taxes_test$data_series,  
  mpc_matrix = readRDS("cache/mpc_matrices/state_non_corporate_taxes.rds"), 
  cumprod = cumprod_growth_consumption$cumprod, 
  gdp = gdp_test$data_series
)

state_non_corporate_taxes_cumulative <- annualize(state_non_corporate_taxes_contribution_cumulative)

# Federal Corporate Taxes
federal_corporate_taxes_contribution_cumulative <- contribution_cumulative(
  x = federal_corporate_taxes_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_corporate_taxes.rds"), 
  cumprod = cumprod_growth_consumption$cumprod, 
  gdp = gdp_test$data_series 
)

federal_corporate_taxes_cumulative <- annualize(federal_corporate_taxes_contribution_cumulative)

# Supply Side IRA 
supply_side_ira_contribution_cumulative <- contribution_cumulative(
  x = supply_side_ira_test$data_series, 
  mpc_matrix = NULL,
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series
)

supply_side_ira_cumulative <- annualize(supply_side_ira_contribution_cumulative)

# State Corporate Taxes
state_corporate_taxes_contribution_cumulative <- contribution_cumulative(
  x = state_corporate_taxes_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/state_corporate_taxes.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

state_corporate_taxes_cumulative <- annualize(state_corporate_taxes_contribution_cumulative)

# Federal Social Benefits
federal_social_benefits_contribution_cumulative <- contribution_cumulative(
  x = federal_social_benefits_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_social_benefits.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series
)

federal_social_benefits_cumulative <- annualize(federal_social_benefits_contribution_cumulative)

# State Social Benefits
state_social_benefits_contribution_cumulative <- contribution_cumulative(
  x = state_social_benefits_test$data_series,
  mpc_matrix = readRDS("cache/mpc_matrices/state_social_benefits.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

state_social_benefits_cumulative <- annualize(state_social_benefits_contribution_cumulative)

# Rebate Checks
rebate_checks_contribution_cumulative <- contribution_cumulative(
  x = rebate_checks_test$data_series,
  mpc_matrix = readRDS("cache/mpc_matrices/rebate_checks.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series
)

rebate_checks_cumulative <- annualize(rebate_checks_contribution_cumulative)

# Rebate Checks ARP
rebate_checks_arp_contribution_cumulative <- contribution_cumulative(
  x = rebate_checks_arp_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/rebate_checks_arp.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series
)

rebate_checks_arp_cumulative <- annualize(rebate_checks_arp_contribution_cumulative)

# Federal UI
federal_ui_contribution_cumulative <- contribution_cumulative(
  x = federal_ui_test$data_series,  
  mpc_matrix = readRDS("cache/mpc_matrices/federal_ui.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_ui_cumulative <- annualize(federal_ui_contribution_cumulative)

# State UI
state_ui_contribution_cumulative <- contribution_cumulative(
  x = state_ui_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/state_ui.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

state_ui_cumulative <- annualize(state_ui_contribution_cumulative)

# Federal Subsidies
federal_subsidies_contribution_cumulative <- contribution_cumulative(
  x = federal_subsidies_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_subsidies.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_subsidies_cumulative <- annualize(federal_subsidies_contribution_cumulative)

# Federal Aid to Small Businesses ARP
federal_aid_to_small_businesses_arp_contribution_cumulative <- contribution_cumulative(
  x = federal_aid_to_small_businesses_arp_test$data_series,  
  mpc_matrix = readRDS("cache/mpc_matrices/federal_aid_to_small_businesses_arp.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_aid_to_small_businesses_arp_cumulative <- annualize(federal_aid_to_small_businesses_arp_contribution_cumulative)

# Federal Other Direct Aid ARP
federal_other_direct_aid_arp_contribution_cumulative <- contribution_cumulative(
  x = federal_other_direct_aid_arp_test$data_series,  
  mpc_matrix = readRDS("cache/mpc_matrices/federal_other_direct_aid_arp.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_other_direct_aid_arp_cumulative <- annualize(federal_other_direct_aid_arp_contribution_cumulative)

# Federal Other Vulnerable ARP
federal_other_vulnerable_arp_contribution_cumulative <- contribution_cumulative(
  x = federal_other_vulnerable_arp_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_other_vulnerable_arp.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_other_vulnerable_arp_cumulative <- annualize(federal_other_vulnerable_arp_contribution_cumulative)

# Federal Student Loans
federal_student_loans_contribution_cumulative <- contribution_cumulative(
  x = federal_student_loans_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_student_loans.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_student_loans_cumulative <- annualize(federal_student_loans_contribution_cumulative)

# State Subsidies
state_subsidies_contribution_cumulative <- contribution_cumulative(
  x = state_subsidies_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/state_subsidies.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

state_subsidies_cumulative <- annualize(state_subsidies_contribution_cumulative)

# Federal Health Outlays
federal_health_outlays_contribution_cumulative <- contribution_cumulative(
  x = federal_health_outlays_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_health_outlays.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_health_outlays_cumulative <- annualize(federal_health_outlays_contribution_cumulative)

# State Health Outlays
state_health_outlays_contribution_cumulative <- contribution_cumulative(
  x = state_health_outlays_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/state_health_outlays.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series
)

state_health_outlays_cumulative <- annualize(state_health_outlays_contribution_cumulative)

# Aggregate contribution cumulative by category
federal_cumulative <- 
  (federal_purchases_cumulative$federal_purchases_contribution_cumulative + 
     consumption_grants_cumulative$consumption_grants_contribution_cumulative + 
     investment_grants_cumulative$investment_grants_contribution_cumulative) 

state_cumulative <- 
  (state_purchases_cumulative$state_purchases_contribution_cumulative - 
     consumption_grants_cumulative$consumption_grants_contribution_cumulative - 
     investment_grants_cumulative$investment_grants_contribution_cumulative)

taxes_cumulative <-
  (federal_non_corporate_taxes_cumulative$federal_non_corporate_taxes_contribution_cumulative + 
     state_non_corporate_taxes_cumulative$state_non_corporate_taxes_contribution_cumulative + 
     federal_corporate_taxes_cumulative$federal_corporate_taxes_contribution_cumulative + 
     supply_side_ira_cumulative$supply_side_ira_contribution_cumulative + 
     state_corporate_taxes_cumulative$state_corporate_taxes_contribution_cumulative) 

transfers_cumulative <-
  (federal_social_benefits_cumulative$federal_social_benefits_contribution_cumulative + 
     state_social_benefits_cumulative$state_social_benefits_contribution_cumulative + 
     rebate_checks_cumulative$rebate_checks_contribution_cumulative + 
     rebate_checks_arp_cumulative$rebate_checks_arp_contribution_cumulative + 
     federal_ui_cumulative$federal_ui_contribution_cumulative + 
     state_ui_cumulative$state_ui_contribution_cumulative + 
     federal_subsidies_cumulative$federal_subsidies_contribution_cumulative + 
     federal_aid_to_small_businesses_arp_cumulative$federal_aid_to_small_businesses_arp_contribution_cumulative + 
     federal_other_direct_aid_arp_cumulative$federal_other_direct_aid_arp_contribution_cumulative + 
     federal_other_vulnerable_arp_cumulative$federal_other_vulnerable_arp_contribution_cumulative + 
     federal_student_loans_cumulative$federal_student_loans_contribution_cumulative + 
     state_subsidies_cumulative$state_subsidies_contribution_cumulative + 
     federal_health_outlays_cumulative$federal_health_outlays_contribution_cumulative + 
     state_health_outlays_cumulative$state_health_outlays_contribution_cumulative) 

consumption_cumulative <-
  (taxes_cumulative +
     transfers_cumulative)

fiscal_impact_measure_cumulative <-
  (federal_cumulative +
     state_cumulative +
     taxes_cumulative +
     transfers_cumulative)

###################################
# CLEAN AND OUTPUT CUMULATIVE FIM #
###################################

# Assign date column  
date <- data.frame(usna$date) %>% 
  rename(date = usna.date)

# Levels ------------------

# Combine all levels into a data frame 
cumulative_df <- cbind(
  date,
  federal_purchases_cumulative,
  consumption_grants_cumulative,
  investment_grants_cumulative,
  state_purchases_cumulative,
  federal_non_corporate_taxes_cumulative,
  state_non_corporate_taxes_cumulative,
  federal_corporate_taxes_cumulative,
  supply_side_ira_cumulative,
  state_corporate_taxes_cumulative,
  federal_social_benefits_cumulative,
  state_social_benefits_cumulative,
  rebate_checks_cumulative,
  rebate_checks_arp_cumulative,
  federal_ui_cumulative,
  state_ui_cumulative,
  federal_subsidies_cumulative,
  federal_aid_to_small_businesses_arp_cumulative,
  federal_other_direct_aid_arp_cumulative,
  federal_other_vulnerable_arp_cumulative,
  federal_student_loans_cumulative,
  state_subsidies_cumulative,
  federal_health_outlays_cumulative,
  state_health_outlays_cumulative,
  federal_cumulative,
  state_cumulative,
  taxes_cumulative,
  transfers_cumulative,
  consumption_cumulative,
  fiscal_impact_measure_cumulative
)

# Quarterly ---------------

# Identify columns with "levels" in their name
cols_to_transform <- grep("cumulative", names(cumulative_df), value = TRUE)

# Replace old "levels" with updated calculations for the "quarterly" columns
cumulative_quarterly_df <- cumulative_df %>%
  bind_cols(map_dfc(cols_to_transform, ~ (((1 + (cumulative_df[[.x]] / 100)) ^ 0.25) - 1) * 100) %>%
              setNames(gsub("cumulative", "quarterly", cols_to_transform))) %>%
  select(date, contains("quarterly"))

# Sum ---------------------

# Filter data between the start_quarter and end_quarter
filtered_data <- cumulative_quarterly_df %>%
  filter(date >= start_quarter & date <= end_quarter)

# Sum across quarterly levels to calculate cumulative contribution from start
# quarter to end quarter
cumulative_sum_df <- filtered_data %>%
  summarize(across(-date, sum, na.rm = TRUE, .names = "{.col}_sum")) %>%
  mutate(period = paste(as.character(start_quarter), "to", as.character(end_quarter))) %>%
  select(period, everything())

# OUTPUT ------------------

# Output each data frame as a sheet in the Excel worksheet
sheets <- list("Cumulative FIM, Annualized" = cumulative_df, 
               "Cumulative FIM, Quarterly" = cumulative_quarterly_df, 
               "Sum" = cumulative_sum_df)

openxlsx::write.xlsx(sheets, file = glue('cumulative_fim.xlsx', overwrite = TRUE))
