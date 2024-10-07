# levels.R
#
# This script determines cumulative contributions of the FIM and its components
# between any two quarters. It calculates cumulative product from the start period,
# generates contributions, and calculates cumuluative contributions.
# 
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
start_quarter <- yearquarter("2022 Q3")
  start_position <- which(usna$date == start_quarter)
end_quarter <- yearquarter("2024 Q2")
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

# This section of code generates 4 functions used to calculate the FIM contributions. 
# Note: there are differences from the FIM code in the minus_neutral and scale_to_gdp_levels functions.
# See src/contributions.R for functions used in the FIM 

# Define Minus Neutral Function 
minus_neutral_levels <- function(x, #the data in question 
                          cumprod #cumulative product of deflator and real potential gdp growth 
                                  #calculated from cumprod function defined in previous function 
) {
  output <- x - x[base_position] * (cumprod+1)
  return(output)
}

# Define MPC Function
mpc_levels <- function(x, mpc_matrix) {
  if (nrow(mpc_matrix) != length(x)) {
    stop("The number of rows in the mpc_matrix must equal the length of the series.")
  }
  vert_x <- matrix(x, ncol = 1)
  vert_x[is.na(vert_x)] <- 0
  output <- mpc_matrix %*% vert_x
  return(output)
}

# Define Scale to GDP Function 
scale_to_gdp_levels <- function(x, gdp) {
  output = x / gdp[base_position] #quarterly adjustment to GDP (does not include 400 in the product)
  return(output)
}

# Define Wrapper Function 
contribution_levels <- function(x, mpc_matrix = NULL, cumprod, gdp) {
  # Apply MPC Function
  if (!is.null(mpc_matrix)) {
    x <- x %>%
      mpc_levels(x = ., mpc_matrix = mpc_matrix)
  }
  
  # Apply the minus_neutral function to x
  result <- x %>%
    minus_neutral_levels(x = ., cumprod = cumprod)
  
  # Apply the scale_to_gdp function
  result %>%
    scale_to_gdp_levels(x = ., gdp = gdp)
}

#########################
# DEFINE LEVEL FUNCTION #
#########################

# Generate "level" function which calculates cumulative contribution levels.
level <- function(v #the contribution levels
                  ) {
  # Capture the name of the variable passed as 'v'
  v_name <- deparse(substitute(v))
  
  data <- data.frame(v) %>%
    mutate(
      x = row_number(),
      quarters_since_start = x - base_position
    ) %>%
    mutate(
      data_series = 100 * ((1 + v)^(4 / quarters_since_start) - 1) #calculate quarterly cumulative levels
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


################################
# GET CONTRIBUTIONS AND LEVELS #
################################

# First, apply the contribution_levels wrapper function and then calculative 
# cumulative contributions for each quarter for each FIM contribution.
# Different cumprod inputs are used for different deflators. 
# Contributions are then aggregated by category and summed for the FIM contribution.

# Federal Purchases 
federal_purchases_contribution_levels <- contribution_levels(
  x = federal_purchases_test$data_series, 
  mpc_matrix = NULL,
  cumprod = cumprod_growth_federal_purchases$cumprod,
  gdp = gdp_test$data_series 
)

federal_purchases_levels <- level(federal_purchases_contribution_levels)

# Consumption Grants
consumption_grants_contribution_levels <- contribution_levels(
  x = consumption_grants_test$data_series,
  mpc_matrix = NULL,
  cumprod = cumprod_growth_consumption_grants$cumprod,
  gdp = gdp_test$data_series 
)

consumption_grants_levels <- level(consumption_grants_contribution_levels)

# Investment Grants 
investment_grants_contribution_levels <- contribution_levels(
  x = investment_grants_test$data_series, 
  mpc_matrix = NULL,
  cumprod = cumprod_growth_investment_grants$cumprod,
  gdp = gdp_test$data_series 
)

investment_grants_levels <- level(investment_grants_contribution_levels)

# State Purchases
state_purchases_contribution_levels <- contribution_levels(
  x = state_purchases_test$data_series, 
  mpc_matrix = NULL,
  cumprod = cumprod_growth_state_purchases$cumprod, 
  gdp = gdp_test$data_series 
)

state_purchases_levels <- level(state_purchases_contribution_levels) 

# Federal Non Corporate Taxes
federal_non_corporate_taxes_contribution_levels <- contribution_levels(
  x = federal_non_corporate_taxes_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_non_corporate_taxes.rds"), 
  cumprod = cumprod_growth_consumption$cumprod, 
  gdp = gdp_test$data_series 
)

federal_non_corporate_taxes_levels <- level(federal_non_corporate_taxes_contribution_levels)

# State Non Corporate Taxes
state_non_corporate_taxes_contribution_levels <- contribution_levels(
  x = state_non_corporate_taxes_test$data_series,  
  mpc_matrix = readRDS("cache/mpc_matrices/state_non_corporate_taxes.rds"), 
  cumprod = cumprod_growth_consumption$cumprod, 
  gdp = gdp_test$data_series
)

state_non_corporate_taxes_levels <- level(state_non_corporate_taxes_contribution_levels)

# Federal Corporate Taxes
federal_corporate_taxes_contribution_levels <- contribution_levels(
  x = federal_corporate_taxes_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_corporate_taxes.rds"), 
  cumprod = cumprod_growth_consumption$cumprod, 
  gdp = gdp_test$data_series 
)

federal_corporate_taxes_levels <- level(federal_corporate_taxes_contribution_levels)

# Supply Side IRA 
supply_side_ira_contribution_levels <- contribution_levels(
  x = supply_side_ira_test$data_series, 
  mpc_matrix = NULL,
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series
)

supply_side_ira_levels <- level(supply_side_ira_contribution_levels)

# State Corporate Taxes
state_corporate_taxes_contribution_levels <- contribution_levels(
  x = state_corporate_taxes_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/state_corporate_taxes.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

state_corporate_taxes_levels <- level(state_corporate_taxes_contribution_levels)

# Federal Social Benefits
federal_social_benefits_contribution_levels <- contribution_levels(
  x = federal_social_benefits_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_social_benefits.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series
)

federal_social_benefits_levels <- level(federal_social_benefits_contribution_levels)

# State Social Benefits
state_social_benefits_contribution_levels <- contribution_levels(
  x = state_social_benefits_test$data_series,
  mpc_matrix = readRDS("cache/mpc_matrices/state_social_benefits.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

state_social_benefits_levels <- level(state_social_benefits_contribution_levels)

# Rebate Checks
rebate_checks_contribution_levels <- contribution_levels(
  x = rebate_checks_test$data_series,
  mpc_matrix = readRDS("cache/mpc_matrices/rebate_checks.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series
)

rebate_checks_levels <- level(rebate_checks_contribution_levels)

# Rebate Checks ARP
rebate_checks_arp_contribution_levels <- contribution_levels(
  x = rebate_checks_arp_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/rebate_checks_arp.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series
)

rebate_checks_arp_levels <- level(rebate_checks_arp_contribution_levels)

# Federal UI
federal_ui_contribution_levels <- contribution_levels(
  x = federal_ui_test$data_series,  
  mpc_matrix = readRDS("cache/mpc_matrices/federal_ui.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_ui_levels <- level(federal_ui_contribution_levels)

# State UI
state_ui_contribution_levels <- contribution_levels(
  x = state_ui_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/state_ui.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

state_ui_levels <- level(state_ui_contribution_levels)

# Federal Subsidies
federal_subsidies_contribution_levels <- contribution_levels(
  x = federal_subsidies_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_subsidies.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_subsidies_levels <- level(federal_subsidies_contribution_levels)

# Federal Aid to Small Businesses ARP
federal_aid_to_small_businesses_arp_contribution_levels <- contribution_levels(
  x = federal_aid_to_small_businesses_arp_test$data_series,  
  mpc_matrix = readRDS("cache/mpc_matrices/federal_aid_to_small_businesses_arp.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_aid_to_small_businesses_arp_levels <- level(federal_aid_to_small_businesses_arp_contribution_levels)

# Federal Other Direct Aid ARP
federal_other_direct_aid_arp_contribution_levels <- contribution_levels(
  x = federal_other_direct_aid_arp_test$data_series,  
  mpc_matrix = readRDS("cache/mpc_matrices/federal_other_direct_aid_arp.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_other_direct_aid_arp_levels <- level(federal_other_direct_aid_arp_contribution_levels)

# Federal Other Vulnerable ARP
federal_other_vulnerable_arp_contribution_levels <- contribution_levels(
  x = federal_other_vulnerable_arp_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_other_vulnerable_arp.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_other_vulnerable_arp_levels <- level(federal_other_vulnerable_arp_contribution_levels)

# Federal Student Loans
federal_student_loans_contribution_levels <- contribution_levels(
  x = federal_student_loans_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_student_loans.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_student_loans_levels <- level(federal_student_loans_contribution_levels)

# State Subsidies
state_subsidies_contribution_levels <- contribution_levels(
  x = state_subsidies_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/state_subsidies.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

state_subsidies_levels <- level(state_subsidies_contribution_levels)

# Federal Health Outlays
federal_health_outlays_contribution_levels <- contribution_levels(
  x = federal_health_outlays_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_health_outlays.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_health_outlays_levels <- level(federal_health_outlays_contribution_levels)

# State Health Outlays
state_health_outlays_contribution_levels <- contribution_levels(
  x = state_health_outlays_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/state_health_outlays.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series
)

state_health_outlays_levels <- level(state_health_outlays_contribution_levels)

# Aggregate contribution levels by category
federal_levels <- 
  (federal_purchases_levels$federal_purchases_contribution_levels + 
     consumption_grants_levels$consumption_grants_contribution_levels + 
     investment_grants_levels$investment_grants_contribution_levels) 

state_levels <- 
  (state_purchases_levels$state_purchases_contribution_levels - 
     consumption_grants_levels$consumption_grants_contribution_levels - 
     investment_grants_levels$investment_grants_contribution_levels)

taxes_levels <-
  (federal_non_corporate_taxes_levels$federal_non_corporate_taxes_contribution_levels + 
     state_non_corporate_taxes_levels$state_non_corporate_taxes_contribution_levels + 
     federal_corporate_taxes_levels$federal_corporate_taxes_contribution_levels + 
     supply_side_ira_levels$supply_side_ira_contribution_levels + 
     state_corporate_taxes_levels$state_corporate_taxes_contribution_levels) 

transfers_levels <-
  (federal_social_benefits_levels$federal_social_benefits_contribution_levels + 
     state_social_benefits_levels$state_social_benefits_contribution_levels + 
     rebate_checks_levels$rebate_checks_contribution_levels + 
     rebate_checks_arp_levels$rebate_checks_arp_contribution_levels + 
     federal_ui_levels$federal_ui_contribution_levels + 
     state_ui_levels$state_ui_contribution_levels + 
     federal_subsidies_levels$federal_subsidies_contribution_levels + 
     federal_aid_to_small_businesses_arp_levels$federal_aid_to_small_businesses_arp_contribution_levels + 
     federal_other_direct_aid_arp_levels$federal_other_direct_aid_arp_contribution_levels + 
     federal_other_vulnerable_arp_levels$federal_other_vulnerable_arp_contribution_levels + 
     federal_student_loans_levels$federal_student_loans_contribution_levels + 
     state_subsidies_levels$state_subsidies_contribution_levels + 
     federal_health_outlays_levels$federal_health_outlays_contribution_levels + 
     state_health_outlays_levels$state_health_outlays_contribution_levels) 

consumption_levels <-
  (taxes_levels +
     transfers_levels)

fiscal_impact_measure_levels <-
  (federal_levels +
     state_levels +
     taxes_levels +
     transfers_levels)

###########################
# CLEAN AND OUTPUT LEVELS #
###########################

# Assign date column  
date <- data.frame(usna$date) %>% 
  rename(date = usna.date)

# Levels ------------------

# Combine all levels into a data frame 
levels_df <- cbind(
  date,
  federal_purchases_levels,
  consumption_grants_levels,
  investment_grants_levels,
  state_purchases_levels,
  federal_non_corporate_taxes_levels,
  state_non_corporate_taxes_levels,
  federal_corporate_taxes_levels,
  supply_side_ira_levels,
  state_corporate_taxes_levels,
  federal_social_benefits_levels,
  state_social_benefits_levels,
  rebate_checks_levels,
  rebate_checks_arp_levels,
  federal_ui_levels,
  state_ui_levels,
  federal_subsidies_levels,
  federal_aid_to_small_businesses_arp_levels,
  federal_other_direct_aid_arp_levels,
  federal_other_vulnerable_arp_levels,
  federal_student_loans_levels,
  state_subsidies_levels,
  federal_health_outlays_levels,
  state_health_outlays_levels,
  federal_levels,
  state_levels,
  taxes_levels,
  transfers_levels,
  consumption_levels,
  fiscal_impact_measure_levels
)

# Quarterly ---------------

# Identify columns with "levels" in their name
cols_to_transform <- grep("levels", names(levels_df), value = TRUE)

# Replace old "levels" with updated calculations for the "quarterly" columns
levels_quarterly_df <- levels_df %>%
  bind_cols(map_dfc(cols_to_transform, ~ (((1 + (levels_df[[.x]] / 100)) ^ 0.25) - 1) * 100) %>%
              setNames(gsub("levels", "quarterly", cols_to_transform))) %>%
  select(date, contains("quarterly"))

# Sum ---------------------

# Filter data between the start_quarter and end_quarter
filtered_data <- levels_quarterly_df %>%
  filter(date >= start_quarter & date <= end_quarter)

# Sum across quarterly levels to calculate cumulative contribution from start
# quarter to end quarter
levels_sum_df <- filtered_data %>%
  summarize(across(-date, sum, na.rm = TRUE, .names = "{.col}_sum")) %>%
  mutate(period = paste(as.character(start_quarter), "to", as.character(end_quarter))) %>%
  select(period, everything())

# OUTPUT ------------------

# Output each data frame as a sheet in the Excel worksheet
sheets <- list("Contribution Levels" = levels_df, 
               "Quarterly Levels" = levels_quarterly_df, 
               "Sum" = levels_sum_df)

openxlsx::write.xlsx(sheets, file = glue('fim_levels.xlsx', overwrite = TRUE))
