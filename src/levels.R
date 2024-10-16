# levels.R
#
# 
# Last Updated: 10/15/2024

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

# Define Minus Neutral Function 
counterfactual_levels <- function(x, #the data in question 
                                 cumprod #cumulative product of deflator and real potential gdp growth 
                                 #calculated from cumprod function defined in previous function 
) {
  output <- x- x[base_position] * (cumprod+1)
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

# Define Wrapper Function 
contribution_levels <- function(x, mpc_matrix = NULL, cumprod) {
  # Apply MPC Function
  if (!is.null(mpc_matrix)) {
    x <- x %>%
      mpc_levels(x = ., mpc_matrix = mpc_matrix)
  }
  
  # Apply the minus_neutral function to x
  result <- x %>%
    counterfactual_levels(x = ., cumprod = cumprod)
}

################################
# GET CONTRIBUTIONS AND LEVELS #
################################

# Federal Purchases 
federal_purchases_contribution_levels <- contribution_levels(
  x = federal_purchases_test$data_series, 
  mpc_matrix = NULL,
  cumprod = cumprod_growth_federal_purchases$cumprod
)

view(federal_purchases_contribution_levels)


# Consumption Grants
consumption_grants_contribution_levels <- contribution_levels(
  x = consumption_grants_test$data_series,
  mpc_matrix = NULL,
  cumprod = cumprod_growth_consumption_grants$cumprod
)

# Investment Grants 
investment_grants_contribution_levels <- contribution_levels(
  x = investment_grants_test$data_series, 
  mpc_matrix = NULL,
  cumprod = cumprod_growth_investment_grants$cumprod
)

# State Purchases
state_purchases_contribution_levels <- contribution_levels(
  x = state_purchases_test$data_series, 
  mpc_matrix = NULL,
  cumprod = cumprod_growth_state_purchases$cumprod
)

# Federal Non Corporate Taxes
federal_non_corporate_taxes_contribution_levels <- contribution_levels(
  x = federal_non_corporate_taxes_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_non_corporate_taxes.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)

# State Non Corporate Taxes
state_non_corporate_taxes_contribution_levels <- contribution_levels(
  x = state_non_corporate_taxes_test$data_series,  
  mpc_matrix = readRDS("cache/mpc_matrices/state_non_corporate_taxes.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)

# Federal Corporate Taxes
federal_corporate_taxes_contribution_levels <- contribution_levels(
  x = federal_corporate_taxes_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_corporate_taxes.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)

# Supply Side IRA 
supply_side_ira_contribution_levels <- contribution_levels(
  x = supply_side_ira_test$data_series, 
  mpc_matrix = NULL,
  cumprod = cumprod_growth_consumption$cumprod
)

# State Corporate Taxes
state_corporate_taxes_contribution_levels <- contribution_levels(
  x = state_corporate_taxes_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/state_corporate_taxes.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)

# Federal Social Benefits
federal_social_benefits_contribution_levels <- contribution_levels(
  x = federal_social_benefits_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_social_benefits.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)

# State Social Benefits
state_social_benefits_contribution_levels <- contribution_levels(
  x = state_social_benefits_test$data_series,
  mpc_matrix = readRDS("cache/mpc_matrices/state_social_benefits.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)

# Rebate Checks
rebate_checks_contribution_levels <- contribution_levels(
  x = rebate_checks_test$data_series,
  mpc_matrix = readRDS("cache/mpc_matrices/rebate_checks.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)

# Rebate Checks ARP
rebate_checks_arp_contribution_levels <- contribution_levels(
  x = rebate_checks_arp_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/rebate_checks_arp.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)

# Federal UI
federal_ui_contribution_levels <- contribution_levels(
  x = federal_ui_test$data_series,  
  mpc_matrix = readRDS("cache/mpc_matrices/federal_ui.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)

# State UI
state_ui_contribution_levels <- contribution_levels(
  x = state_ui_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/state_ui.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)


# Federal Subsidies
federal_subsidies_contribution_levels <- contribution_levels(
  x = federal_subsidies_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_subsidies.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)

# Federal Aid to Small Businesses ARP
federal_aid_to_small_businesses_arp_contribution_levels <- contribution_levels(
  x = federal_aid_to_small_businesses_arp_test$data_series,  
  mpc_matrix = readRDS("cache/mpc_matrices/federal_aid_to_small_businesses_arp.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)

# Federal Other Direct Aid ARP
federal_other_direct_aid_arp_contribution_levels <- contribution_levels(
  x = federal_other_direct_aid_arp_test$data_series,  
  mpc_matrix = readRDS("cache/mpc_matrices/federal_other_direct_aid_arp.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)

# Federal Other Vulnerable ARP
federal_other_vulnerable_arp_contribution_levels <- contribution_levels(
  x = federal_other_vulnerable_arp_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_other_vulnerable_arp.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)

# Federal Student Loans
federal_student_loans_contribution_levels <- contribution_levels(
  x = federal_student_loans_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_student_loans.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)

# State Subsidies
state_subsidies_contribution_levels <- contribution_levels(
  x = state_subsidies_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/state_subsidies.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)

# Federal Health Outlays
federal_health_outlays_contribution_levels <- contribution_levels(
  x = federal_health_outlays_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_health_outlays.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)

# State Health Outlays
state_health_outlays_contribution_levels <- contribution_levels(
  x = state_health_outlays_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/state_health_outlays.rds"), 
  cumprod = cumprod_growth_consumption$cumprod
)


# Aggregate contribution levels by category
federal_levels <- 
  (federal_purchases_contribution_levels + 
     consumption_grants_contribution_levels + 
     investment_grants_contribution_levels) 

state_levels <- 
  (state_purchases_contribution_levels - 
     consumption_grants_contribution_levels - 
     investment_grants_contribution_levels)

taxes_levels <-
  (federal_non_corporate_taxes_contribution_levels + 
     state_non_corporate_taxes_contribution_levels + 
     federal_corporate_taxes_contribution_levels + 
     supply_side_ira_contribution_levels + 
     state_corporate_taxes_contribution_levels) 

transfers_levels <-
  (federal_social_benefits_contribution_levels + 
     state_social_benefits_contribution_levels + 
     rebate_checks_contribution_levels + 
     rebate_checks_arp_contribution_levels + 
     federal_ui_contribution_levels + 
     state_ui_contribution_levels + 
     federal_subsidies_contribution_levels + 
     federal_aid_to_small_businesses_arp_contribution_levels + 
     federal_other_direct_aid_arp_contribution_levels + 
     federal_other_vulnerable_arp_contribution_levels + 
     federal_student_loans_contribution_levels + 
    state_subsidies_contribution_levels + 
     federal_health_outlays_contribution_levels + 
     state_health_outlays_contribution_levels) 

# Get Date Column 
date <- data.frame(usna$date) %>% 
  rename(date = usna.date)

# Get sum of difference between actual and counterfactual for each of the FIM inputs. 
# The counterfactual is equal to the value of the input in the base quarter times cumulative product of the real potential GDP 
# and deflator growth rates. 

minus_neutral_sum <- 
  (federal_levels +
     state_levels +
     taxes_levels +
     transfers_levels)

# Pull "actual" GDP from the GDP test column
gdp_actual <- gdp_test$data_series

# Generate the counterfactual GDP, which is equal to the actual GDP minus delta (the difference between actual and counterfactual for each
# of the FIM inputs.)
gdp_counterfactual <- (gdp_actual - minus_neutral_sum)


percent_difference <- (gdp_actual/gdp_counterfactual - 1)*100

data <- data.frame(
  date,
  minus_neutral_sum, 
  gdp_actual, 
  gdp_counterfactual, 
  percent_difference
) %>% 
  filter(
    date >= start_quarter - 2 & date <= current_quarter + 8 
  )

openxlsx::write.xlsx(data, file = glue('fim_levels.xlsx', overwrite = TRUE))
