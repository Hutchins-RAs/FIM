##############
# FIM LEVELS #
##############

# Define Start and End Quarter 
# Set the start and end quarter below 
start_quarter <- yearquarter("2022 Q2")
  start_position <- which(usna$date == start_quarter)
end_quarter <- yearquarter("2022 Q3")
  end_position <- which(usna$date == end_quarter) 
base_quarter <- start_quarter - 1 
  base_position <- which(usna$date == base_quarter) 
  
#################
# DEFINE GROWTH #
#################
  
# Generate "cumprod_growth" function. This function takes a deflator data series as its input. It gets the cumulative product of (1+rpgg+dg) beginning in the start_quarter. 
cumprod_growth <- 
    function(deflator){
      data.frame(real_potential_gdp_growth_test, deflator) %>% 
        mutate(
          sum = ifelse(date >= start_quarter, data_series.1 + data_series, 0), # sum the growth rates beginning in start_quarter
          cumprod = ifelse(date >= start_quarter, cumprod(1 + sum) - 1, NA) # get the cumulative product of the summed growth rates starting in start_quarter 
        ) 
         select(
           cumprod # keep only the cumulative product 
      )
    }

# Calculate the cumulative product of the growth rates for each of the deflators 

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

# Define Minus Neutral Function 
minus_neutral_levels <- function(x, # the data in question
                          cumprod 
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
  output = x / gdp[base_position]
  return(output)
}

# Define Wrapper Function 
contribution_levels <- function(x, mpc_matrix = NULL, cumprod, gdp) {
  # Apply MPC Function
  if (!is.null(mpc_matrix)) {
    x <- x %>%
      mpc_levels(x = ., mpc_matrix = mpc_matrix)
  }
  
  # Apply the minus_neutral function to x, setting real potential GDP growth
  result <- x %>%
    minus_neutral_levels(x = ., cumprod = cumprod)
  
  # Apply the scale_to_gdp function
  result %>%
    scale_to_gdp_levels(x = ., gdp = gdp)
}

#########################
# Define Level Function #
#########################


level <- function(v) {
  # Capture the name of the variable passed as 'v'
  v_name <- deparse(substitute(v))
  
  data <- data.frame(v) %>%
    mutate(
      x = row_number(),
      quarters_since_start = x - base_position
    ) %>%
    mutate(
      data_series = 100 * ((1 + v)^(4 / quarters_since_start) - 1)
    ) %>%
    select(
      -x,
      -quarters_since_start,
      -v
    ) %>%
    rename(!!v_name := data_series)  # Use !! to unquote and assign the name
  
  return(data)
}


#####################
# Get Contributions #
#####################


# Federal Purchases 
federal_purchases_contribution_levels <- contribution_levels(
  x = federal_purchases_test$data_series, # Using the new test version
  mpc_matrix = NULL,
  cumprod = cumprod_growth_federal_purchases$cumprod,
  gdp = gdp_test$data_series # Using the new test version
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

# State purchases
state_purchases_contribution_levels <- contribution_levels(
  x = state_purchases_test$data_series, 
  mpc_matrix = NULL,
  cumprod = cumprod_growth_state_purchases$cumprod, 
  gdp = gdp_test$data_series 
)

state_purchases_levels <- level(state_purchases_contribution_levels) 

# Federal non corporate taxes
federal_non_corporate_taxes_contribution_levels <- contribution_levels(
  x = federal_non_corporate_taxes_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_non_corporate_taxes.rds"), 
  cumprod = cumprod_growth_consumption$cumprod, 
  gdp = gdp_test$data_series 
)

federal_non_corporate_taxes_levels <- level(federal_non_corporate_taxes_contribution_levels)

# State non corporate taxes
state_non_corporate_taxes_contribution_levels <- contribution_levels(
  x = state_non_corporate_taxes_test$data_series,  
  mpc_matrix = readRDS("cache/mpc_matrices/state_non_corporate_taxes.rds"), 
  cumprod = cumprod_growth_consumption$cumprod, 
  gdp = gdp_test$data_series
)

state_non_corporate_taxes_levels <- level(state_non_corporate_taxes_contribution_levels)

# Federal corporate taxes
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

# State corporate taxes
state_corporate_taxes_contribution_levels <- contribution_levels(
  x = state_corporate_taxes_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/state_corporate_taxes.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

state_corporate_taxes_levels <- level(state_corporate_taxes_contribution_levels)

# Federal social benefits
federal_social_benefits_contribution_levels <- contribution_levels(
  x = federal_social_benefits_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_social_benefits.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series
)

federal_social_benefits_levels <- level(federal_social_benefits_contribution_levels)

# State social benefits
state_social_benefits_contribution_levels <- contribution_levels(
  x = state_social_benefits_test$data_series,
  mpc_matrix = readRDS("cache/mpc_matrices/state_social_benefits.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

state_social_benefits_levels <- level(state_social_benefits_contribution_levels)

# Rebate checks
rebate_checks_contribution_levels <- contribution_levels(
  x = rebate_checks_test$data_series, # Using the new test version 
  mpc_matrix = readRDS("cache/mpc_matrices/rebate_checks.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series # Using the new test version
)

rebate_checks_levels <- level(rebate_checks_contribution_levels)

# Rebate checks ARP
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

# Federal subsidies
federal_subsidies_contribution_levels <- contribution_levels(
  x = federal_subsidies_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_subsidies.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_subsidies_levels <- level(federal_subsidies_contribution_levels)

# Federal aid to small businesses
federal_aid_to_small_businesses_arp_contribution_levels <- contribution_levels(
  x = federal_aid_to_small_businesses_arp_test$data_series,  
  mpc_matrix = readRDS("cache/mpc_matrices/federal_aid_to_small_businesses_arp.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_aid_to_small_businesses_arp_levels <- level(federal_aid_to_small_businesses_arp_contribution_levels)

# Federal other direct aid arp
federal_other_direct_aid_arp_contribution_levels <- contribution_levels(
  x = federal_other_direct_aid_arp_test$data_series,  
  mpc_matrix = readRDS("cache/mpc_matrices/federal_other_direct_aid_arp.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_other_direct_aid_arp_levels <- level(federal_other_direct_aid_arp_contribution_levels)

# Federal other vulnerable arp
federal_other_vulnerable_arp_contribution_levels <- contribution_levels(
  x = federal_other_vulnerable_arp_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_other_vulnerable_arp.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_other_vulnerable_arp_levels <- level(federal_other_vulnerable_arp_contribution_levels)

# Federal student loans
federal_student_loans_contribution_levels <- contribution_levels(
  x = federal_student_loans_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_student_loans.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_student_loans_levels <- level(federal_student_loans_contribution_levels)

# State subsidies
state_subsidies_contribution_levels <- contribution_levels(
  x = state_subsidies_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/state_subsidies.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

state_subsidies_levels <- level(state_subsidies_contribution_levels)

# Federal health outlays
federal_health_outlays_contribution_levels <- contribution_levels(
  x = federal_health_outlays_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/federal_health_outlays.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series 
)

federal_health_outlays_levels <- level(federal_health_outlays_contribution_levels)

# State health outlay
state_health_outlays_contribution_levels <- contribution_levels(
  x = state_health_outlays_test$data_series, 
  mpc_matrix = readRDS("cache/mpc_matrices/state_health_outlays.rds"), 
  cumprod = cumprod_growth_consumption$cumprod,
  gdp = gdp_test$data_series
)

state_health_outlays_levels <- level(state_health_outlays_contribution_levels)

###########################
# CLEAN AND OUTPUT LEVELS #
###########################

# Get FIM 
federal_levels <- 
  (federal_purchases_levels + 
     consumption_grants_levels + 
     investment_grants_levels) 

state_levels <- 
  (state_purchases_levels - 
     consumption_grants_levels - 
     investment_grants_levels)

taxes_levels <-
  (federal_non_corporate_taxes_levels + 
     state_non_corporate_taxes_levels + 
     federal_corporate_taxes_levels + 
     supply_side_ira_levels + 
     state_corporate_taxes_levels) 

transfers_levels <-
  (federal_social_benefits_levels + 
     state_social_benefits_levels + 
     rebate_checks_levels + 
     rebate_checks_arp_levels + 
     federal_ui_levels + 
     state_ui_levels + 
     federal_subsidies_levels + 
     federal_aid_to_small_businesses_arp_levels + 
     federal_other_direct_aid_arp_levels + 
     federal_other_vulnerable_arp_levels + 
     federal_student_loans_levels + 
     state_subsidies_levels + 
     federal_health_outlays_levels + 
     state_health_outlays_levels) 

consumption_levels <-
  (taxes_levels +
     transfers_levels)

fiscal_impact_measure_levels <-
  (federal_levels +
     state_levels +
     taxes_levels +
     transfers_levels)

date <- data.frame(usna$date) %>% 
  rename(date = usna.date)

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

openxlsx::write.xlsx(levels_df, file = glue('fim_levels.xlsx', overwrite = TRUE))
