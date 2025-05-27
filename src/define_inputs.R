# source("define_inputs.R")
# This script defines the 33 input variables used in the FIM.

# We divide the subset of columns that we need into two categories: "accessory" 
# and "main" variables. Both types of columns are inputs for the FIM. The "main"
# columns are key variables that directly lead to a FIM "contribution" output, 
# like the `state_ui` series and the `federal_non_corporate_taxes` series. The
# "accessory" columns are additional macroeconomic variables that are needed to
# calculate the FIM, but do not directly themselves produce a direct contribution
# variables: GDP, various deflators,  and real potential GDP growth.
#
# All variables, accessory and main, are vectors of the same length: about
# 270 elements. This is because they all inherit their length from the `projections` 
# data frame, whose length is determined by the number of periods in the time 
# series from 1970 Q1 to the final projection date (which, as of this writing, 
# was sometime in 2034. But that number increases as time goes by).

# In order for this script to run, the `projections` data frame must be loaded
# into memory.

### Accessory variables
# Deflators
federal_purchases_deflator_growth <- federal_purchases_deflator_growth_test$data_series
consumption_grants_deflator_growth <- consumption_grants_deflator_growth_test$data_series
investment_grants_deflator_growth <- investment_grants_deflator_growth_test$data_series
state_purchases_deflator_growth <- state_purchases_deflator_growth_test$data_series
consumption_deflator_growth <- consumption_deflator_growth_test$data_series
# GDP
real_potential_gdp_growth <- real_potential_gdp_growth_test$data_series
gdp <- gdp_test$data_series
# Consumption 
consumption <- consumption_test$data_series 
# Uncertainty
uncertainty <- uncertainty_test$data_series
# Extras
date <- date_test$date
id <- id_test$data_series
recession <- recession_test$data_series

### Main variables
federal_purchases <- federal_purchases_test$data_series
consumption_grants <- consumption_grants_test$data_series
investment_grants <- investment_grants_test$data_series
state_purchases <- state_purchases_test$data_series
federal_non_corporate_taxes <- federal_non_corporate_taxes_test$data_series
state_non_corporate_taxes <- state_non_corporate_taxes_test$data_series
federal_corporate_taxes <- federal_corporate_taxes_test$data_series
supply_side_ira <- supply_side_ira_test$data_series
state_corporate_taxes <- state_corporate_taxes_test$data_series
federal_social_benefits <- federal_social_benefits_test$data_series
state_social_benefits <- state_social_benefits_test$data_series
rebate_checks <- rebate_checks_test$data_series
rebate_checks_arp <- rebate_checks_arp_test$data_series
federal_ui <- federal_ui_test$data_series
state_ui <- state_ui_test$data_series
federal_subsidies <- federal_subsidies_test$data_series
federal_aid_to_small_businesses_arp <- federal_aid_to_small_businesses_arp_test$data_series
federal_other_direct_aid_arp <- federal_other_direct_aid_arp_test$data_series
federal_other_vulnerable_arp <- federal_other_vulnerable_arp_test$data_series
federal_student_loans <- federal_student_loans_test$data_series
state_subsidies <- state_subsidies_test$data_series
federal_health_outlays <- federal_health_outlays_test$data_series
state_health_outlays <- state_health_outlays_test$data_series
