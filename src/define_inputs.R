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
federal_purchases_deflator_growth <- projections$federal_purchases_deflator_growth
consumption_grants_deflator_growth <- projections$consumption_grants_deflator_growth
investment_grants_deflator_growth <- projections$investment_grants_deflator_growth
state_purchases_deflator_growth <- projections$state_purchases_deflator_growth
consumption_deflator_growth <- projections$consumption_deflator_growth
# GDP
real_potential_gdp_growth <- projections$real_potential_gdp_growth
gdp <- projections$gdp
# Extras
date <- projections$date
id <- projections$id
recession <- projections$recession

### Main variables
federal_purchases <- projections$federal_purchases
consumption_grants <- projections$consumption_grants
investment_grants <- projections$investment_grants
state_purchases <- projections$state_purchases
federal_non_corporate_taxes <- projections$federal_non_corporate_taxes
state_non_corporate_taxes <- projections$state_non_corporate_taxes
federal_corporate_taxes <- projections$federal_corporate_taxes
supply_side_ira <- projections$supply_side_ira
state_corporate_taxes <- projections$state_corporate_taxes
federal_social_benefits <- projections$federal_social_benefits
state_social_benefits <- projections$state_social_benefits
rebate_checks <- projections$rebate_checks
rebate_checks_arp <- projections$rebate_checks_arp
federal_ui <- projections$federal_ui
state_ui <- projections$state_ui
federal_subsidies <- projections$federal_subsidies
federal_aid_to_small_businesses_arp <- projections$federal_aid_to_small_businesses_arp
federal_other_direct_aid_arp <- projections$federal_other_direct_aid_arp
federal_other_vulnerable_arp <- projections$federal_other_vulnerable_arp
federal_student_loans <- projections$federal_student_loans
state_subsidies <- projections$state_subsidies
federal_health_outlays <- projections$federal_health_outlays
state_health_outlays <- projections$state_health_outlays
