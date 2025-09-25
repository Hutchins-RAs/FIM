# fiscal_impact_BETA.R
#
# This script runs the main FIM. It's the working replacement for fiscal_impact.R
# that will eventually substitute for the original. 

#----- setup-01 ---
# Empty section - don't delete for bookdown purposes 

# ---- section-A.1-prep-for-update ----
Sys.setenv(TZ = 'UTC') # Set the default time zone to UTC (Coordinated Universal Time)

# Load packages
packages <- c(
  "tidyverse", "tsibble", "lubridate", "glue", 
  "TimTeaFan/dplyover", "zoo", "TTR", "fs", "gt", 
  "openxlsx", "snakecase", "rlang", "BrookingsInstitution/ggbrookings"
)
librarian::shelf(packages)

# Load all functions in package
devtools::load_all() 

options(digits = 4) # Limit number of digits
options(scipen = 20)# Turn off scientific notation under 20 digits 

#are we running this after a cbo baseline and pre-bea update?
post_cbo_baseline<- FALSE
# Set the value of 'month_year' to the current month and year (in the format "mm-yyyy")
last_month_year <- glue('{format.Date(today() %m-% months(1), "%m")}-{year(today() %m-% months(1))}')
month_year <- glue('{format.Date(today() - 7, "%m")}-{year(today())}')
print(month_year)

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

# ---- section-A.2-create-empty-directories ----

#setting our reference period to be the post-cbo files if we've already produced
# fim output incorporating the cbo update
if(file.exists(glue('results/{month_year}-post-cbo'))){
  last_month_year<- glue('{month_year}-post-cbo')
}

# Create folder for current update in the results directory
dir_create(glue('results/{month_year}')) 
# Folder to store forecast sheet from current update
dir_create(glue('results/{month_year}/input_data')) 
# Beta folder for Lorae's refactored results
dir_create(glue('results/{month_year}/beta'))

# Copy the file 'forecast.xlsx' from the 'data' directory to the 'input_data' directory
# This is the copy we keep for the current update
file_copy(
  path = 'data/forecast.xlsx', 
  new_path = glue('results/{month_year}/input_data/forecast_{month_year}.xlsx'), 
  overwrite = TRUE
)

# ---- section-B-data-import ----
# Source the module in the src directory containing the functions which import
# data
source("src/data_import.R")

## Read in data sources to be combined
projections <- import_projections()
national_accounts <- import_national_accounts()
forecast <- import_forecast()
historical_overrides <- import_historical_overrides()
deflator_overrides <- import_deflator_overrides()

## Calculate what the current quarter is using the date from historical overrides
current_quarter <- historical_overrides %>% slice_max(date) %>% pull(date)

# ---- section-B-1-test-data-import ----

# Source the module that creates the test data columns used in the FIM
source("src/data_cleaning.R")

# Run the functions defined in src/data_cleaning.R to produce the FIM data columns
federal_purchases_test <- create_federal_purchases(
  national_accounts, 
  forecast, 
  create_placeholder_nas()
)

consumption_grants_test <- create_consumption_grants(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)

investment_grants_test <- create_investment_grants(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)

state_purchases_test <- create_state_purchases(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

federal_non_corporate_taxes_test <- create_federal_non_corporate_taxes(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

state_non_corporate_taxes_test <- create_state_non_corporate_taxes(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

federal_corporate_taxes_test <- create_federal_corporate_taxes(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)

supply_side_ira_test <- create_supply_side_ira(
  forecast,
  historical_overrides,
  create_placeholder_nas()
)


state_corporate_taxes_test <- create_state_corporate_taxes(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)

federal_social_benefits_test <- create_federal_social_benefits(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)

state_social_benefits_test <- create_state_social_benefits(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

rebate_checks_test <- create_rebate_checks(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

rebate_checks_arp_test <- create_rebate_checks_arp(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

federal_ui_test <- create_federal_ui(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

state_ui_test <- create_state_ui(
  national_accounts,
  forecast,
  create_placeholder_nas()
)


federal_subsidies_test <- create_federal_subsidies(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

federal_aid_to_small_businesses_arp_test <- create_federal_aid_to_small_businesses_arp(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)


federal_other_direct_aid_arp_test <- create_federal_other_direct_aid_arp(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)


federal_other_vulnerable_arp_test <- create_federal_other_vulnerable_arp(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)


federal_student_loans_test <- create_federal_student_loans(
  national_accounts,
  forecast,
  historical_overrides,
  create_placeholder_nas()
)


state_subsidies_test <- create_state_subsidies(
  national_accounts,
  forecast,
  create_placeholder_nas()
)


federal_health_outlays_test <- create_federal_health_outlays(
  national_accounts,
  forecast,
  create_placeholder_nas()
)


state_health_outlays_test <- create_state_health_outlays(
  national_accounts,
  forecast,
  create_placeholder_nas()
)

# ---- section-B.3-accessory-variables ----

# Quarterly Federal Purchases Deflator Growth 
federal_purchases_deflator_growth_test <- create_federal_purchases_deflator_growth(
  national_accounts,
  projections,
  deflator_overrides,
  create_placeholder_nas()
)

# Annualized Federal Purchases Deflator Growth 
federal_purchases_deflator_growth_annualized_test <- create_annualized_growth(
  x= federal_purchases_deflator_growth_test)

# Quarterly Consumption Grants Deflator Growth 
consumption_grants_deflator_growth_test <- create_consumption_grants_deflator_growth(
  national_accounts,
  projections,
  deflator_overrides,
  create_placeholder_nas()
)

# Annualized Consumption Grants Deflator Growth 
consumption_grants_deflator_growth_annualized_test <- create_annualized_growth(
  x= consumption_grants_deflator_growth_test)

# Quarterly Investment Grants Deflator Growth 
investment_grants_deflator_growth_test <- create_investment_grants_deflator_growth(
  national_accounts,
  projections,
  deflator_overrides,
  create_placeholder_nas()
)

# Annualized Investment Grants Deflator Growth
investment_grants_deflator_growth_annualized_test <- create_annualized_growth(
  x= investment_grants_deflator_growth_test)

# Quarterly State Purchases Deflator Growth 
state_purchases_deflator_growth_test <- create_state_purchases_deflator_growth(
  national_accounts,
  projections,
  deflator_overrides,
  create_placeholder_nas()
)

# Annualized State Purchases Deflator Growth 
state_purchases_deflator_growth_annualized_test <- create_annualized_growth(
  x= state_purchases_deflator_growth_test)

# Quarterly Consumption Deflator Growth
consumption_deflator_growth_test <- create_consumption_deflator_growth(
  national_accounts,
  projections,
  deflator_overrides,
  create_placeholder_nas()
)

# Annualized Consumption Deflator Growth 
consumption_deflator_growth_annualized_test <- create_annualized_growth(
  x= consumption_deflator_growth_test)

# Quarterly Real Potential GDP Growth 
real_potential_gdp_growth_test <- create_real_potential_gdp_growth(
  national_accounts,
  projections,
  create_placeholder_nas()
)

# Annualized Real Potential GDP Growth 
real_potential_gdp_growth_annualized_test <- create_annualized_growth(
  x= real_potential_gdp_growth_test)

# GDP 
gdp_test <- create_gdp(
  national_accounts,
  projections,
  create_placeholder_nas()
)

# Consumption
consumption_test <- create_consumption(
  national_accounts,
  projections,
  create_placeholder_nas()
)

# Uncertainty
uncertainty_test <- create_uncertainty(
  forecast,
  historical_overrides, 
  create_placeholder_nas()
)

# uncertainty_test$data_series[222] <- -0.3

# EXTRAS 
# Date 
date_test <- create_date(
  national_accounts,
  projections
)

# ID 
id_test <- create_id(
  national_accounts, 
  projections
)

# Recession 
recession_test <- create_recession(
  national_accounts, 
  projections,
  create_placeholder_nas()
)

# ---fim-calculation----

######################################################################################
# This is the point where we go from generating our data inputs to actually calculating the FIM
######################################################################################

# This script defines the input variables used in the FIM. It assumes that the 
# test columns are saved in memory from the section above having already
# been run.
source("src/define_inputs.R")

# Next, we source essential functions we need to calculate the FIM in this section.
# All of these modules contain nothing but functions. No actual code is executed
# when you source them. Instead, the code is executed in this script.
source("src/contributions.R")

# Another type of variable we need is MPC matrices. If you read the documentation
# in `src/mpc_lorae.R`, you'll develop a clearer understanding of how these 
# matrices produce an MPC operation. We cache these matrices so that they do not 
# need to be regenerated each time the code is run. Instead, in the future, we'll
# only rebuild these matrices when MPC inputs are changed using an "observer" 
# design pattern. This will save us a lot of computing time.
# 

# ---- section-C.1-apply-taxes-mpcs ----

# APPLY MPCS TO TAXES

# Federal Non-Corporate Taxes
post_mpc_federal_non_corporate_taxes <- mpc(x = federal_non_corporate_taxes_test$data_series, 
                                            mpc = readRDS("cache/mpc_matrices/federal_non_corporate_taxes.rds"))

# State Non-Corporate Taxes
post_mpc_state_non_corporate_taxes <- mpc(x = state_non_corporate_taxes_test$data_series, 
                                          mpc = readRDS("cache/mpc_matrices/state_non_corporate_taxes.rds"))

# Federal Corporate Taxes
post_mpc_federal_corporate_taxes <- mpc(x = federal_corporate_taxes_test$data_series, 
                                        mpc = readRDS("cache/mpc_matrices/federal_corporate_taxes.rds"))

# State Corporate Taxes
post_mpc_state_corporate_taxes <- mpc(x = state_corporate_taxes_test$data_series, 
                                      mpc = readRDS("cache/mpc_matrices/state_corporate_taxes.rds"))

# Supply Side IRA (no MPC)
supply_side_ira <- as.matrix(supply_side_ira_test$data_series)



# ---- section-C.2-apply-transfers-mpcs ----

# Federal Social Benefits 
post_mpc_federal_social_benefits <- mpc(x = federal_social_benefits_test$data_series, 
                                        mpc = readRDS("cache/mpc_matrices/federal_social_benefits.rds"))

# State Social Benefits
post_mpc_state_social_benefits <- mpc(x = state_social_benefits_test$data_series, 
                                      mpc = readRDS("cache/mpc_matrices/state_social_benefits.rds"))

# Rebate Checks 
post_mpc_rebate_checks <- mpc(x = rebate_checks_test$data_series, 
                              mpc = readRDS("cache/mpc_matrices/rebate_checks.rds"))

# Rebate Checks ARP 
post_mpc_rebate_checks_arp <- mpc(x = rebate_checks_arp_test$data_series, 
                                  mpc = readRDS("cache/mpc_matrices/rebate_checks_arp.rds"))

# Federal UI 
post_mpc_federal_ui <- mpc(x = federal_ui_test$data_series, 
                           mpc = readRDS("cache/mpc_matrices/federal_ui.rds"))

# State UI 
post_mpc_state_ui <- mpc(x = state_ui_test$data_series, 
                         mpc = readRDS("cache/mpc_matrices/state_ui.rds"))

# Federal Subsidies
post_mpc_federal_subsidies <- mpc(x = federal_subsidies_test$data_series, 
                                  mpc = readRDS("cache/mpc_matrices/federal_subsidies.rds"))

# Federal Aid to Small Businesses ARP 
post_mpc_federal_aid_to_small_businesses_arp <- mpc(x = federal_aid_to_small_businesses_arp_test$data_series, 
                                                    mpc = readRDS("cache/mpc_matrices/federal_aid_to_small_businesses_arp.rds"))

# Federal Other Direct Aid ARP 
post_mpc_federal_other_direct_aid_arp <- mpc(x = federal_other_direct_aid_arp_test$data_series, 
                                             mpc = readRDS("cache/mpc_matrices/federal_other_direct_aid_arp.rds"))

# Federal Other Vulnerable ARP 
post_mpc_federal_other_vulnerable_arp <- mpc(x = federal_other_vulnerable_arp_test$data_series, 
                                             mpc = readRDS("cache/mpc_matrices/federal_other_vulnerable_arp.rds"))

# Federal Student Loans 
post_mpc_federal_student_loans <- mpc(x = federal_student_loans_test$data_series,
                                      mpc = readRDS("cache/mpc_matrices/federal_student_loans.rds"))

# State Subsidies 
post_mpc_state_subsidies <- mpc(x = state_subsidies_test$data_series, 
                                mpc = readRDS("cache/mpc_matrices/state_subsidies.rds"))

# Federal Health Outlays 
post_mpc_federal_health_outlays <- mpc(x = federal_health_outlays_test$data_series,
                                       mpc = readRDS("cache/mpc_matrices/federal_health_outlays.rds"))

# State Health Outlays 
post_mpc_state_health_outlays <- mpc(x = state_health_outlays_test$data_series, 
                                     mpc = readRDS("cache/mpc_matrices/state_health_outlays.rds"))

# ---- section-C.3-create-net-transfers ----

#### CREATE TAXES #####
taxes_test <- data.frame(date = date, data_series = post_mpc_federal_non_corporate_taxes + 
                           post_mpc_state_non_corporate_taxes + post_mpc_federal_corporate_taxes + 
                           post_mpc_state_corporate_taxes + supply_side_ira) 

#### CREATE TRANSFERS #####

transfers_test <- data.frame(date = date_test$date, data_series = post_mpc_federal_social_benefits + post_mpc_state_social_benefits +
                               post_mpc_rebate_checks + post_mpc_rebate_checks_arp + 
                               post_mpc_federal_ui + post_mpc_state_ui + 
                               post_mpc_federal_subsidies + post_mpc_federal_aid_to_small_businesses_arp + 
                               post_mpc_federal_other_direct_aid_arp + post_mpc_federal_other_vulnerable_arp  + 
                               post_mpc_federal_student_loans + post_mpc_state_subsidies +
                               post_mpc_federal_health_outlays + post_mpc_state_health_outlays)

#### SUM TAXES AND TRANSFERS ####

taxes_transfers_test <- data.frame(date = date, 
                                   data_series = taxes_test$data_series + transfers_test$data_series)

### CREATE FIM STATE PURCHASES 
fim_state_purchases_test = data.frame(date = date, 
                                      data_series = state_purchases_test$data_series -
                                        consumption_grants_test$data_series -
                                        investment_grants_test$data_series)

# ---- section-fim-calculation ----

#######################################################
#               CALCULATE THE FIM                     #
#######################################################

# ---- section-C.4-calculate-purchases-fim ----
# Federal Purchases Contribution (NIPA Consistent)
nipa_federal_purchases_contribution <- contribution_purchases(
  x = federal_purchases_test$data_series, # Using the new test version
  dg = federal_purchases_deflator_growth_test$data_series, # Using the new test version
  rpgg = real_potential_gdp_growth_annualized_test$data_series, # Using the new test version
  gdp = gdp_test$data_series # Using the new test version
)

# State Purchases Contribution (NIPA Consistent)
nipa_state_purchases_contribution <- contribution_purchases(
  x = state_purchases_test$data_series, # Using the new test version
  dg = state_purchases_deflator_growth_test$data_series, # Using the new test version
  rpgg = real_potential_gdp_growth_annualized_test$data_series, # Using the new test version
  gdp = gdp_test$data_series # Using the new test version
) 

# Total NIPA Purchases Contribution (NIPA Consistent)
nipa_total_purchases_contribution <- nipa_state_purchases_contribution + 
  nipa_federal_purchases_contribution

# State Purchases Contribution (FIM Consistent)
fim_state_purchases_contribution <- contribution_purchases(
  x = fim_state_purchases_test$data_series, # Using the new test version
  dg = state_purchases_deflator_growth_test$data_series, # Using the new test version
  rpgg = real_potential_gdp_growth_annualized_test$data_series, # Using the new test version
  gdp = gdp_test$data_series # Using the new test version
) 

# Federal Purchases Contribution (FIM Consistent)
fim_federal_purchases_contribution <- nipa_total_purchases_contribution - 
  fim_state_purchases_contribution 

# ---- section-C.5-calculate-net-transfers-FIM ----

# Net Transfers
consumption_contribution <- contribution_transfers(
  x = taxes_transfers_test$data_series, 
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# Taxes Contribution 
taxes_contribution <- contribution_transfers(
  x = taxes_test$data_series, 
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# Transfers Contribution 
transfers_contribution <- contribution_transfers(
  x = transfers_test$data_series, 
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# Federal Non-Corporate Taxes 
federal_non_corporate_taxes_contribution <- contribution_transfers(
  x = post_mpc_federal_non_corporate_taxes,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# State Non-Corporate Taxes
state_non_corporate_taxes_contribution <- contribution_transfers(
  x = post_mpc_state_non_corporate_taxes,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# Federal Corporate Taxes
federal_corporate_taxes_contribution <- contribution_transfers(
  x = post_mpc_federal_corporate_taxes,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# Supply Side IRA 
supply_side_ira_contribution <- contribution_transfers(
  x = supply_side_ira_test$data_series,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# State Corporate Taxes
state_corporate_taxes_contribution <- contribution_transfers(
  x = post_mpc_state_corporate_taxes,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# Federal Social Benefits Contribution 
federal_social_benefits_contribution <- contribution_transfers(
  x = post_mpc_federal_social_benefits,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# State Social Benefits 
state_social_benefits_contribution <- contribution_transfers(
  x = post_mpc_state_social_benefits,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# Rebate Checks
rebate_checks_contribution <- contribution_transfers(
  x = post_mpc_rebate_checks,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# Rebate Checks ARP 
rebate_checks_arp_contribution <- contribution_transfers(
  x = post_mpc_rebate_checks_arp,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# Federal UI 
federal_ui_contribution <- contribution_transfers(
  x = post_mpc_federal_ui,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# State UI
state_ui_contribution <- contribution_transfers(
  x = post_mpc_state_ui,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# Federal Subsidies Contribution 
federal_subsidies_contribution <- contribution_transfers(
  x = post_mpc_federal_subsidies,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# Federal Aid to Small Businesses ARP 
federal_aid_to_small_businesses_arp_contribution <- contribution_transfers(
  x = post_mpc_federal_aid_to_small_businesses_arp,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# Federal Other Direct Aid ARP 
federal_other_direct_aid_arp_contribution <- contribution_transfers(
  x = post_mpc_federal_other_direct_aid_arp,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# Federal Other Vulnerable ARP
federal_other_vulnerable_arp_contribution <- contribution_transfers(
  x = post_mpc_federal_other_vulnerable_arp,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# Federal Student Loans 
federal_student_loans_contribution <- contribution_transfers(
  x = post_mpc_federal_student_loans,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# State Subsidies Contribution 
state_subsidies_contribution <- contribution_transfers(
  x = post_mpc_state_subsidies,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# Federal Health Outlays 
federal_health_outlays_contribution <- contribution_transfers(
  x = post_mpc_federal_health_outlays,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# State Health Outlays 
state_health_outlays_contribution <- contribution_transfers(
  x = post_mpc_state_health_outlays,
  dg = consumption_deflator_growth_test$data_series, 
  rpgg = real_potential_gdp_growth_test$data_series, 
  c = consumption_test$data_series,
  gdp = gdp_test$data_series
)

# ---- section-C.6-aggregate-FIM-contributions ----

# Rename Purchases to fit our results naming convention 
federal_contribution <- fim_federal_purchases_contribution
state_contribution <- fim_state_purchases_contribution
federal_purchases_contribution <- nipa_federal_purchases_contribution
state_purchases_contribution <- nipa_state_purchases_contribution 

test <- data.frame(uncertainty_test$date, consumption_contribution, uncertainty_test$data_series)
View(test)


# Revise consumption contribution to include uncertainty factor 
consumption_contribution <- consumption_contribution + uncertainty_test$data_series

# Sum the Components to create the total FIM 
fiscal_impact_measure <-
  (federal_contribution +
     state_contribution +
     consumption_contribution) 

# Replace NAs with Zeros
fiscal_impact_measure <- replace(fiscal_impact_measure, 
                                 is.na(fiscal_impact_measure), 0)

# Calculate Four Quarter Moving Average
fiscal_impact_4q_ma <- fiscal_impact_measure %>%
  SMA(zoo::na.locf(., na.rm = F), n=4)


# ---- section-C.7-output-results ----
# Combine all the inputs into a data frame
inputs_df <- data.frame(
  date,
  id,
  recession,
  federal_purchases_deflator_growth,
  consumption_grants_deflator_growth,
  investment_grants_deflator_growth,
  state_purchases_deflator_growth,
  consumption_deflator_growth,
  real_potential_gdp_growth,
  gdp,
  consumption,
  uncertainty, 
  federal_purchases,
  consumption_grants,
  investment_grants,
  state_purchases,
  federal_non_corporate_taxes,
  state_non_corporate_taxes,
  federal_corporate_taxes,
  supply_side_ira,
  state_corporate_taxes,
  federal_social_benefits,
  state_social_benefits,
  rebate_checks,
  rebate_checks_arp,
  federal_ui,
  state_ui,
  federal_subsidies,
  federal_aid_to_small_businesses_arp,
  federal_other_direct_aid_arp,
  federal_other_vulnerable_arp,
  federal_student_loans,
  state_subsidies,
  federal_health_outlays,
  state_health_outlays,
  post_mpc_federal_non_corporate_taxes,
  post_mpc_state_non_corporate_taxes,
  post_mpc_federal_corporate_taxes,
  post_mpc_state_corporate_taxes,
  post_mpc_federal_social_benefits,
  post_mpc_state_social_benefits, 
  post_mpc_rebate_checks,
  post_mpc_rebate_checks_arp, 
  post_mpc_federal_ui,
  post_mpc_state_ui,
  post_mpc_federal_subsidies,
  post_mpc_federal_aid_to_small_businesses_arp,
  post_mpc_federal_other_vulnerable_arp,
  post_mpc_federal_student_loans, 
  post_mpc_state_subsidies, 
  post_mpc_federal_health_outlays, 
  post_mpc_state_health_outlays
) %>%
  as_tsibble(index = date)

# Combine all the contributions into a data frame
contributions_df <- data.frame(
  date,
  id,
  recession,
  federal_purchases_contribution,
  state_purchases_contribution, 
  federal_non_corporate_taxes_contribution, 
  state_non_corporate_taxes_contribution, 
  federal_corporate_taxes_contribution, 
  supply_side_ira_contribution, 
  state_corporate_taxes_contribution, 
  federal_social_benefits_contribution, 
  state_social_benefits_contribution, 
  rebate_checks_contribution,
  rebate_checks_arp_contribution, 
  federal_ui_contribution, 
  state_ui_contribution, 
  federal_subsidies_contribution,
  federal_aid_to_small_businesses_arp_contribution, 
  federal_other_direct_aid_arp_contribution, 
  federal_other_vulnerable_arp_contribution,
  federal_student_loans_contribution, 
  state_subsidies_contribution,
  federal_health_outlays_contribution,
  state_health_outlays_contribution,
  federal_contribution,
  state_contribution,
  taxes_contribution, 
  transfers_contribution, 
  consumption_contribution, 
  fiscal_impact_measure,
  fiscal_impact_4q_ma
) %>%
  as_tsibble(index = date)

# Write the contributions and inputs to an Excel file in results/{month_year}/beta
# TODO: This code only works if the beta/ directory already exists. 
openxlsx::write.xlsx(contributions_df, file = glue('results/{month_year}/beta/contributions-{month_year}.xlsx'), overwrite = TRUE)
openxlsx::write.xlsx(inputs_df, file = glue('results/{month_year}/beta/inputs-{month_year}.xlsx'), overwrite = TRUE)

write_rds(contributions_df, file = 'data/contributions.rds')
usethis::use_data(contributions_df, overwrite = TRUE)

# ---- section-C.7-generate-web-materials ----

# Generate interactive data frame from contributions
interactive <- contributions_df %>% 
  # Filter rows of contributions by date, keeping only those between 1999 Q4 and
  # current quarter + 8
  filter_index('1999 Q4' ~ as.character(current_quarter + 8)) %>% 
  # Select only specific columns
  select(date, 
         impact = fiscal_impact_4q_ma,
         recession,
         total = fiscal_impact_measure,
         federal = federal_contribution,
         state_local = state_contribution,
         consumption = consumption_contribution,
         projection = id
  ) %>% 
  # Recode `recession` and `projection` variables to 0 and 1 binaries
  mutate(recession = recode(recession, `-1` = 0),
         recession = replace_na(recession, 0),
         projection = recode(projection, historical = 0, projection = 1)
  ) %>%
  # Split date column into year and quarter columns
  separate(date, c('year', 'quarter'))

# Write interactive data frame to CSV file
readr::write_csv(interactive,  file = glue('results/{month_year}/beta/interactive-{month_year}.csv'))

# Make HTML FIM graphs for website and email. Save as FIM/Fiscal-Impact.html
rmarkdown::render('Fiscal-Impact.Rmd',
                  # Render R Markdown document to PDF file
                  output_file = 'Fiscal-Impact.html',
                  clean = TRUE,
                  params = list(start = yearquarter('1999 Q4'), end = current_quarter + 8))

# Copy FIM/Fiscal-Impact.html graphs to the results/month-year/beta folder
file_copy(path = 'Fiscal-Impact.html',
          new_path = glue('results/{month_year}/beta/Fiscal-Impact-{month_year}.html'),
          overwrite = TRUE)

# Get update comparison html file 
source("scripts/index_temp.R")

rmarkdown::render(input = 'update-comparison-markdown.Rmd',
                  output_file = glue('results/{month_year}/beta/update-comparison-{month_year}'),
                  clean = TRUE)