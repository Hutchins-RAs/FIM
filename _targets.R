library(targets)
library('tarchetypes')
library('tsibble')


source("R/packages.R")
library('lubridate')
source("R/functions.R")

# Build workflow plan data frame.

options(tidyverse.quiet = TRUE)
options(crayon.enabled = FALSE)


# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.

# Set target-specific options such as packages.
tar_option_set(error = "workspace")
get_historical_data <- function(){
  readRDS('data/historical.rds') %>%
    mutate(id = 'historical') %>%
    millions_to_billions() %>%
    rename(cpiu = ui) 
}


# End this file with a list of target objects.
last_hist_date <- '2020-12-31'
tar_plan(
  projections = 
    read_data() %>%
    cola_adjustment() %>%
    growth_rates() %>%
    project_state_taxes() %>%
    unemployment_insurance_reallocation() %>%
    fmap_share_old() %>%
    override_state_purchases_growth_rate() %>%
    components_growth_rates() %>%
    group_by(id) %>%
    mutate(forecast_period = if_else(date <= last_hist_date, 0, 1)) %>%
    mutate(historical = if_else(date > last_hist_date, 0, 1)) %>%
    project_state_taxes() %>%
    create_projections() %>%
    medicaid_reallocation(),
  proj2 = projections %>% project_state_taxes(),
  fim = fim_create(proj2) %>%
    add_factors(last_date = last_hist_date) %>%
    override_projections() %>%
    mutate(
      federal_cgrants = if_else(date == '2020-12-31', 303.95, federal_cgrants)
    ) %>%
    fill_overrides() %>%
    contributions_purchases_grants() %>%
    total_purchases() %>%
    mutate(federal_cont = federal_cont - federal_grants_cont,
           state_local_cont = state_local_cont + federal_grants_cont) %>%
    remove_social_benefit_components() %>%
    taxes_transfers_minus_neutral() %>%
    calculate_mpc('subsidies') %>%
    calculate_mpc('health_outlays') %>%
    calculate_mpc('social_benefits') %>%
    calculate_mpc('unemployment_insurance') %>%
    calculate_mpc('rebate_checks') %>%
    calculate_mpc('noncorp_taxes') %>%
    calculate_mpc('corporate_taxes') %>%
    taxes_contributions() %>%
    sum_taxes_contributions() %>%
    transfers_contributions() %>%
    sum_transfers_contributions() %>%
    sum_taxes_transfers() %>%
    add_social_benefit_components() %>%
    get_fiscal_impact() %>%
    arrange(
      date,
      recession,
      fiscal_impact,
      fiscal_impact_moving_average,
      federal_cont,
      state_local_cont,
      taxes_transfers_cont,
      federal_taxes_transfers_cont,
      state_taxes_transfers_cont
    ),
  contributions = fim %>%
    filter(date >= '1999-12-31') %>%
    select(ends_with('cont')),
  fimbar1 = fim %>% plot_fiscal_impact(),
  fimbar2 = fim %>% plot_fiscal_impact_components(),
  tar_render(report, "Fiscal-Impact.Rmd")
)
# 
# 
# library(validate)
# 
# 
# rules <- validator(
# fiscal_impact == federal_cont + state_local_cont + taxes_transfers_cont,
# taxes_cont == federal_taxes_cont + state_taxes_cont,
# trasfers_cont  == unemployment_insurance_cont + social_benefits_cont + subsidies_cont + rebate_checks_cont)
# 
# out <- confront(fim, rules)
# summary(out)
