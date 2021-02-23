

librarian::shelf('tidyverse', 'zoo', 'TTR', 'tsibble', 'targets', 'tarchetypes', 'lubridate',
                 'alistaire47/pipecleaner', 'glue', 'validate', 'fim', 'dplyover', 'tsibble')

devtools::load_all()
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

create_projections <- function(df){
  components <- get_components_names()
  df %>%  
    make_cumulative_growth_rates() %>%
    fill(components) %>%
    make_forecasts() %>% 
    sum_projections(gtfp, gftfp, gstfp) %>%
    sum_projections(yptx, gfrpt, gsrpt) %>%
    sum_projections(ytpi, gfrpri, gsrpri) %>%
    sum_projections(grcsi, gfrs, gsrs) %>%
    sum_projections(grcsi, gfrs, gsrs) %>%
    sum_projections(yctlg, gfrcp, gsrcp) %>%
    sum_projections(gsub, gfsub, gssub)
}

components_growth_rates <- function(df){
  df %>%
    purchases_growth() %>%
    transfers_growth() %>%
    health_growth() %>%
    subsidies_growth() %>%
    grants_growth() %>%
    deflators_growth() 
}


# End this file with a list of target objects.
last_hist_date <- '2020-12-31'
tar_plan(
  projections = 
    read_data() %>%
    define_variables() %>%
    growth_assumptions() %>%
    reallocations() %>%
    forecast() %>%
    mutate(across(where(is.numeric),
                  ~ coalesce(.x, 0))),
  fim = 
    projections %>%
    add_factors() %>%
    mutate(dplyover::over(c('ui', 'federal_ui', 'state_ui'),
                          ~if_else(id == 'projection',
                                   .('{.x}_override'),
                                   .('{.x}'))
                          )
           ) %>%
    purchases_contributions() %>% 
       spread_social_benefits() %>%
    mutate(non_corporate_taxes =personal_taxes + payroll_taxes+production_taxes,
           federal_non_corporate_taxes =federal_personal_taxes + federal_payroll_taxes+federal_production_taxes,
           state_non_corporate_taxes =state_personal_taxes + state_payroll_taxes+state_production_taxes) %>% 
     taxes_transfers_minus_neutral() %>% 
      calculate_mpc('subsidies') %>%
      calculate_mpc('health_outlays') %>%
      calculate_mpc('social_benefits') %>%
      calculate_mpc('ui') %>%
      calculate_mpc('non_corporate_taxes') %>%
      calculate_mpc('corporate_taxes') %>%
      mutate(rebate_checks_post_mpc = mpc_rebate_checks(rebate_checks_minus_neutral)) %>% 
      taxes_contributions() %>% 
    sum_taxes_contributions() %>% 
    transfers_contributions() %>% 
      sum_transfers_contributions() %>% 
    add_social_benefit_components() %>% 
    sum_taxes_transfers() %>% 
    mutate(federal_contribution = federal_purchases_contribution + federal_taxes_contribution + federal_transfers_contribution)
)


# Next steps:
#  - add factors
#  - overrides
#  - grants contributions
#  - taxes trannsfers counterfactuals
#  - calculate mpcs
#  - taxes and transfers contributionsno
#  
#  
#  
# fim_create(projections) %>%
#   add_factors(last_date = last_hist_date) %>%
#   override_projections() %>%
#   mutate(
#     federal_cgrants = if_else(date == '2020-12-31', 303.95, federal_cgrants)
#   ) %>%
#   fill_overrides() %>%
#   contributions_purchases_grants() %>%
#   total_purchases() %>%
#   mutate(federal_cont = federal_cont - federal_grants_cont,
#          state_local_cont = state_local_cont + federal_grants_cont) %>%
#   remove_social_benefit_components() %>%
#   taxes_transfers_minus_neutral() %>%
#   calculate_mpc('subsidies') %>%
#   calculate_mpc('health_outlays') %>%
#   calculate_mpc('social_benefits') %>%
#   calculate_mpc('unemployment_insurance') %>%
#   calculate_mpc('rebate_checks') %>%
#   calculate_mpc('noncorp_taxes') %>%
#   calculate_mpc('corporate_taxes') %>%
#   taxes_contributions() %>%
#   sum_taxes_contributions() %>%
#   transfers_contributions() %>%
#   sum_transfers_contributions() %>%
#   sum_taxes_transfers() %>%
#   add_social_benefit_components() %>%
#   get_fiscal_impact() 

# rules <-
#   validate::validator(subsidies == federal_subsidies + state_subsidies,
#             health_outlays == federal_health_outlays + state_health_outlays,
#             social_benefits == federal_social_benefits + state_social_benefits,
#             
#             purchases == federal_purchases + state_purchases ,
#             ui_expansion == peuc + pua + puc + wages_lost_assistance,
#             health_grants>= medicaid_grants)
# out <- validate::confront(df, rules)
# summary(out)
