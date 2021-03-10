librarian::shelf('tidyverse', 'zoo', 'TTR', 'tsibble', 'targets', 'tarchetypes', 'lubridate',
                 'alistaire47/pipecleaner', 'glue', 'validate', 'fim', 'dplyover', 'tsibble')

devtools::load_all()

conflicted::conflict_prefer('filter', 'dplyr') 
conflicted::conflict_prefer('lag', 'dplyr')
# Build workflow plan data frame.

options(tidyverse.quiet = TRUE)
options(crayon.enabled = FALSE)



# Set target-specific options such as packages.
tar_option_set(error = "workspace")



# End this file with a list of target objects.
tar_plan(
  projections = 
    read_data() %>%
    define_variables() %>%
    growth_assumptions() %>%
    create_override(
      var = state_purchases_growth,
      start = yearquarter('2020 Q4'),
      end = yearquarter('2022 Q1'),
      values = c(rep(0.0025, 3), 0.005, 0.0075, 0.01)
    )  %>%
    reallocations() %>% 
    forecast() %>%
    ungroup() %>% 
    mutate(social_benefits = federal_social_benefits + state_social_benefits) %>% 
    mutate(corporate_taxes = federal_corporate_taxes + state_corporate_taxes,
           payroll_taxes = federal_payroll_taxes + state_payroll_taxes,
           production_taxes = federal_production_taxes + state_production_taxes,
           personal_taxes = federal_personal_taxes + state_personal_taxes) %>% 
    mutate(across(where(is.numeric),
                  ~ coalesce(.x, 0))) %>% 
    get_non_corporate_taxes(),
  fim = 
    projections %>%
    add_factors() %>%
    get_overrides() %>%
    mutate(grants = consumption_grants + investment_grants,
           federal_purchases_deflator_growth = q_g(federal_purchases_deflator),
           state_purchases_deflator_growth = q_g(state_purchases_deflator),
           consumption_grants_deflator_growth = q_g(consumption_grants_deflator),
           investment_grants_deflator_growth = q_g(investment_grants_deflator),
           consumption_deflator_growth  =  q_g(consumption_deflator),
           real_potential_gdp_growth = q_g(real_potential_gdp)) %>% 
    purchases_contributions() %>% 
    mutate(social_benefits = social_benefits - ui - rebate_checks,
           federal_social_benefits = federal_social_benefits - federal_ui - rebate_checks,
           state_social_benefits = state_social_benefits - state_ui) %>% 
    taxes_transfers_minus_neutral() %>% 
    mpc_taxes_transfers() %>% 
      taxes_contributions() %>% 
    sum_taxes_contributions() %>% 
    transfers_contributions() %>% 
      sum_transfers_contributions() %>% 
    sum_taxes_transfers() %>% 
    get_fiscal_impact(),
  summary = 
    fim %>% 
      filter_index('2020 Q1' ~ '2021 Q1') %>% 
      select(date, id, fiscal_impact, federal_contribution, 
             state_contribution, grants_contribution, social_benefits_contribution,
             health_outlays_contribution, subsidies_contribution,
             ui_contribution, rebate_checks_contribution),
  levels =
    fim %>% 
      filter_index('2020 Q2' ~ '2021 Q1') %>% 
      select(date, id, fiscal_impact,  social_benefits_contribution,
             health_outlays, subsidies,
             ui_contribution, rebate_checks_contribution)
)
