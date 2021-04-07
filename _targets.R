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

    #  Override growth rates
    create_override(
      var = state_purchases_growth,
      start = yearquarter('2020 Q4'),
      end = yearquarter('2022 Q1'),
      values = c(rep(0.0025, 3), 0.005, 0.0075, 0.01)
    )  %>%
    create_override(
      var = federal_social_benefits_growth,
      start = yearquarter('2021 Q1'),
      end = yearquarter('2022 Q3'),
      values = c(rep(-0.0075, 3), rep(0.015,4))
    ) %>% 
    growth_assumptions() %>%
    reallocate_legislation() %>%  
    mutate(
      across(c(ppp, aviation, paid_sick_leave, employee_retention),
             ~ coalesce(.x, 0)),
      federal_subsidies = federal_subsidies - ppp - aviation - paid_sick_leave - employee_retention,
      subsidies = federal_subsidies + state_subsidies
    ) %>% 
    forecast() %>%
    ungroup() %>% 
    mutate(# Health outlays reattribution
           health_outlays = medicare + medicaid,
           federal_health_outlays = medicare + medicaid_grants,
           state_health_outlays = medicaid - medicaid_grants,
           # Aggregate taxes
           corporate_taxes = federal_corporate_taxes + state_corporate_taxes,
           payroll_taxes = federal_payroll_taxes + state_payroll_taxes,
           production_taxes = federal_production_taxes + state_production_taxes,
           personal_taxes = federal_personal_taxes + state_personal_taxes,
           # Coalesce NA's to 0
           across(where(is.numeric),
                  ~ coalesce(.x, 0))) %>% 
    get_non_corporate_taxes(),
  
  fim = 
    projections %>%
    
    mutate(
      consumption_grants = consumption_grants + education_stabilization_fund + provider_relief_fund + coronavirus_relief_fund,
      federal_social_benefits = federal_social_benefits + nonprofit_provider_relief_fund + nonprofit_ppp ,
      federal_subsidies = federal_subsidies + aviation + ppp + employee_retention + paid_sick_leave ,
      subsidies = federal_subsidies + state_subsidies,
      grants = consumption_grants + investment_grants,
      across(
        .cols = ends_with('deflator') | ends_with('real_potential_gdp'),
        .fns = ~ q_g(.x),
        .names = '{.col}_growth'
      )
    ) %>%
    purchases_contributions() %>%
    taxes_transfers_minus_neutral() %>%
    mpc_taxes_transfers() %>%
    taxes_contributions() %>%
    sum_taxes_contributions() %>%
    transfers_contributions() %>%
    sum_transfers_contributions() %>%
    sum_taxes_transfers() %>%
    get_fiscal_impact() %>% 
  as_tsibble(index = date),
  summary = 
    fim %>% 
      filter_index('2020 Q1' ~ '2021 Q1') %>% 
      select(date, id, fiscal_impact, federal_contribution, 
             state_contribution, grants_contribution, social_benefits_contribution,
             health_outlays_contribution, subsidies_contribution,
             ui_contribution, rebate_checks_contribution),
  levels =
    fim %>% 
      filter_index('2019 Q2' ~ '2022 Q4') %>% 
      select(date, id,   federal_social_benefits,
             federal_health_outlays, federal_subsidies,
            federal_ui, rebate_checks, consumption_grants, federal_purchases)
)
