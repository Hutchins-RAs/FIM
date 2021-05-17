# Setup -------------------------------------------------------------------
librarian::shelf(
  'tidyverse',
  'zoo',
  'TTR',
  'tsibble',
  'targets',
  'tarchetypes',
  'lubridate',
  'alistaire47/pipecleaner',
  'glue',
  'validate',
  'fim',
  'dplyover',
  'tsibble',
  'magrittr',
  'feasts',
  'fable'
)

# Baseline projections ----------------------------------------------------

baseline_projections <-
  read_data() %>%
  define_variables() %>%
  as_tsibble(key = id, index = date) %>% 
  create_override(
    var = state_purchases_growth,
    start = yearquarter('2020 Q4'),
    end = yearquarter('2022 Q1'),
    values = c(rep(0.0025, 3), 0.005, 0.0075, 0.01)
  )  %>% 
  growth_assumptions() %>%
  reallocate_legislation() %>% 
  mutate(
    across(c(ppp, aviation, paid_sick_leave, employee_retention),
           ~ coalesce(.x, 0)),
    federal_subsidies = federal_subsidies - ppp - aviation - paid_sick_leave - employee_retention,
    subsidies = federal_subsidies + state_subsidies
  ) %>% 

  ungroup() %>% 
  forecast2(gdp, federal_purchases, state_purchases) %>% 
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
  get_non_corporate_taxes() %>% 
  as_tsibble(key = id, index = date)

baseline_projections %>% 
  autoplot(federal_purchases)

  

# Projections with add factors --------------------------------------------


add_factors <-
  readxl::read_xlsx('data/add_factors.xlsx',
                    sheet = "FIM Add Factors")  %>% 
  mutate(date = tsibble::yearquarter(date))

arp <- readxl::read_xlsx('data/arp_summary.xlsx') %>% 
  mutate(date = yearquarter(date))

projections <-
  baseline_projections %>% 
  left_join(add_factors, by = 'date') %>% 
  mutate(across(.cols = starts_with('add'),
                .fns = ~ if_else(id == 'historical',
                                 0,
                                 .x)))  %>% 
  mutate(across(c(consumption_grants,
                  federal_purchases,
                  state_purchases,
                  federal_social_benefits,
                  state_social_benefits,
                  federal_subsidies,
                  federal_health_outlays,
                  state_health_outlays),
                ~ .x + get(paste0('add_', cur_column())))) %>% 
  mutate(consumption_grants = if_else(date >= yearquarter('2020 Q2'),
                                      consumption_grants_override,
                                      consumption_grants)) %>% 
  left_join(arp, by = 'date')

projections %>% 
autoplot(vars(gdp, real_potential_gdp)) +
  gghutchins::scale_color_hutchins()+
  gghutchins::theme_hutchins()

projections %>% 
  autoplot(vars(federal_social_benefits,state_social_benefits, gdp))
  gghutchins::scale_color_hutchins()

if (requireNamespace("fable", quietly = TRUE)) {
  library(fable)
  library(tsibbledata)
  library(tsibble)
  
  tsibbledata::gafa_stock %>%
    autoplot(vars(Close, log(Close)))
}

# Contributions -----------------------------------------------------------


contributions <-
  projections %>% 
  purchases_contributions() %>%
  taxes_transfers_minus_neutral() %>% 
  calculate_mpc('social_benefits') %>% 
  calculate_mpc('ui') %>%  
  calculate_mpc('subsidies') %>% 
  calculate_mpc('health_outlays') %>% 
  calculate_mpc('corporate_taxes') %>% 
  calculate_mpc('non_corporate_taxes') %>% 
  mutate(rebate_checks_post_mpc  = mpc_rebate_checks(rebate_checks_minus_neutral)) %>% 

  taxes_contributions() %>%
  sum_taxes_contributions() %>%
  transfers_contributions() %>%
  sum_transfers_contributions() %>%
  sum_taxes_transfers() %>%
  get_fiscal_impact() %>%
  as_tsibble(key = id, index = date)
 