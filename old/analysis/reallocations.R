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

reallocate <- function(.data, from, vars, to = NULL){
  series <-  enquos(vars)
  
  if(is_tsibble(.data)){
    index <- index_var(.data)
    key <- key_vars(.data)
  }
  
if(missing(to)){
  .data %>% 
    as_tibble() %>% 
    mutate(date,
              "{{ from }}" := {{ from }} - reduce(select(., c(!!!series)), `+`)) %>% 
    as_tsibble(key = key, index = index)
} else{
  .data %>% 
    as_tibble() %>% 
    mutate(date,
              "{{ from }}" := {{ from }} - reduce(select(., c(!!!series)), `+`),
           "{{ to }}" := {{ to }} + reduce(select(., c(!!!series)), `+`)) %>% 
    as_tsibble(key = key, index = index)
}
 
  
}
read_data() %>%
  define_variables() %>%
  
  growth_assumptions() %>% 
  
  reallocate(from = state_social_benefits, 
             vars = c(medicaid, medicare), 
             to = manu) %>%
  reallocate(from = federal_social_benefits,
             vars = c(rebate_checks, ui, medicare)) %>% 
  reallocate(from = state_health_outlays,
             vars = medicaid_grants)
  reallocate(medicare, rebate_checks, ui,  from = federal_social_benefits) %>% 
  reallocate(, from = federal_subsidies) 
  
  mutate(x = purrr::reduce(list(gdp, federal_social_benefits, federal_subsidies), `+`), .keep = "used")

read_data() %>%
  define_variables() %>%
  as_tsibble(key = id, index = date) %>% 
  growth_assumptions() %>% select(state_social_benefits)
# Baseline projections ----------------------------------------------------

baseline_projections <-
  read_data() %>%
  define_variables() %>%
  as_tsibble(key = id, index = date) %>% 
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