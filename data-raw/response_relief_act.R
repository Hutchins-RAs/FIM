## code to prepare `response_relief_act` dataset goes here
library('readxl')
librarian::shelf('tidyverse')
rra <- read_xlsx('inst/extdata/december_2020_stimulus.xlsx')


rra %>% 
  pivot_longer(-date) %>% 
  mutate(
    component = case_when(
      # Social benefits
      name %in% c(
        'rebate_checks',
        'snap',
        'eitc',
        'puc_extension',
        'nonprofit_ppp',
        'nonprofit_provider_relief_fund'
      ) ~ 'Social benefits',
      # Subsidies
      name %in% c(
        'ppp',
        'employee_retention',
        'business_meals_deduction',
        'aviation'
      ) ~ 'Subsidies',
      # Grants
      name %in% c(
        'transportation',
        'education_stabilization_fund',
        'provider_relief_fund',
        'other_grants'
      ) ~ 'Grants'
    )
  ) %>% 
  group_by(date, component) %>%
  pivot_wider(names_from = c(name), values_from = 'value') %>% 
  nest(where(is.numeric)) %>% 
  arrange(date) %>% 
  mutate(response_relief_act = map_dbl(data, sum, na.rm = TRUE)) %>% 
  pivot_wider(id = date,
              names_from = component,
              names_prefix = 'response_relief_act',
              values_from = response_relief_act) %>% 
  janitor::clean_names() 
usethis::use_data(rra, overwrite = TRUE)
