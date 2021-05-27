
mpc_arp <- tribble(
  ~ variable, ~mpc,
  "rebate_checks", "mpc_direct_aid_arp",
  "other_direct_aid", "mpc_direct_aid_arp",
  "health_grants", "mpc_health_outlays",
  "non_health_grants", "mpc_non_health_grants_arp",
  "other_vulnerable", "mpc_vulnerable_arp",
  
  "ui", "mpc_ui_arp",
  "aid_to_small_businesses", "mpc_small_businesses_arp")

arp_long <- 
  readxl::read_xlsx('data/arp_summary.xlsx') %>% 
  mutate(date = yearquarter(date)) %>% 
  pivot_longer(starts_with(c('federal', 'state')),
               names_to = c('government', 'variable', 'legislation'),
               names_pattern = '(federal|state)_(.*)_(arp)',
               values_to = 'values') %>% 
  as_tsibble(index = date, key = c(legislation, government, variable)) %>%   append_row(-1L) %>% 
  mutate(values = coalesce(values, 0)) %>% 
  
  full_join(baseline_projections %>% filter_index("2020 Q4" ~ .) %>% select(date, real_potential_gdp_growth, consumption_deflator_growth, consumption_grants_deflator_growth, gdp),
            by = 'date') %>% 
  group_by(variable) %>% 
  mutate(counterfactual = lag(values,default = 0 ) * (1 + real_potential_gdp_growth + consumption_deflator_growth)) %>% 
  mutate_where(variable == 'non_health_grants',
               counterfactual =  lag(values,default = 0 ) * (1 + real_potential_gdp_growth + consumption_deflator_growth)) %>% 
  left_join(mpc_arp, by = 'variable') %>% 
  mutate(consumption = invoke_map_dbl(mpc, values),
         counterfactual_consumption = invoke_map_dbl(mpc, counterfactual)) %>% 
  mutate(contribution =400 * (consumption - counterfactual_consumption ) / lag(gdp, default = 0))


arp_wide <-
  arp_long %>%
  as_tibble() %>% 
  
  pivot_wider(
    id_cols = date,
    names_from = c(government, variable, legislation),
    names_glue = "{government}_{variable}_{legislation}_{.value}",
    values_from = c(values, contribution)
  ) %>% 
  rename_with(~ stringr::str_remove(.x, "_values")) %>% 
  rename(rebate_checks_arp = federal_rebate_checks_arp,
         rebate_checks_arp_contribution = federal_rebate_checks_arp_contribution)



arp_contribution <-
  arp_wide %>% 
  select(date, ends_with('contribution')) %>% 
  mutate(date,
         consumption_grants_arp_contribution = federal_non_health_grants_arp_contribution,
         federal_health_outlays_arp_contribution = federal_health_grants_arp_contribution,
         federal_social_benefits_arp_contribution = federal_other_direct_aid_arp_contribution + federal_other_vulnerable_arp_contribution,
         federal_subsidies_arp_contribution = federal_aid_to_small_businesses_arp_contribution,
         rebate_checks_arp_contribution,
         federal_ui_arp_contribution,
         state_ui_arp_contribution) %>% 
  summarise(
    date,
    consumption_grants_arp_contribution,
    federal_transfers_arp_contribution = 
      federal_social_benefits_arp_contribution +
      federal_subsidies_arp_contribution +
      federal_health_outlays_arp_contribution + 
      federal_ui_arp_contribution +
      rebate_checks_arp_contribution,
    state_transfers_arp_contribution = state_ui_arp_contribution)



arp_long <- 
  readxl::read_xlsx('data/arp_summary.xlsx') %>% 
  mutate(date = yearquarter(date)) %>% 
  pivot_longer(starts_with(c('federal', 'state')),
               names_to = c('government', 'variable', 'legislation'),
               names_pattern = '(federal|state)_(.*)_(arp)',
               values_to = 'values') %>% 
  as_tsibble(index = date, key = c(legislation, government, variable)) %>%   append_row(-1L) %>% 
  mutate(values = coalesce(values, 0)) %>% 
  
  full_join(baseline_projections %>% filter_index("2020 Q4" ~ .) %>% select(date, real_potential_gdp_growth, consumption_deflator_growth, consumption_grants_deflator_growth, gdp),
            by = 'date') %>% 
  group_by(variable) %>% 
  mutate(counterfactual = lag(values,default = 0 ) * (1 + real_potential_gdp_growth + consumption_deflator_growth)) %>% 
  mutate_where(variable == 'non_health_grants',
               counterfactual =  lag(values,default = 0 ) * (1 + real_potential_gdp_growth + consumption_deflator_growth)) %>% 
  left_join(mpc_arp, by = 'variable')

arp_long %>% 
  summarise(date,
            values,
            mpc,
            calc = get(mpc)(values))
  
  
  mutate(plot = map2(.x = variable,
                     .y = data,
                     .f = ~comparison_plot(.data = .y, 
                                           variable = .x)))
