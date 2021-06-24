
# Load previous months results
previous <-
  readxl::read_xlsx('results/5-2021/fim-5-2021.xlsx') %>%
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>%
  as_tsibble(index = date) %>%
  filter_index("2021 Q2" ~ "2023 Q1") %>% 
  mutate(federal_non_health_grants_arp = mpc_non_health_grants_arp(federal_non_health_grants_arp)) %>% 
  # mutate(federal_purchases = federal_purchases + federal_non_health_grants_arp,
  #        federal_purchases_contribution = federal_purchases_contribution + federal_non_health_grants_arp_contribution) %>% 
  mutate(federal_ui = federal_ui + federal_ui_arp,
         federal_ui_contribution = federal_ui_contribution + federal_ui_arp_contribution) %>%
   mutate(federal_health_outlays= federal_health_outlays + federal_health_grants_arp,
         federal_health_outlays_contribution = federal_health_outlays_contribution + federal_health_grants_arp_contribution) %>%
  as_tibble() %>%
  select(date, fiscal_impact, federal_contribution, federal_corporate_taxes_contribution,
         federal_non_corporate_taxes_contribution, federal_health_outlays_contribution,
         federal_ui_contribution,  rebate_checks_contribution, rebate_checks_arp_contribution,
         federal_other_vulnerable_arp_contribution, federal_other_direct_aid_arp_contribution,
         federal_social_benefits_contribution, federal_subsidies_contribution, federal_aid_to_small_businesses_arp_contribution,
         
         state_contribution, state_corporate_taxes_contribution, state_non_corporate_taxes_contribution,
         state_health_outlays_contribution, state_ui_contribution, state_subsidies_contribution, state_social_benefits_contribution
         ) 




previous %>% 
  openxlsx::write.xlsx('temp.xlsx')



#
current <- contributions %>%
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>%
  as_tsibble(index = date) %>%
  filter_index("2021 Q2" ~ "2023 Q4") %>% 
  as_tibble() %>% 
  select(date, fiscal_impact, federal_contribution, federal_corporate_taxes_contribution,
         federal_non_corporate_taxes_contribution, federal_health_outlays_contribution,
         federal_ui_contribution, rebate_checks_contribution, rebate_checks_arp_contribution,
         federal_other_vulnerable_arp_contribution, federal_other_direct_aid_arp_contribution,
         federal_social_benefits_contribution, federal_subsidies_contribution, federal_aid_to_small_businesses_arp_contribution,
         
         state_contribution, state_corporate_taxes_contribution, state_non_corporate_taxes_contribution,
         state_health_outlays_contribution, state_ui_contribution, state_subsidies_contribution, state_social_benefits
  )

contributions %>% 
  select(ends_with('contribution')) %>% names()

current %>% 
  writexl::write_xlsx('temp.xlsx')


previous_long <- pivot_longer(previous, cols = where(is.numeric), values_to = 'previous')
current_long <- pivot_longer(current, cols = where(is.numeric), values_to = 'current')

comparison <- inner_join(previous_long,
                         current_long,
                         by = c('date', 'name')) %>% 
  rename(variable = name) %>%
  pivot_longer(c(previous, current),
               values_to = 'value',
               names_to = 'source')

comparison %>% 
  pivot_wider(names_from = date,
              values_from = value) %>% 
  arrange(source) %>% 
  openxlsx::write.xlsx('contributions_comparison.xlsx')

total <- inner_join(previous_long,
           current_long,
           by = c('date', 'name')) %>%
  mutate(difference = current - previous,
         variable = name) %>% 
  pivot_longer(where(is.numeric),
               names_to = 'type')

total %>% 
  filter(type == 'previous') %>% 
  pivot_wider(names_from = date,
              values_from = value) %>% 
  openxlsx::write.xlsx('previous.xlsx')

total %>% 
  filter(type == 'current') %>%
  pivot_wider(names_from = date,
              values_from = value) %>% 
  openxlsx::write.xlsx('current.xlsx')

total %>% 
  filter(type == 'difference') %>% 
  pivot_wider(names_from = date,
              values_from = value) %>% 
  openxlsx::write.xlsx('difference.xlsx')






  

