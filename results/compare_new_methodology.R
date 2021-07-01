old <- readRDS('data/contribution.RDS') %>% 
  mutate(federal_ui_contribution = federal_ui_contribution + federal_ui_arp_contribution,
         federal_ui = federal_ui + federal_ui_arp,
         federal_health_outlays = federal_health_outlays + federal_health_grants_arp,
         federal_health_outlays_contribution = federal_health_outlays_contribution + 
           federal_health_grants_arp_contribution,
         federal_purchases = federal_purchases + federal_non_health_grants_arp,
         federal_purchases_contribution = federal_purchases_contribution + federal_non_health_grants_arp_contribution) 
new <- contributions


old_cont <- old %>% 
  select(date, fiscal_impact, ends_with('contribution')) %>% 
  pivot_longer(where(is.numeric)) %>% 
  filter_index("2021 Q2")

new_cont <- new %>% 
  select(date, fiscal_impact, ends_with('contribution')) %>% 
  pivot_longer(where(is.numeric)) %>% 
  filter_index("2021 Q2")

left_join(old_cont, new_cont, by = c('date', 'name'),
          suffix = c('_old', '_new')) %>% 
  drop_na() %>% 
  as_tibble() %>% 
  filter(name %notin% c('state_contribution',
                        'federal_contribution',
                        'transfers_contribution',
                        'federal_transfers_contribution',
                        'state_transfers_contribution',
                        'non_corporate_taxes_contribution',
                        'corrporate_taxes_contribution',
                        'taxes_contribution',
                        'social_benefits_contribution',
                        'health_outlays_contribution',
                        'social_benefits_contribution',
                        'grants_contribution')) %>% 
  select(-id_new, -id_old) %>% 
  mutate(diff = value_old - value_new) %>% 
  arrange(desc(abs(diff))) %>% 
  print(n = 51, digits = 2)


old_levels <-
  old %>% 
  mutate(federal_purchases = federal_purchases) %>% 
  pivot_longer(where(is.numeric)) 

new_levels <- 
  new %>% 
 
  pivot_longer(where(is.numeric))


left_join(old_levels, new_levels, by = c('date', 'name'),
          suffix = c('_old', '_new')) %>% 
  filter_index("2021 Q2") %>% 
  drop_na() %>% 
  as_tibble() %>% 
  select(-id_new, -id_old) %>% 
  mutate(diff = value_old - value_new) %>% 
  filter(name %in% c('consumption_grants',
                     'fiscal_impact',
                     'federal_purchases',
                     'state_purchases',
                     'federal_health_outlays',
                     'state_health_outlays',
                     'federal_corporate_taxes',
                     'state_corporate_taxes',
                     'federal_non_corporate_taxes',
                     'state_non_corporate_taxes',
                     'rebate_checks',
                     'rebate_checks_arp',
                     'federal_ui',
                     'state_ui',
                     'state_social_benefits',
                     'federal_social_benefits')) %>% 
  arrange(desc(abs(diff))) %>% 

  print(n = 51)

