


april <- 
  readxl::read_xlsx("results/4-2021/fim-4-2021.xlsx") %>%
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>%
  as_tsibble(index = date) %>%
  
  rename_with( ~ paste0(.x, 'ribution'), ends_with("cont")) %>%
  rename(
    federal_ui_contribution = federal_unemployment_insurance_contribution,
    federal_ui = federal_unemployment_insurance,
    state_ui = state_unemployment_insurance,
    state_contribution  = state_local_contribution,
    state_ui_contribution = state_unemployment_insurance_contribution,
    ui_contribution = unemployment_insurance_contribution,
    
    ui = unemployment_insurance,
    ui_minus_neutral = unemployment_insurance_minus_neutral,
    ui_post_mpc = unemployment_insurance_post_mpc
  ) %>%
  select(-date.y) %>%
  filter_index("2020 Q4" ~ .) %>%
  select(
    date,
    consumption_deflator_growth = pi_pce,
    rebate_checks_arp,
    contains('arp') ,
    real_potential_gdp_growth = gdppoth, gdp,
    federal_aid_to_small_businesses_arp_contribution = aid_to_small_businesses_contribution,
    federal_other_vulnrable_arp_contribution = other_vulnerable_contribution,
    federal_other_direct_aid_arp_contribution = other_direct_aid_contribution,
    
  ) %>%
  rename(federal_health_grants_arp_contribution = health_grants_arp_contribution,) %>%
 
  pivot_longer(-date)

  
  
package <- contribution %>% 
  
    select(-id)  %>% 
  select(contains('arp')) %>% 
    pivot_longer(-c(date, id))
    
 
combined <- inner_join(package, april, by = c('name', "date"), suffix = c('_package', '_april')) %>% 
   drop_na() %>% 
  filter_index("2021 Q2" ~ "2023 Q1") %>% 
   rename(value = value_package,
          april = value_april) %>% 
   mutate(diff = round(value - april, 4)) %>%
   mutate(across(c(value, april),
                 ~ round(.x, 3))) 

combined %>% 
  filter(diff != 0) %>% View()
 
  
  