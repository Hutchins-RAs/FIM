
april <- readxl::read_xlsx("results/4-2021/fim-4-2021.xlsx") %>% 
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>% 
  as_tsibble(index = date) %>% 
  rename_with(~paste0(.x, 'ribution'), ends_with("cont")) %>% 
  rename(federal_ui_contribution = federal_unemployment_insurance_contribution,
         federal_ui = federal_unemployment_insurance,
         federal_ui_post_mpc = federal_unemployment_insurance_post_mpc,
         federal_ui_minus_neutral = federal_unemployment_insurance_minus_neutral,
         state_ui = state_unemployment_insurance,
         
         
         state_ui_contribution = state_unemployment_insurance_contribution,
         ui_contribution = unemployment_insurance_contribution,
         
         federal_aid_to_small_businesses_arp = aid_to_small_businesses,
        state_contribution = state_local_contribution,
        state_purchases = state_local_nom,
        federal_purchases_contribution = federal_nom_contribution,
        consumption_grants_contribution = federal_cgrants_contribution,
        federal_non_health_grants_arp_contribution = non_health_grants_contribution,
        state_purchases_contribution = state_local_nom_contribution, 
         ui = unemployment_insurance,
         ui_minus_neutral = unemployment_insurance_minus_neutral,
         ui_post_mpc = unemployment_insurance_post_mpc) %>% 
  mutate(federal_subsidies = federal_subsidies - federal_aid_to_small_businesses_arp,
         federal_health_outlays = federal_health_outlays -  health_grants_arp ,
         state_social_benefits = state_social_benefits - state_ui - state_ui_arp -add_state_social_benefits)
  

april_long <-
  april %>% 
  select(-date.y, -id) %>% 
  pivot_longer(-c(date),
               values_to = "april")

contribution %>% 

  summarise(date,
      id,
      # 
      fiscal_impact,
      # federal_taxes_contribution,
      # state_taxes_contribution,
      # taxes_contribution,
      # federal_social_benefits_contribution,
      # federal_ui_contribution = federal_ui_contribution + federal_ui_arp_contribution,
      # rebate_checks_contribution,
      # federal_health_outlays_contribution,
      # federal_subsidies_contribution,
       state_social_benefits_contribution = state_social_benefits_contribution ,
      state_social_benefits = state_social_benefits - add_state_social_benefits ,
      add_state_social_benefits,
       state_ui ,
      state_ui_arp,
      state_health_outlays,
      wages_lost_assistance
       
      # state_subsidies_contribution,
      # state_transfers_contribution,
      # transfers_contribution,
      # federal_contribution,
      # state_purchases_contribution,
      # consumption_grants_contribution,
      # federal_non_health_grants_arp_contribution,
      #state_contribution
      
      ) %>%
  filter_index("2021 Q2" ~ "2021 Q3") %>% 
  pivot_longer(-c(date, id)) %>%
  left_join(april_long, by = c("date", "name")) 



contribution %>% 
  
  summarise(date,
            id,
fiscal_impact = fiscal_impact + state_ui_arp_contribution

  ) %>%
  filter_index("1999 Q4" ~ "2023 Q1")  %>% 
  
  pivot_longer(-c(date, id)) %>%
  left_join(april_long, by = c("date", "name")) %>% 
  mutate(diff = round(value  - april, 5)) %>% View()
