deficit  <- usna %>% 
  filter_index("1970 Q4" ~ "2023 Q1") %>% 
  
  summarise(date,
            gdp,
            federal_taxes = federal_corporate_taxes + federal_non_corporate_taxes,
            federal_transfers = federal_social_benefits  +
              federal_subsidies,
            federal_personal_taxes,
            federal_purchases,
            consumption_grants = gross_consumption_grants - medicaid_grants,
            grants = consumption_grants + investment_grants) %>% 
  mutate(federal_outlays = federal_transfers + federal_purchases + grants,
         federal_deficit =   federal_outlays - federal_taxes,
         .after = 'date') 

deficit %>% 
  
  pivot_longer(c( federal_outlays, federal_taxes)) %>% 
  mutate(value = value / gdp) %>% 
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_percent())

  
  deficit %>% 
    filter_index("2011 Q4" ~ "2020 Q4") %>% 
    separate(date, c('year', 'quarter'), sep = ' ') %>% 
    mutate(year = lead(year)) %>% 
    group_by(year) %>% 
    summarise(across(where(is.numeric), ~ mean(.x)))
  
  
  deficit %>% 
    filter_index("2018 Q1" ~ "2020 Q4") 
    
  