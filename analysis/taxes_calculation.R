comparison
inner_join(previous_long, 
           current_long,
           by = c('date', 'name', 'id')) %>% 
  openxlsx::write.xlsx('comparison.xlsx')


tax <- 
  contribution  %>% 
  filter_index("2016 Q4" ~ .) %>% 
  select(date, federal_non_corporate_taxes, consumption_deflator_growth, real_potential_gdp_growth, gdp)

tax %>% 
  mutate(counterfactual = lag(federal_non_corporate_taxes) * (1 + consumption_deflator_growth + real_potential_gdp_growth) ) %>% 
  mutate(post_mpc = mpc_non_corporate_taxes(federal_non_corporate_taxes) - mpc_non_corporate_taxes(counterfactual),
         .after = 'date') %>% 
  
  mutate(contribution = 400 * (post_mpc) / lag(gdp), .after = 'date') %>% 
  filter_index("2020 Q4" ~ .)


tax %>% 
  mutate_where(date == yearquarter('2020 Q4'),
               federal_non_corporate_taxes = 3314.3) %>% 
  mutate_where(date == yearquarter("2021 Q1"),
               federal_non_corporate_taxes = 3359) %>% 
  mutate(counterfactual = lag(federal_non_corporate_taxes) * (1 + consumption_deflator_growth + real_potential_gdp_growth) ) %>%
  mutate(federal_non_corporate_taxes_spending = mpc_non_corporate_taxes(federal_non_corporate_taxes),
         counterfactual_spending = mpc_non_corporate_taxes(counterfactual), .after = 'date') %>% 
  
  mutate(net = federal_non_corporate_taxes_spending - counterfactual_spending,
         .after = 'date') %>% 
  
  mutate(contribution = 400 * (net) / lag(gdp), .after = 'date') %>% 
  relocate(gdp) %>% 
  filter_index("2020 Q4" ~ .)


