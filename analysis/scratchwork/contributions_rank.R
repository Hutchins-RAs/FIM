librarian::shelf(tidyverse, tsibble)
contributions <- readRDS('data/contributions.RDS')
df

# coutnerfactual <- function(df, var, deflator){
#   lag({{var}})
# }
# 
# contributions %>% 
#   mutate(x = counterfactual(gdp))
# contribution <- function(df, var){
#   mutate(df,
#          x = {{var}} / lag(.data$gdp))
# }
# contributions %>% 
#   contribution(federal_purchases) %>% 
#   mutate_where(date == yearquarter('2021 Q2'),
#                federal_purchases = 10) 
#   
    
 summary <- contributions %>% 
   filter_index("2022 Q1") %>% 
   as_tibble() %>% 
    select(date, fiscal_impact, ends_with('contribution'), 
           -contains('transfers'), -c(social_benefits_contribution, taxes_contribution,
                                      health_outlays_contribution, federal_purchases_contribution, state_purchases_contribution, consumption_grants_contribution, investment_grants_contribution, grants_contribution, subsidies_contribution)) %>%
   pivot_longer(where(is.numeric)) %>% arrange(desc(abs(value))) %>% 
   print(n = 34)
   
summary %>% 
  filter(name != 'fiscal_impact') %>% 
  summarise(positive = sum(value[value>0]), Negative = sum(value[value <0]))
 


