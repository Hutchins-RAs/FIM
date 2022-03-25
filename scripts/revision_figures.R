# Load previous months results
previous <-
  readxl::read_xlsx(glue('results/{last_month_year}/fim-{last_month_year}.xlsx')) %>%
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>%
  as_tsibble(index = date) %>%
  filter_index("2020 Q2" ~ as.character(current_quarter + 9)) %>% 
  mutate(subsidies = federal_subsidies + state_subsidies,
         subsidies_contribution = federal_subsidies_contribution + state_subsidies_contribution)
# Select current results
current <- 
  contributions %>%
  drop_na(date) %>%
  filter_index("2020 Q2" ~ as.character(current_quarter + 9)) %>% 
  as_tibble() %>%
  select(
    date,
    fiscal_impact,
    federal_contribution,
    grants_contribution,
    federal_corporate_taxes_contribution,
    federal_non_corporate_taxes_contribution,
    federal_health_outlays_contribution,
    federal_ui_contribution,
    rebate_checks_contribution,
    rebate_checks_arp_contribution,
    federal_other_vulnerable_arp_contribution,
    federal_other_direct_aid_arp_contribution,
    federal_social_benefits_contribution,
    federal_subsidies_contribution,
    federal_aid_to_small_businesses_arp_contribution,
    
    state_contribution,
    state_corporate_taxes_contribution,
    state_non_corporate_taxes_contribution,
    state_health_outlays_contribution,
    state_ui_contribution,
    state_subsidies_contribution,
    state_social_benefits,
    gdp,
    real_potential_gdp_growth,
    federal_purchases_deflator_growth,
    state_purchases_deflator_growth,
    cpiu,
    consumption_deflator_growth
  ) %>% 
  as_tsibble(index = date) 

# contributions %>% 
#   select(date, contains('deflator')) %>% 
#   filter_index("2021 Q2" ~ "2023 Q4") %>% 
#   mutate(across(ends_with('growth'),
#                  ~ ((1 + .x)^4))-1) %>% 
#   View()
# 
# previous %>% 
#   select(date, contains('deflator')) %>% 
#   filter_index("2021 Q2" ~ "2023 Q4") %>% 
#   mutate(across(ends_with('growth'),
#                 ~ ((1 + .x)^4))-1) %>% 
#   View()
# 


# Pivot both longer
previous_long <- pivot_longer(previous, cols = where(is.numeric), values_to = 'previous')
current_long <- pivot_longer(current, cols = where(is.numeric), values_to = 'current')

# Merge and compare
comparison <- inner_join(previous_long,
                         current_long,
                         by = c('date', 'name'))

comparison_wide <-
  comparison %>% 
  filter(date >= yearquarter("2021 Q2")) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  mutate(difference = current - previous,
         across(where(is.numeric),
                round,
                digits = 4)) %>% 
  pivot_longer(where(is.numeric),
               names_to = 'source') %>% 
  arrange(source) %>% 
  pivot_wider(names_from = date,
              values_from = value) %>%
  mutate(name = snakecase::to_title_case(name)) 

comparison_deflators <-
  comparison %>% 
  select(date, contains('deflator'), previous, current) %>% 
  filter(date >= yearquarter("2021 Q2")) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  mutate(difference = current - previous,
         across(where(is.numeric),
                round,
                digits = 4)) %>% 
  pivot_longer(where(is.numeric),
               names_to = 'source') %>% 
  arrange(source) %>% 
  pivot_wider(names_from = date,
              values_from = value) %>%
  mutate(name = snakecase::to_title_case(name)) 

openxlsx::write.xlsx(x = comparison_deflators,
                     file = glue('results/{month_year}/comparison-deflators-{month_year}.xlsx'),
                     overwrite = TRUE)
openxlsx::write.xlsx(x = comparison_wide,
                     file = glue('results/{month_year}/contributions-comparison-{month_year}.xlsx'),
                     overwrite = TRUE)


# Figures -----------------------------------------------------------------

# Load previous months results
previous <-
  readxl::read_xlsx(glue('results/{last_month_year}/fim-{last_month_year}.xlsx')) %>%
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>%
  as_tsibble(index = date) %>%
  filter_index("2020 Q1" ~ as.character(current_quarter + 8)) %>% 
  select(-id) %>% 
  mutate(subsidies = federal_subsidies + state_subsidies,
         subsidies_contribution = federal_subsidies_contribution + state_subsidies_contribution)
# Select current results
current <- 
  contributions %>%
  drop_na(date) %>%
  filter_index("2020 Q1" ~ as.character(current_quarter + 8)) %>% 
  as_tsibble(index = date) %>% 
  select(-id)

previous_long <- pivot_longer(previous, cols = where(is.numeric), values_to = 'previous')
current_long <- pivot_longer(current, cols = where(is.numeric), values_to = 'current')

comparison <- inner_join(current_long,
                         previous_long,
                         by = c('date', 'name')) %>% 
  rename(variable = name) %>% 
  as_tsibble(index = date) %>% 
  select(-id)

comparison_long <-
  comparison %>% 
  pivot_longer(c(previous, current),
               names_to = 'source') 


components <- c(
  "fiscal_impact",
  "federal_purchases_contribution",
  "state_purchases_contribution",
  "federal_purchases",
  "state_purchases",
  "consumption_grants_contribution",
  "investment_grants_contribution",
  "consumption_grants",
  "investment_grants",
  "federal_contribution",
  "state_contribution",
  "federal_corporate_taxes_contribution",
  "state_corporate_taxes_contribution",
  "federal_corporate_taxes",
  "state_corporate_taxes",
  "federal_non_corporate_taxes_contribution",
  "state_non_corporate_taxes_contribution",
  "federal_non_corporate_taxes",
  "state_non_corporate_taxes",
  "transfers_contribution",
  "federal_transfers_contribution",
  "state_transfers_contribution",
  "federal_health_outlays_contribution",
  "state_health_outlays_contribution",
  "federal_health_outlays",
  "state_health_outlays",
  "medicaid",
  "medicaid_grants",
  "medicare",
  "subsidies_contribution",
  "subsidies",
  "federal_aid_to_small_businesses_arp_contribution",
  "federal_aid_to_small_businesses_arp",
  "federal_ui_contribution",
  "state_ui_contribution",
  "federal_ui",
  "state_ui",
  "federal_other_vulnerable_arp_contribution",
  "federal_other_vulnerable_arp",
  "rebate_checks_contribution",
  "rebate_checks",
  "rebate_checks_arp_contribution",
  "rebate_checks_arp",
  "federal_other_direct_aid_arp_contribution",
  "federal_other_direct_aid_arp",
  "federal_social_benefits_contribution",
  "state_social_benefits_contribution",
  "federal_social_benefits",
  "state_social_benefits"
)

comparison_nested <-
  comparison_long %>%
  filter(variable %in% components) %>% 
  group_by(variable) %>%
  nest() %>%
  mutate(plot = map2(.x = variable,
                     .y = data,
                     .f = ~comparison_plot(.data = .y,
                                           variable = .x)))


write_rds(comparison_nested, 'data/comparison_nested')
plots <- comparison_nested %>% 
  pivot_wider(id_cols = -data, 
              names_from = 'variable',
              values_from = 'plot')
write_rds(plots, 'data/plots')

plots <- rlang::set_names(comparison_nested$plot, comparison_nested$variable)
