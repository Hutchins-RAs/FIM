# Setup -------------------------------------------------------------------
librarian::shelf(
  'tidyverse',
  'zoo',
  'TTR',
  'tsibble',
  'targets',
  'tarchetypes',
  'lubridate',
  'alistaire47/pipecleaner',
  'glue',
  'validate',
  'fim',
  'dplyover',
  'tsibble',
  'magrittr',
  'feasts',
  'fable'
)


# Wrangle data ------------------------------------------------------------
usna_processed <-
  read_data() %>%
  # Don't sneak in ui reallocation here.
  define_variables() %>%
  as_tsibble(key = id, index = date) %>% 
  create_override(
    var = state_purchases_growth,
    start = yearquarter('2020 Q4'),
    end = yearquarter('2022 Q1'),
    values = c(rep(0.0025, 3), 0.005, 0.0075, 0.01)
  )  %>% 
  growth_assumptions() %>%
  mutate(across(ends_with('deflator'),
                ~ q_g(.x),
                .names = "{.col}_growth")) %>% 
  mutate(real_potential_gdp_growth = q_g(real_potential_gdp)) %>% 
  mutate(consumption_grants = gross_consumption_grants - medicaid_grants,
         grants = consumption_grants + investment_grants,
         federal_social_benefits = federal_social_benefits - medicare,
         state_social_benefits =  state_social_benefits - medicaid + state_ui,
         social_benefits = federal_social_benefits + state_social_benefits) 



# Forecast ----------------------------------------------------------------
baseline_projections <-
  usna_processed %>% 
  forecast() %>% 
ungroup() %>% 
mutate(# Health outlays reattribution
  health_outlays = medicare + medicaid,
  federal_health_outlays = medicare + medicaid_grants,
  state_health_outlays = medicaid - medicaid_grants,
  # Aggregate taxes
  corporate_taxes = federal_corporate_taxes + state_corporate_taxes,
  payroll_taxes = federal_payroll_taxes + state_payroll_taxes,
  production_taxes = federal_production_taxes + state_production_taxes,
  personal_taxes = federal_personal_taxes + state_personal_taxes,
  # Coalesce NA's to 0
  across(where(is.numeric),
         ~ coalesce(.x, 0)))
  
add_factors <-
  readxl::read_xlsx('data/add_factors.xlsx',
                    sheet = "FIM Add Factors")  %>% 
  mutate(date = tsibble::yearquarter(date))
# ARP -------------------------------------------------------------
fun <- tribble(
  ~ variable, ~mpc,
  'social_benefits' , 'mpc_social_benefits', 
  'health_outlays' , 'mpc_health_outlays',
  'subisidies' , 'mpc_subsidies',
  'ui' , 'mpc_ui',
  'corporate_taxes' , 'mpc_corporate_taxes',
  'purchases', 'identity',
  'consumption_grants', 'identity'
)

mpc_arp <- tribble(~ variable, ~mpc,
                   "rebate_checks", "mpc_direct_aid",
                   "other_direct_aid", "mpc_direct_aid",
                   "ui", "mpc_ui",
                   "aid_to_small_businesses", "mpc_aid_to_small_businesses")

arp_long <- readxl::read_xlsx('data/arp_summary.xlsx') %>% 
  mutate(date = yearquarter(date)) %>% 
  pivot_longer(starts_with(c('federal', 'state')),
              names_to = c('government', 'variable', 'legislation'),
              names_pattern = '(federal|state)_(.*)_(arp)',
              values_to = 'values') %>% 
  left_join(baseline_projections %>% select(date, real_potential_gdp_growth, consumption_deflator_growth),
            by = 'date') 
 
#   
# arp_wide <-
#   arp_long %>% 
#   pivot_wider(names_from = c(government, variable, legislation),
#               values_from = values)
#   pivot_wider(names_from = c(government, variable, legislation),
#               values_from = value)

# Add factors -------------------------------------------------------------
projections <-
  baseline_projections %>% 
  left_join(add_factors, by = 'date') %>% 
  mutate(across(.cols = starts_with('add'),
                .fns = ~ if_else(id == 'historical',
                                 0,
                                 .x)))  %>% 
  mutate(across(c(consumption_grants,
                  federal_purchases,
                  state_purchases,
                  federal_social_benefits,
                  state_social_benefits,
                  federal_subsidies,
                  federal_health_outlays,
                  state_health_outlays),
                ~ .x + get(paste0('add_', cur_column())))) %>% 
  mutate(consumption_grants = if_else(date >= yearquarter('2020 Q2'),
                                      consumption_grants_override,
                                      consumption_grants)) 
# Consumption -------------------------------------------------------------
consumption <-
  projections %>% 
  taxes_transfers_minus_neutral() %>% 
  calculate_mpc('social_benefits') %>% 
  calculate_mpc('ui') %>%  
  mutate(rebate_checks_post_mpc = mpc_rebate_checks(rebate_checks_minus_neutral)) %>% 
  calculate_mpc('subsidies') %>% 
  calculate_mpc('health_outlays') %>% 
  calculate_mpc('corporate_taxes') %>% 
  calculate_mpc('non_corporate_taxes')

baseline_consumption <-
  baseline_projections %>% 
  taxes_transfers_minus_neutral() %>% 
  calculate_mpc('social_benefits') %>% 
  calculate_mpc('ui') %>%  
  mutate(rebate_checks_post_mpc = mpc_rebate_checks(rebate_checks_minus_neutral)) %>% 
  calculate_mpc('subsidies') %>% 
  calculate_mpc('health_outlays') %>% 
  calculate_mpc('corporate_taxes') %>% 
  calculate_mpc('non_corporate_taxes')


# Contribution ------------------------------------------------------------
contribution <-
  consumption %>% 
  purchases_contributions() %>% 
  
  taxes_contributions() %>%
  sum_taxes_contributions() %>%
  transfers_contributions() %>%
  sum_transfers_contributions() %>%
  sum_taxes_transfers() %>%
  get_fiscal_impact()

baseline_contribution <- 
  baseline_consumption %>% 
  purchases_contributions() %>% 
  taxes_contributions() %>% 
  sum_taxes_contributions() %>% 
  transfers_contributions() %>% 
  sum_transfers_contributions() %>% 
  sum_taxes_transfers() %>% 
  get_fiscal_impact()

pkg <- 
  baseline_contribution %>% 
  filter_index("2019 Q1") %>% 
  select(date, 
           ends_with("contribution"),
         -contains("tax")) %>% 
  pivot_longer(-date,
               values_to = "pkg")



march <-
  readxl::read_xlsx("results/3-2021/fim-3-2021.xlsx") %>% 
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>% 
  as_tsibble(index = date) %>% 
  filter_index("2019 Q1") %>% 
  select(date, ends_with("cont"))  %>% 
  select(-contains("tax"), -contains(c("grants", "purchases", "nom", "federal_cont", "state_local_cont"))) %>% 
  rename_with(~paste0(.x, 'ribution'), ends_with("cont")) %>% 
  rename(federal_ui_contribution = federal_unemployment_insurance_contribution,
         state_ui_contribution = state_unemployment_insurance_contribution,
         ui_contribution = unemployment_insurance_contribution) %>% 
  
  pivot_longer(-date, values_to = "march") 


readxl::read_xlsx("results/3-2021/fim-3-2021.xlsx") %>% 
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>% 
  as_tsibble(index = date) %>% 
  filter_index("2019 Q1") %>% 
  select(state_unemployment_insurance)
pkg %>% 
  inner_join(march, by = c("date", "name")) %>% 
  mutate(across(where(is.numeric),
                ~ round(.x, 3))) %>% 
  mutate(diff = pkg - march) %>% View()

contribution%>% 
filter_index("2019 Q1" ~ "2019 Q4") %>% 
  select(date, 
            ends_with("contribution")) %>% View()

contribution %>% 
  filter_index("2019 Q1") %>% 
  select(transfers_contribution,
         federal_social_benefits_contribution,
         federal_health_outlays_contribution,
         federal_subsidies_contribution)
readxl::read_xlsx("results/4-2021/fim-4-2021.xlsx") %>% 
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>% 
  as_tsibble(index = date) %>% 
  filter_index("2019 Q1") %>% 
  select(
    transfers_cont,
    social_benefits_cont,
    federal_social_benefits_cont,
    federal_health_outlays_cont,
    federal_subsidies_cont)

# Comparison --------------------------------------------------------------

previous <- 
  readxl::read_xlsx("results/4-2021/fim-4-2021.xlsx") %>% 
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>% 
  as_tsibble(index = date) %>% 
  filter_index("2019 Q1" ~ "2019 Q4")
previous_long <-
  previous %>% 
  select(-ends_with(c("post_mpc", "growth", "arp", "cont", "pi", "minus_neutral", "override", "ex_grants"))) %>%
  select(date, starts_with(c("federal" ,"state"))) %>% 
  pivot_longer(starts_with(c('federal', 'state')),
               names_to = c('government', 'variable'),
               names_pattern = '(federal|state)_(.*)',
               values_to = 'previous') %>% 
  mutate(variable = recode(variable, 
                           `nom` = "purchases",
                           `cgrants` = "consumption_grants",
                           `igrants` = "investment_grants",
                           `unemployment_insurance` = "ui",
                           `noncorp_taxes` = "non_corporate_taxes",
                           `local_nom` = "purchases",
                           
                           ))
 
usna_processed %>% 
  filter_index("2018 Q4" ~ "2019 Q4") %>% 
  select(date, gdp, real_potential_gdp_growth, 
         federal_purchases, state_purchases, consumption_grants, investment_grants, federal_purchases_deflator_growth, state_purchases_deflator_growth, consumption_grants_deflator_growth, investment_grants_deflator_growth) %>% 
  mutate(federal_purchases_counterfactual = lag(federal_purchases) * (1 + federal_purchases_deflator_growth + real_potential_gdp_growth),
         
         consumption_grants_counterfactual = lag(consumption_grants) * (1 + consumption_grants_deflator_growth + real_potential_gdp_growth),
         investment_grants_counterfactual = lag(investment_grants) * (1 + investment_grants_deflator_growth + real_potential_gdp_growth),
         ) %>% 
  mutate(federal_nipa_contribution = 400 * (federal_purchases - federal_purchases_counterfactual) / lag(gdp),
         consumption_grants_contribution =
           400 * (consumption_grants - consumption_grants_counterfactual) / lag(gdp),
         investment_grants_contribution = 
           400 * (investment_grants - investment_grants_counterfactual) / lag(gdp),
         
         ) %>% 
  select(date, ends_with("contribution")) %>% 
  drop_na() %>% 
  mutate(federal = federal_nipa_contribution + consumption_grants_contribution + investment_grants_contribution)


# Scratch -----------------------------------------------------------------


`%notin%` <- Negate(`%in%`)
contribution_long<-
usna_processed %>% 
  filter_index("2019 Q1" ~ "2019 Q4") %>% 
  select(-ends_with(c("post_mpc", "growth", "arp", "cont", "pi", "minus_neutral", "override", "ex_grants", "deflator", "cumulative", "contribution" ))) %>% 
  select(date, starts_with(c("federal" ,"state")),  federal_consumption_grants = consumption_grants, federal_investment_grants = investment_grants, federal_rebate_checks = rebate_checks) %>% 
  pivot_longer(starts_with(c('federal', 'state')),
               names_to = c('government', 'variable'),
               names_pattern = '(federal|state)_(.*)',
               values_to = 'current') %>% 
  mutate(component = case_when(variable %in% c( 'social_benefits', 'subsidies', 'health_outlays', 'ui' ) ~ 'transfers',
                               variable %in% c('corporate_taxes', 'non_corporate_taxes') ~ 'taxes',
                               variable %in% c('purchases', 'consumption_grants', 'investment_grants') ~ 'government')) %>% 
  left_join(previous_long, by = c("date", "government", "variable")) %>% 
  drop_na() %>% 
  select(date,  component,government,variable,previous, current) %>% 
  arrange(government, component, variable,date) %>% 
  mutate(difference = current - previous) %>% View()


contribution %>% 
  filter_index("2019 Q1" ~ "2019 Q4") %>% 
  prepare_interactive() 
  

previous %>% 
  select(date,
         impact = fiscal_impact_moving_average,
         total = fiscal_impact,
         federal = federal_cont,
         state_local = state_local_cont,
         consumption = taxes_transfers_cont)

baseline_projections %>% 
  filter_index("2019 Q1" ~ "2019 Q4") %>% 
  select(date, federal_purchases, state_purchases, consumption_grants, investment_grants, real_potential_gdp_growth,
         federal_purchases_deflator_growth, state_purchases_deflator_growth,
         consumption_deflator_growth, investment_grants_deflator_growth) 
  pivot_longer(-date,
               names_to = c(".value", "deflator"),
               names_pattern = '(.)(.)') 
  pivot_longer(ends_with(c('purchases', 'grants')),
               names_to = 'variable') %>% 
  pivot_longer(ends_with('deflator_growth'),
               names_to = 'variable',
               values_to = 'deflator')
  
  baseline_projections_long <-
    baseline_projections %>% 
    filter_index("2018 Q4" ~ "2019 Q4")%>% 
    as_tibble() %>% 
  select(date, gdp,federal_purchases_level = federal_purchases, state_purchases_level = state_purchases, gdp, real_potential_gdp_growth,
         federal_purchases_deflator_growth, state_purchases_deflator_growth,
         federal_consumption_grants_level = consumption_grants, 
         federal_investment_grants_level = investment_grants,
         federal_consumption_grants_deflator_growth = consumption_grants_deflator_growth, 
         federal_investment_grants_deflator_growth = investment_grants_deflator_growth
         
         ) %>% 
  pivot_longer(starts_with(c('federal', 'state')),
               names_to = c('government', 'variable', '.value'),
               names_pattern = '(federal|state)_(purchases|consumption_grants|investment_grants)_(.*)',
               values_to = 'current') 
  
  baseline_projections_long %>% 
    group_by(government, variable) %>% 
    summarise(date, level,
              counterfactual = lag(level) * (1 + deflator_growth + real_potential_gdp_growth),
              contribution = 400 * (level - counterfactual) / lag(gdp)) %>% 
    drop_na()
  
  pivot_longer(-c(date, real_potential_gdp_growth), 
         names_pattern = '(.)(_level|_growth)',
         names_to = c('variable', '.value'))
  pivot_longer(ends_with('deflator_growth'),
               values_to = 'deflator') %>% 
  select(-name)
