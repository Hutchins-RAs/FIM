# Setup -------------------------------------------------------------------
  librarian::shelf(
    "tidyverse",
    "zoo",
    "TTR",
    "tsibble",
    "targets",
    "tarchetypes",
    "lubridate",
    "alistaire47/pipecleaner",
    "glue",
    "validate",
    "fim",
    "dplyover",
    "tsibble",
    "magrittr",
    "feasts",
    "fable"
  )




# Wrangle data ------------------------------------------------------------
usna <-
  read_data() %>%
  # Don't sneak in ui reallocation here.
  define_variables() %>%
  as_tsibble(key = id, index = date) %>% 
  create_override(
    var = state_purchases_growth,
    start = yearquarter('2021 Q2'),
    end = yearquarter('2023 Q2'),
    
    # Before april 
    # values = c(0.00625, rep(0.0075, 3), 0.00625, 0.005, rep(0.0025, 3)) 
    # 
    # After april
    values = c(
      0.024113689,
      0.019426547,
      0.015868285,
      0.015868285,
      0.015868285,
      0.014673846,
      0.013475174,
      0.012272234,
      0.012272234

    )
  ) %>% 
  growth_assumptions() %>%
  ungroup() %>% 
 
  mutate(real_potential_gdp_growth = q_g(real_potential_gdp)) %>% 
  mutate(federal_social_benefits = federal_social_benefits - ui - rebate_checks,
         # state_social_benefits = state_social_benefits + state_ui,
         social_benefits = federal_social_benefits + state_social_benefits) %>% 
  mutate(federal_ui = federal_ui - wages_lost_assistance)


## Remove ARP from USNA
arp <-   readxl::read_xlsx('data/arp_summary.xlsx') %>% 
  mutate(date = yearquarter(date)) 

usna_processed <-
  usna %>% 
  left_join(arp, by = 'date') %>% 
  mutate(across(.cols = contains('arp'),
                .fns = ~ coalesce(.x, 0))) %>% 
  rename(rebate_checks_arp = federal_rebate_checks_arp) %>% 

  mutate_where(
    id == "historical",
    # federal_social_benefits = federal_social_benefits -  state_ui_arp -  federal_other_direct_aid_arp - federal_other_vulnerable_arp - federal_health_grants_arp,
    federal_subsidies = federal_subsidies - federal_aid_to_small_businesses_arp,
    
    #federal_ui = federal_ui - federal_ui_arp,
    #state_ui = state_ui - state_ui_arp,
    rebate_checks = rebate_checks - rebate_checks_arp,
    federal_ui_arp = 0,
    state_ui_arp = 0,
    
    # Error that we currently have
    state_ui =  state_ui +  wages_lost_assistance,
    state_social_benefits = state_social_benefits + state_ui  ,
  ) %>% 
  
  mutate(social_benefits = federal_social_benefits + state_social_benefits)



# Forecast ----------------------------------------------------------------
baseline_projections <-
  usna_processed %>% 
  mutate(consumption_grants = gross_consumption_grants - medicaid_grants) %>% 
  forecast() %>% 

ungroup() %>% 
  mutate(
         grants = consumption_grants + investment_grants,
    
         state_social_benefits =  state_social_benefits - medicaid,
         federal_social_benefits = federal_social_benefits - medicare,
         social_benefits = federal_social_benefits +  state_social_benefits
  ) %>%
mutate(# Health outlays reattribution
  health_outlays = medicare + medicaid,
  federal_health_outlays = medicare + medicaid_grants,
  state_health_outlays = medicaid - medicaid_grants,
  

  # Aggregate taxes
  corporate_taxes = federal_corporate_taxes + state_corporate_taxes,
  payroll_taxes = federal_payroll_taxes + state_payroll_taxes,
  production_taxes = federal_production_taxes + state_production_taxes,
  personal_taxes = federal_personal_taxes + state_personal_taxes,
  federal_non_corporate_taxes = federal_payroll_taxes + federal_personal_taxes + federal_production_taxes,
  state_non_corporate_taxes = state_payroll_taxes + state_personal_taxes + state_production_taxes,
  non_corporate_taxes = federal_non_corporate_taxes + state_non_corporate_taxes,
  
  # Coalesce NA's to 0
  across(where(is.numeric),
         ~ coalesce(.x, 0))) 
  

baseline_projections %>% 
  filter_index("2020 Q1" ~ .) %>% 
  as_tibble() %>% 
  select(date,
         state_health_outlays,
         state_social_benefits,
         state_non_corporate_taxes,
         state_corporate_taxes,
         federal_health_outlays, 
         federal_social_benefits,
         federal_subsidies,
         consumption_grants) %>% 
 
  pivot_longer(-date) %>% 
  pivot_wider(names_from = date,
              values_from = value) %>% 
  
  openxlsx::write.xlsx("data/baseline_projections.xlsx")

# Add factors -------------------------------------------------------------
add_factors <-
  readxl::read_xlsx('data/add_factors.xlsx',
                    sheet = "FIM Add Factors")  %>% 
  mutate(date = tsibble::yearquarter(date))
projections <-
  baseline_projections %>% 
  left_join(add_factors, by = 'date') %>% 
  mutate(across(.cols = starts_with('add'),
                .fns = ~ if_else(id == 'historical',
                                 0,
                                 .x)))  %>% 
  mutate(across(c(
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
                                      consumption_grants)) %>% 
  mutate_where(id == "projection",
               federal_ui = federal_ui_override,
               state_ui =  state_ui_override,
               ui = ui_override) %>% 
  mutate_where(date == yearquarter("2021 Q1"),
               ppp = 639.2,
               federal_subsidies = 753,
               subsidies = federal_subsidies + state_subsidies) 

# American Rescue Plan ----------------------------------------------------


arp_contribution <-
  
  readxl::read_xlsx('data/arp_summary.xlsx') %>% 
  mutate(date = yearquarter(date)) %>% 
  as_tsibble(index = date) %>% 
  append_row(-1) %>% 
  mutate(across(where(is.numeric), ~ coalesce(.x, 0))) %>% 
  
  full_join(baseline_projections %>% filter_index("2020 Q4" ~ .) %>% select(date, real_potential_gdp_growth, consumption_deflator_growth, consumption_grants_deflator_growth, gdp),
            by = 'date') %>% rename(rebate_checks_arp = federal_rebate_checks_arp) %>% 
  mutate_where(date == yearquarter('2021 Q1'), 
               federal_ui_arp = 0,
               state_ui_arp = 0) %>% 
  mutate(across(
    .cols = all_of(
      c(
        'rebate_checks_arp',
        'federal_other_direct_aid_arp',
        'federal_health_grants_arp',
        'federal_non_health_grants_arp',
        'federal_other_vulnerable_arp',
        'federal_ui_arp',
        'state_ui_arp',
        'federal_aid_to_small_businesses_arp'
      )
    ),
    .fns = ~ .x - dplyr::lag(.x, default = 0) * (1 + real_potential_gdp_growth + consumption_deflator_growth),
    .names = '{.col}_minus_neutral'
  )) %>% 
  mutate(federal_non_health_grants_arp_post_mpc = mpc_non_health_grants_arp(federal_non_health_grants_arp)) %>% 
  mutate(federal_non_health_grants_arp_contribution = 400 * (
    federal_non_health_grants_arp_post_mpc  - lag(federal_non_health_grants_arp_post_mpc) * (1 + real_potential_gdp_growth + consumption_grants_deflator_growth)
  ) / lag(gdp, default = 1)) %>% 
  mutate(
    across(
      .cols = all_of(
        c('federal_ui_arp', 'state_ui_arp', 'federal_other_vulnerable_arp') %>% paste0('_minus_neutral')
      ),
      .fns = ~ mpc_vulnerable_arp(.x),
      .names = '{.col}_post_mpc'
    ),
    across(
      .cols = all_of(
        c('rebate_checks_arp', 'federal_other_direct_aid_arp') %>% paste0('_minus_neutral')
      ),
      .fns = ~ mpc_direct_aid_arp(.),
      .names = '{.col}_post_mpc'
    ),
    federal_health_grants_arp_minus_neutral_post_mpc = mpc_health_outlays(federal_health_grants_arp_minus_neutral),
    federal_aid_to_small_businesses_arp_minus_neutral_post_mpc = mpc_small_businesses_arp((federal_aid_to_small_businesses_arp_minus_neutral ))) %>% 
  mutate(across(ends_with('post_mpc'),
                ~ 400 * .x / lag(gdp),
                .names = "{.col}_contribution")) %>% 
  rename_with(~ str_replace(.x, '_minus_neutral_post_mpc_contribution', '_contribution')) %>% 
  mutate(date,
         consumption_grants_arp_contribution = federal_non_health_grants_arp_contribution,
         federal_health_outlays_arp_contribution = federal_health_grants_arp_contribution,
         federal_social_benefits_arp_contribution = federal_other_direct_aid_arp_contribution + federal_other_vulnerable_arp_contribution,
         federal_subsidies_arp_contribution = federal_aid_to_small_businesses_arp_contribution,
         rebate_checks_arp_contribution,
         federal_ui_arp_contribution,
         state_ui_arp_contribution) %>% 
  mutate(
    date,
    consumption_grants_arp_contribution,
    federal_transfers_arp_contribution = 
      federal_social_benefits_arp_contribution +
      federal_subsidies_arp_contribution +
      federal_health_outlays_arp_contribution + 
      federal_ui_arp_contribution +
      rebate_checks_arp_contribution,
    state_transfers_arp_contribution = state_ui_arp_contribution)





# Consumption -------------------------------------------------------------

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

# Contribution ------------------------------------------------------------

# Without add factors or ARP ---------
baseline_contribution <- 
  baseline_consumption %>% 
  purchases_contributions() %>% 
  taxes_contributions() %>% 
  sum_taxes_contributions() %>% 
  transfers_contributions() %>% 
  sum_transfers_contributions() %>% 
  sum_taxes_transfers() %>% 
  get_fiscal_impact()


# ------
contribution_no_arp <-
  consumption %>% 
  purchases_contributions() %>% 
  taxes_contributions() %>% 
  sum_taxes_contributions() %>% 
  transfers_contributions() %>% 
  sum_transfers_contributions() %>% 
  get_fiscal_impact()



contribution <-
  contribution_no_arp %>% 
  coalesce_join(arp_contribution, by = c('date', 'id')) %>% 
  mutate(across(
    .cols = contains("arp"),
    .fns = ~ coalesce(.x, 0)
  )) %>%
  mutate(
    grants_contribution = consumption_grants_contribution + investment_grants_contribution ,
    federal_contribution = federal_purchases_contribution + grants_contribution + consumption_grants_arp_contribution ,
    state_contribution = state_purchases_contribution - grants_contribution
  ) %>%
  mutate(
    federal_transfers_contribution = federal_transfers_arp_contribution + federal_transfers_contribution,
    state_transfers_contribution = state_transfers_arp_contribution + state_transfers_contribution,
    transfers_contribution = federal_transfers_contribution + state_transfers_contribution
  ) %>%
  sum_taxes_transfers() %>%
  get_fiscal_impact()






# Summaries ---------------------------------------------------------------


contribution %>% 
  prepare_interactive() %>% 
  mutate(across(where(is.numeric),
                ~ round(.x, 3))) %>% View()




contribution_summary <-
  contribution %>% 
  select(date, ends_with('contribution')) %>% 
  filter_index("2020 Q2" ~ "2022 Q4") %>% 
  pivot_longer(-c(date, id)) %>% 
  mutate(value = round(value, 3))





march <- readxl::read_xlsx("results/3-2021/fim-3-2021.xlsx") %>% 
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>% 
  as_tsibble(index = date) %>% 
  
  rename_with(~paste0(.x, 'ribution'), ends_with("cont")) %>% 
  rename(federal_ui_contribution = federal_unemployment_insurance_contribution,
         federal_ui = federal_unemployment_insurance,
         state_ui = state_unemployment_insurance,
         
         state_ui_contribution = state_unemployment_insurance_contribution,
         ui_contribution = unemployment_insurance_contribution) 

march_transfers <-
  march %>% 
  select(
    state_social_benefits,
    state_social_benefits_contribution,
    state_ui,
    state_ui_contribution) %>% 
  filter_index("2020 Q1" ~ "2021 Q1") %>% 
  pivot_longer(-date,
               values_to = "march")
contribution %>% 
  select(
    state_social_benefits,
    state_social_benefits_contribution,
state_ui,
    state_ui_contribution) %>% 
  filter_index("2020 Q1" ~ "2021 Q1") %>% 
  pivot_longer(-c(date, id)) %>% 
  left_join(march_transfers, by = c("date", "name")) 
  
april <- readxl::read_xlsx("results/4-2021/fim-4-2021.xlsx") %>% 
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>% 
  as_tsibble(index = date) %>% 
  
  rename_with(~paste0(.x, 'ribution'), ends_with("cont")) %>% 
  rename(federal_ui_contribution = federal_unemployment_insurance_contribution,
         federal_ui = federal_unemployment_insurance,
         state_ui = state_unemployment_insurance,
         
         state_ui_contribution = state_unemployment_insurance_contribution,
         ui_contribution = unemployment_insurance_contribution,
         
         ui = unemployment_insurance,
         ui_minus_neutral = unemployment_insurance_minus_neutral,
         ui_post_mpc = unemployment_insurance_post_mpc) 
  
april_transfers <-
  april %>% 

  select(date,
        
        
        federal_contribution,
        state_contribution =  state_local_contribution
          ) %>% 
  
  filter_index("2021 Q1" ~ "2022 Q1") %>% 
  pivot_longer(-date,
               values_to = "april")

contribution %>% 
  select(date,
         
         
         federal_contribution,
         state_contribution
  ) %>% 
  filter_index("2021 Q1" ~ "2022 Q1") %>% 
  pivot_longer(-c(date, id)) %>% 
  left_join(april_transfers, by = c("date", "name")) %>% 
  drop_na()

contribution %>% 
  filter_index("2021 Q1") %>% 
  mutate(x = 3118 + rebate_checks + rebate_checks_arp + federal_ui + federal_ui_arp + federal_other_direct_aid_arp + federal_other_vulnerable_arp ,
         .keep = "used",
         .before = everything())

usna %>% 
  filter_index("2021 Q1") %>% 
  select(federal_social_benefits)
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
