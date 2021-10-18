# The purpose of this script is to calculate the counterfactuals used in the FIM levels explainer 
# Read code as normal until forecast object is created
# Setup -------------------------------------------------------------------

Sys.setenv(TZ = 'UTC')
librarian::shelf(
  "tidyverse",
  "zoo",
  "TTR",
  "tsibble",
  "lubridate",
  "glue",
  "fim",
  "dplyover",
  gt
)
options(digits = 4)
options(scipen = 20)
devtools::load_all()

contributions <- readr::read_rds(here::here("data/contributions.rds"))
# Set dates for current and previous months
month_year <- glue('{format.Date(today(), "%m")}-{year(today())}')
last_month_year <- glue('{format.Date(today()-months(1), "%m")}-{year(today())}')

if(!dir.exists(glue('results/{month_year}'))) {
  dir.create(glue('results/{month_year}'))
}

# Wrangle data ------------------------------------------------------------
# Prepare the FIM data as usual.
# 
# 
# Since BEA put all CARES act grants to S&L in Q2 2020 we need to
# override the historical data and spread it out based on our best guess
# for when the money was spent.
overrides <- readxl::read_xlsx(here::here('data/forecast.xlsx'),
                               sheet = 'historical overrides') %>% 
  select(-name) %>% 
  pivot_longer(-variable) %>% 
  pivot_wider(names_from = 'variable',
              values_from = 'value') %>% 
  rename(date = name) %>% 
  mutate(date = yearquarter(date))

# Load national accounts data from BEA
usna <-
  read_data() %>%
  # Rename Haver codes for clarity
  define_variables() %>%
  # Specify time series structure:
  # Key is historical or forecast period
  # Indexed by date
  as_tsibble(key = id, index = date) %>%
  # Calculate GDP growth for data but take CBO for projection
  mutate_where(id == 'historical',
               real_potential_gdp_growth = q_g(real_potential_gdp)) %>% 
  # Net out unemployment insurance, rebate checks, and Medicare to apply different MPC's
  mutate(
    federal_social_benefits = federal_social_benefits - ui - rebate_checks - medicare,
    state_social_benefits = state_social_benefits - medicaid,
    social_benefits = federal_social_benefits + state_social_benefits,
    consumption_grants = gross_consumption_grants - medicaid_grants,
  ) %>% 
  mutate(rebate_checks_arp = if_else(date == yearquarter("2021 Q1"),
                                     1348.1,
                                     0)) %>%
  mutate_where(id == 'projection',
               rebate_checks_arp = NA,
               federal_ui = NA,
               state_ui = NA) %>% 
  mutate_where(date == yearquarter('2021 Q1'),
               rebate_checks = rebate_checks - rebate_checks_arp,
               federal_social_benefits = federal_social_benefits + 203
  ) %>% 
  mutate(consumption_grants = gross_consumption_grants - medicaid_grants,
         
         # Aggregate taxes
         corporate_taxes = federal_corporate_taxes + state_corporate_taxes,
         non_corporate_taxes = federal_non_corporate_taxes + state_non_corporate_taxes) %>% 
  mutate_where(id == 'projection',
               consumption_grants_deflator_growth = state_purchases_deflator_growth,
               investment_grants_deflator_growth = state_purchases_deflator_growth) %>% 
  mutate_where(date >= yearquarter('2020 Q2') & date <= yearquarter('2021 Q2'),
               consumption_grants = overrides$consumption_grants_override) 

# Forecast ----------------------------------------------------------------
forecast <- 
  readxl::read_xlsx(here::here('data/forecast.xlsx'),
                    sheet = 'forecast') %>% 
  select(-15:-17, -name) %>% 
  pivot_longer(-variable) %>% 
  pivot_wider(names_from = 'variable',
              values_from = 'value') %>% 
  rename(date = name) %>% 
  mutate(date = yearquarter(date))


projections <- coalesce_join(usna, forecast, by = 'date') %>%
  
  mutate(# Coalesce NA's to 0
    across(where(is.numeric),
           ~ coalesce(.x, 0))) %>%
  mutate(
    health_outlays = medicare + medicaid,
    federal_health_outlays = medicare + medicaid_grants,
    state_health_outlays = medicaid - medicaid_grants
  )


# MPC ---------------------------------------------------------------------


# Consumption levels ------------------------------------------------------
mpc_data <- read_mpc_file()
consumption <-
  projections %>% 
  rename(federal_rebate_checks = rebate_checks,
         federal_rebate_checks_arp = rebate_checks_arp) %>% 
  # Create counterfactual
  mutate(
    across(
      c(matches('corporate|non_corporate|social_benefits|health_outlays|ui$|subsidies|aid_to_small_businesses|rebate_checks|direct_aid|vulnerable')) & !contains('provider_relief') & !ends_with('growth'),
      ~ counterfactual(.x, consumption_deflator_growth),
      .names = '{.col}_counterfactual'
    )) %>% 
  # ACTUAL CONSUMPTION
  mpc_tidy(mpc_data,
           c(matches('corporate|non_corporate|social_benefits|health_outlays|subsidies|aid_to_small_businesses|rebate_checks|direct_aid|vulnerable') &
               !ends_with('growth') &
               !starts_with('provider_relief')))  %>% 
  # UI MPC depends on quarter so the code above won't help
  mutate(across(c(federal_ui, federal_ui_counterfactual,  state_ui, state_ui_counterfactual),
                .fns = ~ if_else(date < yearquarter("2021 Q2"),
                                 mpc_ui(.x),
                                 mpc_ui_arp(.x)),
                .names = '{.col}_consumption')) %>% 
  mutate(across(c(federal_purchases, state_purchases, consumption_grants, investment_grants),
                .names = '{.col}_consumption')) %>% 
  # COUNTERFACTUAL CONSUMPTION FOR PURCHASES
  mutate(
    federal_purchases_counterfactual_consumption = counterfactual(federal_purchases_consumption, deflator = federal_purchases_deflator_growth),
    state_purchases_counterfactual_consumption = counterfactual(state_purchases_consumption, deflator = state_purchases_deflator_growth),
    consumption_grants_counterfactual_consumption = counterfactual(consumption_grants_consumption, consumption_grants_deflator_growth),
    investment_grants_counterfactual_consumption = counterfactual(investment_grants_consumption, investment_grants_deflator_growth)
  ) 

## Reformat table

consumption_summary <-
  consumption %>%
  select(date,
         gdp,
         real_gdp,
         matches('federal|state') & matches('_consumption') | matches('consumption_grants_|investment_grants_')) %>% 
  rename_with(
    .cols = ends_with('counterfactual_consumption'),
    .fn = ~ str_replace(
      string = .x,
      pattern = 'counterfactual_consumption',
      replacement = "counterfactual"
    )
  ) %>% 
  mutate(
    federal_purchases_consumption = federal_purchases_consumption + consumption_grants_consumption + investment_grants_consumption,
    federal_purchases_counterfactual = federal_purchases_counterfactual + consumption_grants_counterfactual + investment_grants_counterfactual,
    state_purchases_consumption = state_purchases_consumption - consumption_grants_consumption - investment_grants_consumption,
    state_purchases_counterfactual = state_purchases_counterfactual - consumption_grants_counterfactual - investment_grants_counterfactual
  ) %>% 
  select(-matches('consumption_grants_|investment_grants_')) %>% 
  pivot_longer(
    -c(date, id, gdp, real_gdp),
    names_to = c('government', 'variable', 'level'),
    names_pattern = '(federal|state)_(.*)_(.*)'
  ) %>%
  pivot_wider(names_from = 'level',
              values_from = value) %>% 
  mutate(net = consumption - counterfactual) %>% 
  group_by(variable, government) %>% 
  mutate(contribution = 400 * net / dplyr::lag(gdp)) %>% 
  ungroup()

deflators <- projections %>% 
  select(date, consumption_deflator_growth, )
options(scipen=999)

transfers <- 
  consumption_summary %>% 
  left_join(contributions %>% select(date, real_potential_gdp_growth, consumption_deflator_growth, federal_purchases_deflator_growth, state_purchases_deflator_growth)) %>% 
  filter_index("2020 Q1" ~ "2023 Q2") %>% 
  mutate(period = if_else(date > yearquarter("2021 Q2"),
                          "forecast",
                          "history")) %>% 
  arrange(variable, government) %>% 
  mutate(counterfactual = case_when(date == min(date) ~ consumption,
                                    variable == 'purchases' & government == 'federal' ~ 1 + real_potential_gdp_growth + federal_purchases_deflator_growth,
                                    variable == 'purchases' & government == 'state' ~ 1 + real_potential_gdp_growth + state_purchases_deflator_growth,
                                    TRUE ~ 1 + real_potential_gdp_growth + consumption_deflator_growth)) %>% 
  ungroup() %>% 
  group_by(variable, government) %>% 
  mutate(counterfactual = purrr::accumulate(counterfactual, `*`)) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  group_by(date) %>% 
  summarise(consumption = sum(consumption),
            counterfactual = sum(counterfactual))

# Net Govt Spending  ------------------------------------------------------
# 
# Code to prepare alternative counterfactual in which purchases and transfers grow at a 2% annualized growth rate starting Q1 2020. 

counterfactual_start <- yearquarter("2020 Q1")
rate <- (1.02) ^ (0.25)

consumption_alt <- 
  projections %>% 
  filter_index("2020 Q1" ~ "2023 Q2") %>% 
  rename(federal_rebate_checks = rebate_checks,
         federal_rebate_checks_arp = rebate_checks_arp) %>% 
  select(date,
         gdp, 
         gdp_deflator,
         real_potential_gdp_growth,
         federal_purchases_deflator_growth,
         federal_purchases_deflator, 
         state_purchases_deflator_growth,
         state_purchases_deflator, 
         consumption_grants, 
         consumption_grants_deflator_growth,
         consumption_grants_deflator,
         investment_grants, 
         investment_grants_deflator_growth,
         investment_grants_deflator,
         consumption_deflator,
         consumption_deflator_growth, 
         real_gdp,
         matches('federal|state')) %>% 
  mutate(across(c(federal_purchases_deflator, state_purchases_deflator,
                  consumption_grants_deflator, investment_grants_deflator,
                  consumption_deflator),
                ~ if_else(date <= yearquarter("2021 Q2"),
                          .x / 100,
                          .x))) %>% 
  project(consumption_grants_deflator, investment_grants_deflator,  with = 1 + state_purchases_deflator_growth, from = yearquarter("2021 Q2")) %>% 
 mutate(across(c(matches('corporate|non_corporate|social_benefits|health_outlays|ui$|subsidies|aid_to_small_businesses|rebate_checks|direct_aid|vulnerable')) & !contains('provider_relief') & !ends_with('growth'),
               ~ .x / consumption_deflator)) %>% 
  mutate(federal_purchases = federal_purchases / federal_purchases_deflator, 
         state_purchases = state_purchases / state_purchases_deflator,
         consumption_grants = consumption_grants / consumption_grants_deflator,
         investment_grants = investment_grants / investment_grants_deflator) %>% 
  mutate(
    across(
      c(matches('corporate|non_corporate|social_benefits|health_outlays|ui$|subsidies|aid_to_small_businesses|rebate_checks|direct_aid|vulnerable|purchases$|investment_grants$|consumption_grants$')) & !contains('provider_relief') & !ends_with('growth'),
      ~ case_when(date > counterfactual_start ~ 1 + real_potential_gdp_growth,
                  date == counterfactual_start ~ .x),
      .names = '{.col}_counterfactual'
    ))  %>% 
  mutate_where(date >= counterfactual_start, 
               across(ends_with('counterfactual'),
                      ~ purrr::accumulate(.x, `*`))) %>% 
  mutate(
    federal_purchases = federal_purchases + consumption_grants + investment_grants,
    federal_purchases_counterfactual = federal_purchases_counterfactual + consumption_grants_counterfactual + investment_grants_counterfactual,
    state_purchases = state_purchases - consumption_grants - investment_grants,
    state_purchases_counterfactual = state_purchases_counterfactual - consumption_grants_counterfactual - investment_grants_counterfactual
  ) %>%
  # ACTUAL CONSUMPTION
  mpc_tidy(mpc_data,
           c(matches('corporate|non_corporate|social_benefits|health_outlays|ui$|ui_counterfactual|subsidies|aid_to_small_businesses|rebate_checks|direct_aid|vulnerable') &
               !ends_with('growth') &
               !starts_with('provider_relief')))

# Put data in long format and calculate net purchases, taxes, and transfers.
consumption_alt_long <-
  consumption_alt %>% 
  select(date,
         gdp, 
         gdp_deflator,
         real_gdp,
         federal_purchases_consumption = federal_purchases,
         federal_purchases_counterfactual,
         state_purchases_consumption = state_purchases, 
         state_purchases_counterfactual,
         matches('federal|state') & matches('_consumption')) %>% 
  rename_with(
    .cols = ends_with('counterfactual_consumption'),
    .fn = ~ str_replace(
      string = .x,
      pattern = 'counterfactual_consumption',
      replacement = "counterfactual"
    )
  ) %>% 
  pivot_longer(
    -c(date, id, real_gdp, gdp, gdp_deflator),
    names_to = c('government', 'variable', 'level'),
    names_pattern = '(federal|state)_(.*)_(.*)'
  ) %>%
  pivot_wider(names_from = 'level',
              values_from = value) %>% 
  mutate(net = consumption - counterfactual) %>% 
  mutate(
    category = case_when(
      variable %in% c('rebate_checks', 'rebate_checks_arp') ~ 'rebate_checks',
      variable %in% c('ui') ~ 'UI',
      variable %in% c('subsidies', 'aid_to_small_businesses_arp') ~ 'subsidies',
      variable %in% c(
        'social_benefits',
        'other_direct_aid_arp',
        'other_vulnerable_arp'
      ) ~ 'social_benefits',
      variable %in% c('corporate_taxes', 'non_corporate_taxes') ~ 'taxes',
      variable %in% c('purchases') ~ 'purchases',
      TRUE ~ 'health_outlays'
    )
  ) %>% 
  mutate(category = snakecase::to_title_case(category)) %>%
  mutate_where(category == 'Ui',
               category = 'UI') 
