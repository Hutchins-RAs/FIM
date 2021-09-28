
# Read code until forecast object is created
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



# Purchases ---------------------------------------------------------------

purchases <- 
  projections %>% 
  select(date, gdp, real_potential_gdp_growth, federal_purchases, state_purchases, consumption_grants, investment_grants, federal_purchases_deflator_growth, state_purchases_deflator_growth, consumption_grants_deflator_growth, investment_grants_deflator_growth) 

purchases <-
  projections %>% 
  select(date, gdp, real_potential_gdp_growth, federal_purchases, state_purchases, federal_purchases_deflator_growth, state_purchases_deflator_growth) %>% 
  pivot_longer(c(federal_purchases, state_purchases), names_to = 'variable') %>%
  as_tibble() %>% 
  pivot_longer(ends_with('deflator_growth'),
               names_to = 'deflator_name',
               values_to = 'deflator',
               names_pattern = "(.*)_deflator_growth") %>% 
  filter(variable == deflator_name) %>% 
  select(-deflator_name) %>% 
  group_by(variable) %>% 
  mutate(cfct_contribution = 400 * ((lag(value) * ( real_potential_gdp_growth)) / lag(gdp))) %>% 
  mutate(actual_contribution = 400 * (value - lag(value) * (1 + deflator)) / lag(gdp),
         fi = actual_contribution - cfct_contribution) %>% 
  group_by(date) %>% 
  summarise(actual_contribution = sum(actual_contribution),
            cfct_contribution = sum(cfct_contribution),
            fi = sum(fi)) %>% 
  mutate(key = 'purchases')


# Consumption levels ------------------------------------------------------
mpc_data <- read_mpc_file()
transfers <-
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
  select(date,
         gdp,
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
    -c(date, id, gdp),
    names_to = c('government', 'variable', 'level'),
    names_pattern = '(federal|state)_(.*)_(.*)'
  ) %>% 
  pivot_wider(names_from = 'level',
              values_from = value) %>% 
  as_tibble() %>% 
  mutate(excess = consumption - counterfactual) %>% 
  group_by(date, gdp) %>% 
  summarise(excess = sum(excess), .groups = 'drop') 


# the contribution of real consumption to GDP is just the growth rate of real consumption times the share of consumption in GDP
pce <-
  projections %>% 
  select(date, gdp, consumption, consumption_deflator_growth) %>% 
  left_join(transfers, by = c('date', 'gdp')) %>% 
  mutate(consumption_cfct = consumption - excess) %>% 
  mutate(actual_contribution = 400 * (consumption / lag(consumption) - (1 + consumption_deflator_growth)) * (lag(consumption) / lag(gdp))) %>% 
  mutate(cfct_contribution = 400 * (consumption_cfct / lag(consumption) - (1 + consumption_deflator_growth)) * (lag(consumption) / lag(gdp))) %>% 
  mutate(fi = actual_contribution - cfct_contribution) %>% 
  mutate(key = 'taxes_transfers')

levels <- bind_rows(pce, purchases) %>% 
  as_tibble() %>% 
  group_by(date) %>% 
  summarise(
            cfct_contribution = sum(cfct_contribution),
            actual_contribution = sum(actual_contribution),
            fi = sum(fi)) %>% 
  left_join(projections %>% select(date, gdp), by = 'date') %>% 
  mutate(gdp_delta = abs(gdp - lag(gdp))) %>% 
  mutate(across(c(cfct_contribution, actual_contribution, fi),
                ~ (.x / 100) * gdp_delta,
                .names = "{.col}_lvl"))

library('ggbrookings')
theme_set(theme_brookings())
update_geom_defaults('line',
                     list(size = 1.5))
update_geom_defaults('point', 
                     list(size = 2))
levels %>% 
  select(date, cfct_contribution_lvl, actual_contribution_lvl) %>% 
  pivot_longer(-date) %>% 
  filter(date >= yearquarter("2020 Q1") & date < yearquarter("2023 Q3")) %>% 
  mutate(year = year(date)) %>% 
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_color_brookings(labels = c('Actual','Counterfactual')) +
  scale_x_yearquarter(date_labels = paste0('Q', '%q')) +
  facet_wrap(. ~ year, scales = 'free') +
  labs(title = 'Actual and counterfactual contribution of fiscal policy to GDP in dollars',
       y = 'Billions',
       x = NULL,
       caption = "**Note:** The counterfactual is the contribution that would have prevailed if real purchases, real taxes, and real transfers were growing with potential GDP.") 
