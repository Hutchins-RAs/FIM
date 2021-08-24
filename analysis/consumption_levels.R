# Setup -------------------------------------------------------------------
librarian::shelf(
  tidyverse,
  tsibble,
  lubridate,
  glue,
  fim,
  ggbrookings,
  dplyover,
  gt)


# Wrangle data ------------------------------------------------------------

# Since BEA put all CARES act grants to S&L in Q2 2020 we need to
# override the historical data and spread it out based on our best guess
# for when the money was spent.
overrides <- readxl::read_xlsx('data/forecast_07_2021.xlsx',
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
  readxl::read_xlsx('data/forecast_07_2021.xlsx',
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


# Consumption levels ------------------------------------------------------

consumption <-
  projections %>% 
  rename_with(.cols = starts_with('rebate'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'rebate',
                                  replacement = "federal_rebate"))  %>% 
  # ACTUAL CONSUMPTION
  mpc_tidy(mpc_data, 
           c(matches('corporate|non_corporate|social_benefits|health_outlays|ui$|subsidies|aid_to_small_businesses|rebate_checks|direct_aid|vulnerable') &
               !ends_with('growth') &
               !starts_with('provider_relief')))  
  # COUNTERFACTUAL CONSUMPTION
  mutate(consumption, across(ends_with('_consumption'),
              ~ counterfactual(.x, consumption_deflator_growth),
              .names = '{.col}_counterfactual'))


consumption_summary <-
  consumption %>% 
  select(date,gdp,  matches('federal|state') & matches('_consumption'), date) %>% 
  rename_with(.cols = ends_with('consumption_counterfactual'),
              .fn = ~ str_replace(string = .x,
                                  pattern = 'consumption_counterfactual',
                                  replacement = "counterfactual"))  %>% 
  pivot_longer(-c(date, id, gdp),
               names_to = c('government','variable', 'level'),
               names_pattern = '(federal|state)_(.*)_(.*)') %>% 
  pivot_wider(names_from = 'level',
              values_from = value)

theme_set(theme_brookings())
update_geom_defaults('size',
                     list(size = 1.5))

consumption_summary %>% 
  filter(government == 'federal',
         variable %in% c('aid_to_small_businesses_arp', 'health_outlays',
                         'other_direct_aid_arp', 'other_vulnerable_arp', 'social_benefits', 'subsidies', 'ui', 'rebate_checks'),
         date > yearquarter('2018 Q1') & date < yearquarter('2023 Q1')) %>% 
  mutate(variable = snakecase::to_title_case(variable)) %>% 
  ggplot(aes(x = date, y = consumption, fill = variable, color = variable)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  scale_color_brookings('categorical') +
  scale_y_continuous(labels = scales::label_dollar()) +
  facet_wrap( variable ~ ., scales = 'free') +
  labs(title = 'Consumption Levels for Federal Transfers',
       x = NULL,
       y = NULL) +
  theme(
    plot.title = ggtext::element_textbox_simple(
       lineheight = 1,
     # padding = margin(5, 5, 5, 5), # padding around text inside the box
     # margin = margin(0, 0, 10, 0) # margin outside the box
    )
  )
  
