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





# Consumption -------------------------------------------------------------


alt_consumption <- projections %>% 
  mutate(across(matches('taxes|social_benefits|health_outlays|ui|rebate_checks|subsidies|arp'),
                .fns = counterfactual,
                deflator = 'consumption_deflator_growth'))


mpc_data <- read_mpc_file()
mpc_tidy <-function(data, timing_file = NULL, vars){
  
  timing <- timing_file %||% read_mpc_file()
  
  mutate(data, 
         across({{vars}},
                ~ roll::roll_sum(.x, 
                                 weights = rev(get_timing(timing, .x)),
                                 width = length(get_timing(timing, .x)),
                                 online = FALSE,
                                 min_obs = 1),
                .names = '{.col}_consumption'))
 
  #rlang::eval_tidy(call, data = data)
}


consumption <-
  projections %>% 
  mpc_tidy(mpc_data, 
               c(matches('corporate|non_corporate|social_benefits|health_outlays|rebate_checks|ui$|subsidies|aid_to_small_businesses|direct_aid|vulnerable') &
               !ends_with('growth') &
               !starts_with('provider_relief')))

consumption %>% 
  filter_index('2018 Q1' ~ .) %>% 
  select(date, ends_with('_consumption')) %>% 
  ggplot(aes(x = date, y = rebate_checks_consumption)) +
  geom_col() +
  scale_y_continuous(labels = scales::label_dollar()) +
  ggbrookings::theme_brookings()


  select(ends_with('contribution')) %>%
    rename_with(~ str_replace(.x, "_consumption_contribution", "_contribution")) %>% 
    sum_transfers_contributions() %>% 
    
    mutate(
      grants_contribution = consumption_grants_contribution + investment_grants_contribution,
      federal_contribution = federal_purchases_contribution + grants_contribution,
      state_contribution = state_purchases_contribution - grants_contribution
    ) %>%
    mutate(social_benefits_contribution = federal_social_benefits_contribution + state_social_benefits_contribution) %>%
    mutate(non_corporate_taxes_contribution = federal_non_corporate_taxes_contribution + state_non_corporate_taxes_contribution) %>%
    mutate(taxes_contribution = non_corporate_taxes_contribution + corporate_taxes_contribution) %>%
    mutate(
      transfers_contribution = federal_social_benefits_contribution + state_social_benefits_contribution +
        rebate_checks_contribution + rebate_checks_arp_contribution + federal_ui_contribution + state_ui_contribution +
        federal_subsidies_contribution + federal_aid_to_small_businesses_arp_contribution +  state_subsidies_contribution + federal_health_outlays_contribution +
        state_health_outlays_contribution + federal_other_direct_aid_arp_contribution + federal_other_vulnerable_arp_contribution,
      taxes_contribution = federal_non_corporate_taxes_contribution + state_non_corporate_taxes_contribution +
        federal_corporate_taxes_contribution + state_corporate_taxes_contribution
    ) %>%
    #sum_taxes_contributions() %>%
    get_fiscal_impact() %>% 
  select(date, fiscal_impact)


temp %>% 
  filter(if_any(-c(date, contains('taxes')), ~ .x < 0))
temp2 <-
  temp%>% 
  mutate(across(ends_with('_consumption'),
                ~ counterfactual(.x, consumption_deflator_growth),
                .names = '{.col}_counterfactual')) %>% 
  as_tibble() %>% 
  select(date, ends_with('consumption'), ends_with('counterfactual')) %>% 
 pivot_longer(-date) %>% 
  
  mutate(type = if_else(str_ends(name, 'consumption'),
                         'actual',
                          'counterfactual'),
    
         name = str_remove(name, '(_consumption|_consumption_counterfactual)$'))  %>% 
  
  pivot_wider(names_from = type,
              values_from = value) 

temp2 %>% 
  filter(name == 'federal_social_benefits',
         date > yearquarter("2000 Q1") & date < yearquarter('2023 Q2')) %>% 
  pivot_longer(where(is.numeric),
               names_to = 'type') %>% 
  ggplot(aes(x = date, y = value, color = type)) +
  geom_line()

# Contribution ------------------------------------------------------------

contributions <-
  consumption %>%
  purchases_contributions() %>% 
  mutate(across(ends_with("post_mpc"),
                ~ 400 * .x / lag(gdp),
                .names = "{.col}_contribution"
  )) %>%
  rename_with(~ str_replace(.x, "_minus_neutral_post_mpc_contribution", "_contribution")) %>% 
  rename_with(~ str_replace(.x, "minus_neutral_post_mpc", "post_mpc")) %>% 
  rename_with(~ str_replace(.x, "post_mpc_contribution", "contribution")) %>% 
  sum_transfers_contributions() %>% 
  
  mutate(
    grants_contribution = consumption_grants_contribution + investment_grants_contribution,
    federal_contribution = federal_purchases_contribution + grants_contribution,
    state_contribution = state_purchases_contribution - grants_contribution
  ) %>%
  mutate(social_benefits_contribution = federal_social_benefits_contribution + state_social_benefits_contribution) %>%
  mutate(non_corporate_taxes_contribution = federal_non_corporate_taxes_contribution + state_non_corporate_taxes_contribution) %>%
  mutate(taxes_contribution = non_corporate_taxes_contribution + corporate_taxes_contribution) %>%
  mutate(
    transfers_contribution = federal_social_benefits_contribution + state_social_benefits_contribution +
      rebate_checks_contribution + rebate_checks_arp_contribution + federal_ui_contribution + state_ui_contribution +
      federal_subsidies_contribution + federal_aid_to_small_businesses_arp_contribution +  state_subsidies_contribution + federal_health_outlays_contribution +
      state_health_outlays_contribution + federal_other_direct_aid_arp_contribution + federal_other_vulnerable_arp_contribution,
    taxes_contribution = federal_non_corporate_taxes_contribution + state_non_corporate_taxes_contribution +
      federal_corporate_taxes_contribution + state_corporate_taxes_contribution
  ) %>%
  #sum_taxes_contributions() %>%
  get_fiscal_impact()