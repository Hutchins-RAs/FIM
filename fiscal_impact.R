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
  "dplyover"
)

options(digits = 4)
options(scipen = 20)
devtools::load_all()


# Wrangle data ------------------------------------------------------------

overrides <- readxl::read_xlsx('data/forecast_06_2021.xlsx',
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
         non_corporate_taxes = federal_non_corporate_taxes + state_non_corporate_taxes,
         
   
  ) %>% 
  mutate_where(id == 'projection',
               consumption_grants_deflator_growth = state_purchases_deflator_growth,
               investment_grants_deflator_growth = state_purchases_deflator_growth) %>% 
  mutate_where(date >= yearquarter('2020 Q2') & date <= yearquarter('2021 Q1'),
               consumption_grants = overrides$consumption_grants_override) 







# Forecast ----------------------------------------------------------------
forecast <- readxl::read_xlsx('data/forecast_06_2021.xlsx',
                              sheet = 'forecast') %>% 
  select(-name) %>% 
  pivot_longer(-variable) %>% 
  pivot_wider(names_from = 'variable',
              values_from = 'value') %>% 
  rename(date = name) %>% 
  mutate(date = yearquarter(date))


projections <- coalesce_join(usna, forecast, by = 'date') %>%
  
  mutate(
    
    # Coalesce NA's to 0
    across(
      where(is.numeric),
      ~ coalesce(.x, 0)
    )
  ) %>% 
  mutate(    health_outlays = medicare + medicaid,
             federal_health_outlays = medicare + medicaid_grants,
             state_health_outlays = medicaid - medicaid_grants)
  




# Consumption -------------------------------------------------------------


consumption <-
  projections %>%
  taxes_transfers_minus_neutral() %>%
  calculate_mpc("social_benefits") %>%
  mutate(rebate_checks_post_mpc = mpc_rebate_checks(rebate_checks_minus_neutral)) %>%
  calculate_mpc("subsidies") %>%
  calculate_mpc("health_outlays") %>%
  calculate_mpc("corporate_taxes") %>%
  calculate_mpc("non_corporate_taxes") %>% 
  mutate(across(c(federal_ui_minus_neutral, state_ui_minus_neutral),
                .fns = ~ if_else(date < yearquarter("2021 Q2"),
                                  mpc_ui(.x),
                                  mpc_ui_arp(.x)),
                .names = '{.col}_post_mpc')) %>% 
         
  mutate(across(
    .cols = all_of(
      c(
        "rebate_checks_arp",
        "federal_other_direct_aid_arp",
        "federal_other_vulnerable_arp",
       # "federal_ui_arp",
        #"state_ui_arp",
        "federal_aid_to_small_businesses_arp"
      )
    ),
    .fns = ~ .x - dplyr::lag(.x, default = 0) * (1 + real_potential_gdp_growth + consumption_deflator_growth),
    .names = "{.col}_minus_neutral"
  )) %>% 
  mutate(
    across(
      .cols = any_of(
        c("federal_ui_arp", "state_ui_arp", "federal_other_vulnerable_arp") %>% paste0("_minus_neutral")
      ),
      .fns = ~ mpc_vulnerable_arp(.x),
      .names = "{.col}_post_mpc"
    ),
    across(
      .cols = all_of(
        c("rebate_checks_arp", "federal_other_direct_aid_arp") %>% paste0("_minus_neutral")
      ),
      .fns = ~ mpc_direct_aid_arp(.),
      .names = "{.col}_post_mpc"
    ),
 
    federal_aid_to_small_businesses_arp_minus_neutral_post_mpc = mpc_small_businesses_arp((federal_aid_to_small_businesses_arp_minus_neutral))
  )

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
  mutate(transfers_contribution = federal_social_benefits_contribution + state_social_benefits_contribution +
           rebate_checks_contribution + rebate_checks_arp_contribution + federal_ui_contribution + state_ui_contribution +
           federal_subsidies_contribution + federal_aid_to_small_businesses_arp_contribution +  state_subsidies_contribution + federal_health_outlays_contribution +
           state_health_outlays_contribution + federal_other_direct_aid_arp_contribution + federal_other_vulnerable_arp_contribution,
         taxes_contribution = federal_non_corporate_taxes_contribution + state_non_corporate_taxes_contribution +
           federal_corporate_taxes_contribution + state_corporate_taxes_contribution) %>% 
  #sum_taxes_contributions() %>% 
  get_fiscal_impact()

contributions %>% select(date,fiscal_impact, ends_with('contribution')) %>% 
  filter_index('2020 Q2') %>% View()

# Contribution ------------------------------------------------------------

# Without add factors or ARP ---------

# ------
# 
# 
# 
# saveRDS(contribution, file = 'data/contribution.RDS')
# 
# contribution %>% 
#   filter_index("1999 Q4" ~ "2023 Q1") %>% 
#   prepare_interactive() %>% 
#   mutate(recession = if_else(recession == -1, 0, recession)) %>% 
#   writexl::write_xlsx('results/5-2021/interactive-5-2021.xlsx')
# 
# 
# openxlsx::write.xlsx(contribution, 'results/5-2021/fim-5-2021.xlsx')
# 
# fim_long <-
#   contribution %>% 
#   select(
#     date,
#     federal_social_benefits,
#     state_social_benefits,
#     federal_health_outlays,
#     state_health_outlays,
#     federal_subsidies,
#     state_subsidies,
#     federal_ui,
#     state_ui,
#     federal_corporate_taxes,
#     state_corporate_taxes,
#     gdp,
#     real_potential_gdp_growth,
#     
#     federal_purchases,
#     state_purchases,
#     federal_consumption_grants = consumption_grants,
#     federal_investment_grants = investment_grants
#   ) %>% 
#   pivot_longer(
#     starts_with(c('federal', 'state')),
#     names_to = c('government', 'variable'),
#     names_pattern = '(federal|state)_(.*)',
#     values_to = 'values'
#   ) %>%
#   mutate(
#     component = case_when(
#       variable %in% c('social_benefits', 'subsidies', 'health_outlays', 'ui') ~ 'transfers',
#       variable %in% c('corporate_taxes') ~ 'taxes',
#       variable %in% c('purchases', 'consumption_grants', 'investment_grants') ~ 'government'
#     )
#   ) %>% 
#   relocate(government, component, .after = date) %>% 
#   arrange(government, component, variable, date)
# 
# 
# 
# 
# fim_long %>% 
#   rename_with(.fn = ~snakecase::to_title_case(.),
#               .cols = everything()) %>% 
#   openxlsx::write.xlsx("results/5-2021/fim_long.xlsx")
# 
# 
# contribution %>% 
#   pivot_longer(where(is.numeric),
#                names_to = 'variable') %>% 
#   as_tibble() %>% 
#   select(-id) %>% 
#   pivot_wider(
#     names_from = date,
#     values_from = value
#   ) %>% 
#   mutate(variable = snakecase::to_title_case(variable)) %>% 
#   
#   arrange(variable) %>% 
#   openxlsx::write.xlsx('fim_output.xlsx')
# 
# 
# # Comparison --------------------------------------------------------------


# Load previous months results
previous <-
  readxl::read_xlsx('results/5-2021/fim-5-2021.xlsx') %>%
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>%
  as_tsibble(index = date) %>%
  filter_index("2020 Q2" ~ "2023 Q1")

#
current <- contributions %>%
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>%
  as_tsibble(index = date) %>%
  filter_index("2020 Q2" ~ "2023 Q1")

previous_long <- pivot_longer(previous, cols = where(is.numeric), values_to = 'previous')
current_long <- pivot_longer(current, cols = where(is.numeric), values_to = 'current')

comparison <- inner_join(previous_long,
                         current_long,
                         by = c('date', 'name', 'id')) %>%
  rename(variable = name) %>% 
  pivot_longer(c(previous, current),
               values_to = 'value',
               names_to = 'source')

comparison_nested <-
  comparison %>%
  group_by(variable) %>%
  nest() %>%
  mutate(plot = map2(.x = variable,
                     .y = data,
                     .f = ~fim::comparison_plot(.data = .y,
                                           variable = .x)))

# 
plots <- rlang::set_names(comparison_nested$plot, comparison_nested$variable)  
# rmarkdown::render('update-comparison.Rmd')
# 
# 
# contribution %>% 
#   filter_index("2020 Q1" ~ .) %>%
#   pivot_longer(where(is.numeric),
#                names_to = 'variable') %>% 
#   as_tibble() %>% 
#   select(-id) %>% 
#   pivot_wider(
#     names_from = date,
#     values_from = value
#   ) %>% 
#   mutate(variable = snakecase::to_title_case(variable)) %>% 
#   
#   arrange(variable) %>% 
#   openxlsx::write.xlsx('fim_output.xlsx')
# 
# arp %>% 
#   
#   pivot_longer(where(is.numeric),
#                names_to = 'variable') %>% 
#   as_tibble() %>% 
#   
#   pivot_wider(
#     names_from = date,
#     values_from = value
#   ) %>% 
#   mutate(variable = snakecase::to_title_case(variable)) %>% 
#   
#   arrange(variable) %>% 
#   openxlsx::write.xlsx('arp_output.xlsx')
# 
# 
# 
# contribution %>% 
#   select(date, ends_with('contribution')) %>% 
#   filter_index("2020 Q1" ~ "2023 Q1") %>% 
#   
#   pivot_longer(where(is.numeric),
#                names_to = 'variable') %>% 
#   as_tibble() %>% 
#   select(-id) %>% 
#   pivot_wider(
#     names_from = date,
#     values_from = value
#   ) %>% 
#   mutate(variable = snakecase::to_title_case(variable)) %>% 
#   
#   arrange(variable) %>% 
#   openxlsx::write.xlsx('contributions.xlsx')
# 
# april_published <- readxl::read_xlsx('results/4-2021/fim_published.xlsx')
# 
