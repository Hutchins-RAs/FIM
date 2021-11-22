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
  gt,
  openxlsx,
  snakecase,
  rlang
)
options(digits = 4)
options(scipen = 20)
devtools::load_all()

# Set dates for current and previous months
month_year <- glue('{format.Date(today() - 7, "%m")}-{year(today())}')
last_month_year <- glue('{format.Date(today() - 7 -months(1), "%m")}-{year(today())}')

if(!dir.exists(glue('results/{month_year}'))) {
  dir.create(glue('results/{month_year}'))
}

# Wrangle data ------------------------------------------------------------

# Since BEA put all CARES act grants to S&L in Q2 2020 we need to
# override the historical data and spread it out based on our best guess
# for when the money was spent.
overrides <- readxl::read_xlsx('data/forecast.xlsx',
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
    federal_social_benefits_gross = federal_social_benefits,
    federal_social_benefits = federal_social_benefits - ui - rebate_checks - medicare - nonprofit_provider_relief_fund,
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
  mutate_where(date >= yearquarter('2020 Q2') & date <= yearquarter('2021 Q3'),
               consumption_grants = overrides$consumption_grants_override) 
# Forecast ----------------------------------------------------------------
forecast <- 
  readxl::read_xlsx('data/forecast.xlsx',
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
  ) %>% 
  mutate_where(date >= yearquarter('2020 Q2') & date <= yearquarter('2021 Q3'),
               federal_other_direct_aid_arp = overrides$federal_other_direct_aid_arp_override,
               federal_other_vulnerable_arp = overrides$federal_other_vulnerable_arp_override,
               federal_social_benefits = overrides$federal_social_benefits_override) %>% 
  mutate_where(date == yearquarter("2021 Q1"),
               federal_social_benefits = federal_social_benefits + 203) %>% 
  mutate_where(date == yearquarter("2021 Q3"),
               federal_corporate_taxes = 290,
               state_corporate_taxes = 119.9)

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


openxlsx::write.xlsx(contributions, file = glue('results/{month_year}/fim-{month_year}.xlsx'), overwrite = TRUE)
write_rds(contributions, file = 'data/contributions.rds')


# Web materials  -------------------------------------------------------------

# Interactive data
interactive <- 
  contributions %>% 
  filter_index('1999 Q4' ~ '2023 Q2') %>% 
  mutate(consumption = transfers_contribution + taxes_contribution,
         recession = recode(recession, `-1` = 0),
         recession = replace_na(recession, 0),
         id = recode(id, 
                     historical = 0,
                     projection = 1)) %>% 
  select(date, 
         impact = fiscal_impact_moving_average,
         recession,
         total = fiscal_impact,
         federal = federal_contribution,
         state_local = state_contribution,
         consumption,
         projection = id) %>% 
  separate(date, c('year', 'quarter'))

readr::write_csv(interactive,  file = glue('results/{month_year}/interactive-{month_year}.csv'))

# Figures for website
rmarkdown::render('Fiscal-Impact.Rmd',
                  output_file = glue::glue('results/{month_year}/Fiscal-Impact-{month_year}'),
                  clean = TRUE)

# Comparison ------------------------------------------------------------

# Load previous months results
previous <-
  readxl::read_xlsx(glue('results/{last_month_year}/fim-{last_month_year}.xlsx')) %>%
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>%
  as_tsibble(index = date) %>%
  filter_index("2020 Q2" ~ "2023 Q4")
# Select current results
current <- 
  contributions %>%
  drop_na(date) %>%
  filter_index("2020 Q2" ~ "2023 Q4") %>% 
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
  select(-id) %>% 
  pivot_wider(names_from = date,
              values_from = value) %>%
  mutate(name = snakecase::to_title_case(name)) 

comparison_deflators <-
  comparison %>% 
  select(date,id, contains('deflator'), previous, current) %>% 
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
  select(-id) %>% 
  pivot_wider(names_from = date,
              values_from = value) %>%
  mutate(name = snakecase::to_title_case(name)) 

openxlsx::write.xlsx(x = comparison_deflators,
                     file = glue('results/{month_year}/{month_year}_comparison_deflators.xlsx'),
                     overwrite = TRUE)
openxlsx::write.xlsx(x = comparison_wide,
                     file = glue('results/{month_year}/{month_year}/contributions_comparison.xlsx'),
                     overwrite = TRUE)


# Figures -----------------------------------------------------------------

# Load previous months results
previous <-
  readxl::read_xlsx(glue('results/{last_month_year}/fim-{last_month_year}.xlsx')) %>%
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>%
  as_tsibble(index = date) %>%
  filter_index("2020 Q1" ~ "2023 Q4") %>% 
  select(-id)
# Select current results
current <- 
  contributions %>%
  drop_na(date) %>%
  filter_index("2020 Q1" ~ "2023 Q4") %>% 
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

comparison

comparison_nested <-
  comparison_long %>%
  group_by(variable) %>%
  nest() %>%
  mutate(plot = map2(.x = variable,
                     .y = data,
                     .f = ~comparison_plot(.data = .y,
                                           variable = .x)))


plots <- rlang::set_names(comparison_nested$plot, comparison_nested$variable)


# Table -------------------------------------------------------------------


# Revisions table ---------------------------------------------------------



current_revisions <-
  current %>% 
  as_tibble() %>% 
  mutate(consumption_grants = gross_consumption_grants - medicaid_grants) %>% 
  select(date,  federal_purchases_nipa = federal_purchases, state_purchases_nipa = state_purchases, consumption_grants,
         investment_grants) %>% 
  pivot_longer(
    where(is.numeric),
    values_to = 'current'
  ) 

previous_revisions <-
  previous %>% 
  as_tibble() %>% 
  mutate(consumption_grants = gross_consumption_grants - medicaid_grants) %>% 
  mutate_where(date == yearquarter("2021 Q3"),
               consumption_grants = NA_real_) %>% 
  select(date, federal_purchases_nipa = federal_purchases, state_purchases_nipa = state_purchases, consumption_grants,
         investment_grants) %>% 
  pivot_longer(
    where(is.numeric),
    values_to = 'previous'
  ) 

revisions <- inner_join(current_revisions, previous_revisions,
                        by = c('date', 'name')) %>% 
  mutate(diff = current - previous,
         diff_pct = (current / previous) - 1) %>% 
  filter(date <= yearquarter("2021 Q3"))

revisions_tbl <-
  revisions %>% 
  group_by(date) %>% 
  mutate(date = as.character(date),
         name = snakecase::to_title_case(name)) %>% 
  gt(groupname_col = 'date') %>% 
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = 24)
    )
  ) %>%
  
  opt_row_striping() %>%
  tab_header(title = md('BEA Revisions on Purchases')) %>%
  fmt_currency(where(is.numeric)) %>% 
  fmt_percent(diff_pct) %>% 
  
  opt_all_caps() %>%
  opt_table_font(
    font = list(
      google_font("Roboto"),
      default_fonts()))  %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "bottom", weight = px(3)),
      #Make text bold
      cell_text(weight = "bold"))) %>%
  tab_options(
    column_labels.border.top.width = px(10),
    # column_labels.border.top.color = "transparent",
    # table.border.top.color = "transparent",
    # table.border.bottom.color = "transparent",
    heading.background.color = '#003A79',
    data_row.padding = px(10),
    source_notes.font.size = 14,
    heading.align = "center",
    row_group.background.color = "#D0D3D4") 
#fim since 2020 q1 and decomposed into changes due to fed taxes, fed transfers, fed purchases and same list for state.
#That table should be a standard table we produce every time we run the FIM.
components <- c(
  'federal_contribution',
  'state_contribution', 
  'federal_transfers_contribution',
  'federal_corporate_taxes_contribution',
  'federal_non_corporate_taxes_contribution',
  'state_transfers_contribution',
  'state_corporate_taxes_contribution',
  'state_non_corporate_taxes_contribution')

transfers <- c('social_benefits', 'subsidies', 'aid_to_small_businesses_arp', 'health_outlays', 'rebate_checks', 'rebate_checks_arp', 'ui', 'other_direct_aid_arp', 'other_vulnerable_arp')


current_summary <- 
  current %>% 
  rename(federal_rebate_checks_contribution = rebate_checks_contribution, 
         federal_rebate_checks_arp_contribution = rebate_checks_arp_contribution,
  ) %>% 
  select(date,
         matches('federal|state') & matches('_contribution') | matches('consumption_grants_|investment_grants_') | matches('fiscal_impact$')) %>% 
  rename(total_fiscal_impact_contribution = fiscal_impact) %>% 
  mutate(
    federal_purchases_contribution = federal_purchases_contribution + consumption_grants_contribution + investment_grants_contribution,
    state_purchases_contribution = state_purchases_contribution - consumption_grants_contribution - investment_grants_contribution,
    
  ) %>% 
  select(-matches('consumption_grants_|investment_grants_')) %>% 
  pivot_longer(
    -c(date, id),
    names_to = c('government', 'variable', 'level'),
    names_pattern = '(total|federal|state)_(.*)_(.*)'
  ) %>% 
  ungroup() %>% 
  mutate(category = case_when(variable %in% transfers ~ 'Transfers',
                              str_detect(variable, 'taxes') ~ 'Taxes',
                              str_detect(variable, 'purchases') ~ "Purchases",
                              variable == 'fiscal_impact' ~ 'Fiscal Impact'), .after = 'government') %>% 
  drop_na() %>% 
  as_tibble() %>% 
  group_by(date, government, category) %>% 
  summarise(value = sum(value)) %>% 
  arrange(date, factor(government, levels = c('total','federal', 'state')))



previous_summary <-
  previous %>% 
  rename(federal_rebate_checks_contribution = rebate_checks_contribution, 
         federal_rebate_checks_arp_contribution = rebate_checks_arp_contribution,
  ) %>% 
  select(-federal_purchases_contribution, -state_purchases_contribution) %>% 
  rename(federal_purchases_contribution = federal_contribution,
         state_purchases_contribution = state_contribution) %>% 
  select(date,
         matches('federal|state') & matches('_contribution') | matches('consumption_grants_|investment_grants_') | matches('fiscal_impact$')) %>% 
  rename(total_fiscal_impact_contribution = fiscal_impact) %>% 
  select(-matches('consumption_grants_|investment_grants_')) %>% 
  pivot_longer(
    -c(date),
    names_to = c('government', 'variable', 'level'),
    names_pattern = '(total|federal|state)_(.*)_(.*)'
  ) %>% 
  ungroup() %>% 
  mutate(category = case_when(variable %in% transfers ~ 'Transfers',
                              str_detect(variable, 'taxes') ~ 'Taxes',
                              str_detect(variable, 'purchases') ~ "Purchases",
                              variable == 'fiscal_impact' ~ 'Fiscal Impact'), .after = 'government') %>% 
  drop_na() %>% 
  as_tibble() %>% 
  group_by(date, government, category) %>% 
  summarise(value = sum(value)) %>% 
  arrange(date, factor(government, levels = c('total','federal', 'state')))




summary <- inner_join(current_summary,
                      previous_summary,
                      by = c('date','government', 'category')) %>% 
  rename(current = value.x,
         previous = value.y) %>% 
  mutate(difference = current - previous)

summary %>% 
  as_tibble() %>% 
  # filter(government != 'total') %>% 
  mutate_where(category == "Purchases",
               category = "Purchases FIM") %>% 
  group_by(date) 

summary_tbl <-
  summary %>% 
  as_tibble() %>% 
  # filter(government != 'total') %>% 
  mutate_where(category == "Purchases",
               category = "Purchases FIM") %>% 
  group_by(date) %>% 
  mutate(date = as.character(date)) %>% 
  gt(groupname_col = 'date',
     rowname_col = 'government') %>% 
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = 24)
    )
  ) %>%
  
  opt_row_striping() %>%
  tab_header(title = md('FIM Components Summary')) %>%
  fmt_percent(where(is.numeric),
              scale_values = FALSE) %>% 
  
  opt_all_caps() %>%
  opt_table_font(
    font = list(
      google_font("Roboto"),
      default_fonts()))  %>%
  tab_style(
    locations = cells_column_labels(columns = everything()),
    style     = list(
      #Give a thick border below
      cell_borders(sides = "bottom", weight = px(3)),
      #Make text bold
      cell_text(weight = "bold"))) %>%
  tab_options(
    column_labels.border.top.width = px(10),
    # column_labels.border.top.color = "transparent",
    # table.border.top.color = "transparent",
    # table.border.bottom.color = "transparent",
    heading.background.color = '#003A79',
    data_row.padding = px(10),
    source_notes.font.size = 14,
    heading.align = "center",
    row_group.background.color = "#D0D3D4") %>% 
  tab_style(  style = list(
    cell_text(weight = "bold")
  ),
  locations = list(cells_body(rows = category == 'Fiscal Impact')))

# Deflators ---------------------------------------------------------------



deflators <- inner_join(previous_long,
                        current_long,
                        by = c('date', 'name' )) %>% 
  mutate(diff = current - previous) %>% 
  rename(variable = name) %>% 
  filter(str_detect(variable, 'deflator_growth')) %>% 
  mutate(across(where(is.numeric),
                ~ ((1 + .x)^4))-1)

diff_plot <-
  deflators %>% 
  mutate(variable = snakecase::to_title_case(variable)) %>% 
  ggplot(aes(x = date,  y =  diff, color = variable)) +
  #geom_col(position=position_dodge2(reverse = TRUE)) +
  geom_line(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(variable~.) +
  scale_y_continuous(labels = scales::label_percent()) +
  
  labs(title = 'Difference in deflator growth', 
       subtitle = 'Annualized',
       x = NULL,
       y = NULL)

cur_plot <-
  deflators %>% 
  mutate(variable = snakecase::to_title_case(variable)) %>% 
  ggplot(aes(x = date,  y =  current, color = variable)) +
  #geom_col(position=position_dodge2(reverse = TRUE)) +
  geom_line(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(variable~.) +
  scale_y_continuous(labels = scales::label_percent()) +
  geom_hline(yintercept = 0) +
  labs(title = 'Current deflator growth', 
       subtitle = 'Annualized',
       x = NULL,
       y = NULL)

prev_plot <-
  deflators %>% 
  mutate(variable = snakecase::to_title_case(variable)) %>% 
  ggplot(aes(x = date,  y =  previous, color = variable)) +
  #geom_col(position=position_dodge2(reverse = TRUE)) +
  geom_line(show.legend = FALSE) +
  geom_point(show.legend = FALSE) +
  facet_wrap(variable~.) +
  scale_y_continuous(labels = scales::label_percent()) +
  geom_hline(yintercept = 0) +
  labs(title = 'Previous deflator growth', 
       subtitle = 'Annualized',
       x = NULL,
       y = NULL)

rmarkdown::render(input = 'index.Rmd',
                  clean = TRUE)

if (dir.exists(glue('results/{month_year}'))) {
  file.copy('index.html',
            glue('results/{month_year}/update-comparison-{month_year}'))
}

