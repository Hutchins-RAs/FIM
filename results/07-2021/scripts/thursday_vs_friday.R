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

last_month_year <- glue('{month(today())-1}-{year(today())}')
# Load previous months results
previous <-
  readxl::read_xlsx(glue('results/07-2021/fim-07-29-2021.xlsx')) %>%
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


# Figures -----------------------------------------------------------------

# Load previous months results
previous <-
  readxl::read_xlsx(glue('results/07-2021/fim-07-29-2021.xlsx')) %>%
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

current_summary <- 
  current %>% 
  as_tibble() %>% 
  as_tsibble(index = date) %>% 
  select(date, fiscal_impact, components) %>% 
  rename(federal_purchases_fim_contribution = federal_contribution,
         state_purchases_fim_contribution = state_contribution) %>% 
  mutate(federal_taxes_contribution = federal_non_corporate_taxes_contribution + federal_corporate_taxes_contribution,
         state_taxes_contribution = state_non_corporate_taxes_contribution + state_corporate_taxes_contribution) %>% 
  select(-contains('corporate')) %>% 
  pivot_longer(
    c(fiscal_impact, starts_with(c('federal', 'state'))),
    names_to = c('government', 'contribution'),
    names_pattern = '(fiscal|federal|state)_(.*)',
    values_to = 'current'
  ) %>% 
  mutate(contribution = stringr::str_remove(contribution, '_contribution'),
         government = recode(government,
                             fiscal = 'total')) %>% 
  group_by(government) %>% 
  arrange(date, factor(government, levels = c('total', 'federal', 'state')))

previous_summary <-
  previous %>% 
  as_tsibble(index = date) %>% 
  select(date, fiscal_impact, components) %>% 
  rename(federal_purchases_fim_contribution = federal_contribution,
         state_purchases_fim_contribution = state_contribution) %>% 
  mutate(federal_taxes_contribution = federal_non_corporate_taxes_contribution + federal_corporate_taxes_contribution,
         state_taxes_contribution = state_non_corporate_taxes_contribution + state_corporate_taxes_contribution) %>% 
  select(-contains('corporate')) %>% 
  pivot_longer(
    c(fiscal_impact, starts_with(c('federal', 'state'))),
    names_to = c('government', 'contribution'),
    names_pattern = '(fiscal|federal|state)_(.*)',
    values_to = 'previous'
  ) %>% 
  mutate(contribution = stringr::str_remove(contribution, '_contribution'),
         government = recode(government,
                             fiscal = 'total')) %>% 
  group_by(government) %>% 
  arrange(date, factor(government, levels = c('total', 'federal', 'state'))) 


summary <- inner_join(current_summary,
                      previous_summary,
                      by = c('date', 'government', 'contribution')) %>% 
  mutate(difference = current - previous) %>% 
  arrange(date, factor(government, levels = c('total', 'federal', 'state'))) 


summary_tbl <-
  summary %>% 
  as_tibble() %>% 
  # filter(government != 'total') %>% 
  mutate(date = as.character(date),
         across(c(government, contribution),
                ~ snakecase::to_title_case(.x))) %>% 
  group_by(date) %>% 
  gt(groupname_col = 'date',
     rowname_col = 'government') %>% 
  tab_style(
    locations = cells_title(groups = "title"),
    style     = list(
      cell_text(weight = "bold", size = 24)
    )
  ) %>%
  
  cols_label(current = md("Current<br>Friday<br>07/30"),
             previous = md("Previous<br>Thursday<br>07/29")) %>% 
  
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
    column_labels.border.top.color = "transparent",
    table.border.top.color = "transparent",
    table.border.bottom.color = "transparent",
    heading.background.color = '#003A79',
    data_row.padding = px(10),
    source_notes.font.size = 14,
    heading.align = "center",
    row_group.background.color = "#D0D3D4") %>% 
  tab_style(  style = list(
    cell_text(weight = "bold")
  ),
  locations = list(cells_body(rows = contribution == 'Impact')))

gtsave(summary_tbl, glue::glue('results/0{month_year}/summary_thursday_vs_friday.png'))

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

rmarkdown::render(input = 'update-comparison.Rmd',
                  output_dir = "results/07-2021/",
                  output_file = 'thursday_vs_friday',
                  clean = TRUE)

