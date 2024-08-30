
current_quarter <- yearquarter(Sys.Date()) - 1

# Load previous months results
previous <-
  readxl::read_xlsx(glue('results/{last_month_year}/beta/contributions-{last_month_year}.xlsx')) %>% 
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>%
  as_tsibble(index = date) %>%
  filter_index("2020 Q2" ~ as.character(current_quarter + 8)) %>% 
  select(-recession)


# Select current results
current <- 
  readxl::read_xlsx(glue('results/{month_year}/beta/contributions-{month_year}.xlsx')) %>%
  drop_na(date) %>%
  mutate(date = yearquarter(date)) %>%  
  as_tsibble(index = date) %>%
  filter_index("2020 Q2" ~ as.character(current_quarter + 8)) %>%
  select(-recession)


# Pivot both longer

previous_long <- previous %>%
  mutate(across(-c(date,id), as.numeric)) %>%
  pivot_longer(
    cols = -c(date, id),  
    names_to = "name",    
    values_to = "previous"  
  )
current_long <- current %>%
  mutate(across(-c(date,id), as.numeric)) %>%
  pivot_longer(
    cols = -c(date, id),  
    names_to = "name",    
    values_to = "current"  
  )

if(current_quarter<= yearquarter("2022 Q2")){
  stloans<- current_long %>% filter(name == "federal_student_loans_contribution") %>% mutate(id = "historical") %>% mutate_where(date > yearquarter('2022 Q2'),id = "projection") %>% mutate(previous = 0) %>% select(date, name, id, previous)
  previous_long<- bind_rows(previous_long, stloans)
}

# Merge and compare
comparison <- inner_join(previous_long,
                         current_long,
                         by = c('date', 'name'))

comparison_wide <-
  comparison %>% 
  filter(date >= yearquarter("2021 Q2")) %>% 
  ungroup() %>% 
  as_tibble() %>% 
  mutate(
    current = as.numeric(current), 
    previous = as.numeric(previous),
    difference = current-previous, 
    across(where(is.numeric),
           round,
           digits = 4)) %>% 
  pivot_longer(where(is.numeric), 
               names_to = 'source') %>% 
  arrange(source) %>%
  pivot_wider(names_from = date,
              values_from = value) %>% 
  mutate(name = snakecase::to_title_case(name))

openxlsx::write.xlsx(x = comparison_wide,
                     file = glue('results\{month_year}\beta\comparison-{month_year}.xlsx'),
                     overwrite = TRUE)


# Figures -----------------------------------------------------------------

# Load previous months results
previous <-
  readxl::read_xlsx(glue('results/{last_month_year}/beta/contributions-{last_month_year}.xlsx')) %>%
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>%
  as_tsibble(index = date) %>%
  filter_index("2020 Q1" ~ as.character(current_quarter + 8)) %>% 
  select(-id, 
         -recession)

current <- 
  readxl::read_xlsx(glue('results/{month_year}/beta/contributions-{month_year}.xlsx')) %>%
  drop_na(date) %>%
  mutate(date = yearquarter(date)) %>%  
  as_tsibble(index = date) %>%
  filter_index("2020 Q2" ~ as.character(current_quarter + 8)) %>%
  select(-id, 
         -recession)

previous_long <- pivot_longer(previous, cols = where(is.numeric), values_to = 'previous')
current_long <- pivot_longer(current, cols = where(is.numeric), values_to = 'current')

if(current_quarter<= yearquarter("2022 Q2")){
  stloans<- current_long %>% 
    mutate(id = na_if(id, "historical"))%>% 
    mutate(id = na_if(id, "projection")) %>% 
    select(-id)%>%
    filter(name == "federal_student_loans" | name == "federal_student_loans_contribution" | name == "federal_student_loans_post_mpc" | name == "federal_student_loans_minus_neutral")  %>% 
    mutate(previous = 0) %>%
    select(-current)
  
  previous_long<- bind_rows(previous_long, stloans)
}

comparison <- inner_join(current_long,
                         previous_long,
                         by = c('date', 'name')) %>% 
  rename(variable = name) %>% 
  as_tsibble(index = date) 


comparison_long <-
  comparison %>% 
  pivot_longer(c(previous, current),
               names_to = 'source') 

components <- c(
  "federal_purchases_contribution", 
  "consumption_grants_contribution", 
  "investment_grants_contribution",
  "state_purchases_contribution", 
  "federal_non_corporate_taxes_contribution",
  "state_non_corporate_taxes_contribution", 
  "federal_corporate_taxes_contribution", 
  "supply_side_ira_contribution", 
  "state_corporate_taxes_contribution", 
  "federal_social_benefits_contribution", 
  "state_social_benefits_contribution", 
  "rebate_checks_contribution", 
  "rebate_checks_arp_contribution", 
  "federal_ui_contribution", 
  "state_ui_contribution", 
  "federal_subsidies_contribution",  
  "federal_aid_to_small_businesses_arp_contribution", 
  "federal_other_direct_aid_arp_contribution", 
  "federal_other_vulnerable_arp_contribution", 
  "federal_student_loans_contribution",
  "state_subsidies_contribution", 
  "federal_health_outlays_contribution", 
  "state_health_outlays_contribution", 
  "federal_contribution",
  "state_contribution", 
  "taxes_contribution", 
  "transfers_contribution",
  "fiscal_impact_measure"
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
plots <- rlang::set_names(comparison_nested$plot, comparison_nested$variable)
write_rds(plots, 'data/plots')

# Get Table-----------------------------------------

current_summary <- 
  current %>%
  select(date, federal_contribution, state_contribution, taxes_contribution, transfers_contribution, fiscal_impact_measure) %>% 
  pivot_longer(
    values_to = 'Current',
    names_to = "name", 
    -date
  ) 

previous_summary <-
  previous %>% 
  select(date, federal_contribution, state_contribution, taxes_contribution, transfers_contribution, fiscal_impact_measure) %>%
  pivot_longer(
    values_to = 'Previous',
    names_to = "name",
    -date
  )

summary <- inner_join(current_summary,
                      previous_summary,
                      by = c("date", "name")) %>% 
  mutate(Difference = Current - Previous) %>%
  mutate(name= case_when(
    name == "state_contribution" ~ "State Purchases",
    name == "federal_contribution" ~ "Federal Purchases",
    name == "fiscal_impact_measure" ~ "Fiscal Impact Measure",
    name == "taxes_contribution" ~ "Taxes",
    name == "transfers_contribution" ~ "Transfers",
    TRUE ~ "Other"
  )) %>%
  mutate(name = factor(name, levels = c("Fiscal Impact Measure", "Federal Purchases", "State Purchases", "Taxes", "Transfers"))) %>%
  arrange(date, name)


summary_tbl <- 
  summary %>% 
  as_tibble() %>%
  group_by(date) %>% 
  mutate(date = as.character(date)) %>% 
  gt(groupname_col = 'date') %>% 
  tab_style(locations = cells_title(groups = "title"),
            style = list(
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
    heading.background.color = 'royalblue4',
    data_row.padding = px(10),
    source_notes.font.size = 14,
    heading.align = "center",
    row_group.background.color = "#D0D3D4") %>% 
  tab_style(  style = list(
    cell_text(weight = "bold")
  ),
  locations = list(cells_body(rows = name == 'Fiscal Impact Measure')))








