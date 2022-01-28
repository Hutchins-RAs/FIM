
# Table -------------------------------------------------------------------


# Revisions table ---------------------------------------------------------



current_revisions <-
  current %>% 
  as_tibble() %>% 
  select(date,  federal_purchases_nipa = federal_purchases, state_purchases_nipa = state_purchases, consumption_grants,
         investment_grants) %>% 
  pivot_longer(
    where(is.numeric),
    values_to = 'current'
  ) 

previous_revisions <-
  previous %>% 
  as_tibble() %>% 
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
  filter(date <= current_quarter)

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