##########################
# Memo Data File #
##########################

# This script modifies the index_temp file that we use to create our manu file
# Here we are using it to create our memo file and get numbers

# Set Current Quarter
current_quarter <- yearquarter(Sys.Date()) - 1

#------------------- Calculate Real Levels ----------------------------#

# Load in previous month's inputs
previous_inputs <-
  #EDIT WHEN NOT JANK (REMOVE)
  #############################################################################
  readxl::read_xlsx(glue('results/02-2026/beta/inputs-02-2026.xlsx')) %>%
  #############################################################################
  #WHAT IT USED TO BE 


  #readxl::read_xlsx(glue('results/{last_month_year}/beta/inputs-{last_month_year}.xlsx')) %>%
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>%
  as_tsibble(index = date) %>%
  filter_index("2024 Q1" ~ as.character(current_quarter + 8)) %>%
  select(-id,
         -recession)

# Load the current month's inputs 
current_inputs <- 
  readxl::read_xlsx(glue('results/{month_year}/beta/inputs-{month_year}.xlsx')) %>%
  drop_na(date) %>%
  mutate(date = yearquarter(date)) %>%  
  as_tsibble(index = date) %>%
  filter_index("2024 Q1" ~ as.character(current_quarter + 8)) %>%
  select(-id, 
         -recession)

# Use the nominal values and consumption deflator to calculate the real levels 
# Previous
previous_inputs <- previous_inputs %>%
  mutate(
    # Calculate real federal/state purchases that are NIPA consistent 
    nipa_federal_purchases_real = federal_purchases - lag(federal_purchases) * federal_purchases_deflator_growth,
    nipa_state_purchases_real = state_purchases - lag(state_purchases) * state_purchases_deflator_growth,

    nipa_total_purchases = federal_purchases + state_purchases,
    nipa_total_purchases_real = nipa_federal_purchases_real + nipa_state_purchases_real,
    
    # Calculate real federal/state purchases that are FIM consistent 
    fim_state_purchases = state_purchases + consumption_grants + investment_grants,
    fim_state_purchases_real = fim_state_purchases - lag(fim_state_purchases) * state_purchases_deflator_growth,
    
    fim_federal_purchases = nipa_total_purchases - fim_state_purchases,
    fim_federal_purchases_real = nipa_total_purchases_real - fim_state_purchases_real,
    
    # Calculate consumption
    consumption_real = consumption - lag(consumption) * consumption_deflator_growth,

    federal_non_corporate_taxes,
    federal_non_corporate_taxes_real = federal_non_corporate_taxes - lag(federal_non_corporate_taxes) * consumption_deflator_growth,
    
    state_non_corporate_taxes,
    state_non_corporate_taxes_real = state_non_corporate_taxes - lag(state_non_corporate_taxes) * consumption_deflator_growth,
    
    federal_corporate_taxes,
    federal_corporate_taxes_real = federal_corporate_taxes - lag(federal_corporate_taxes) * consumption_deflator_growth,
    
    supply_side_ira,
    supply_side_ira_real = supply_side_ira - lag(supply_side_ira) * consumption_deflator_growth,
    
    state_corporate_taxes,
    state_corporate_taxes_real = state_corporate_taxes - lag(state_corporate_taxes) * consumption_deflator_growth,
    
    federal_social_benefits,
    federal_social_benefits_real = federal_social_benefits - lag(federal_social_benefits) * consumption_deflator_growth,
    
    state_social_benefits,
    state_social_benefits_real = state_social_benefits - lag(state_social_benefits) * consumption_deflator_growth,
    
    federal_ui,
    federal_ui_real = federal_ui - lag(federal_ui) * consumption_deflator_growth,
    
    state_ui,
    state_ui_real = state_ui - lag(state_ui) * consumption_deflator_growth,
    
    federal_subsidies,
    federal_subsidies_real = federal_subsidies - lag(federal_subsidies) * consumption_deflator_growth,
    
    federal_student_loans,
    federal_student_loans_real = federal_student_loans - lag(federal_student_loans) * consumption_deflator_growth,
    
    state_subsidies,
    state_subsidies_real = state_subsidies - lag(state_subsidies) * consumption_deflator_growth,
    
    federal_health_outlays,
    federal_health_outlays_real = federal_health_outlays - lag(federal_health_outlays) * consumption_deflator_growth,
    
    state_health_outlays,
    state_health_outlays_real = state_health_outlays - lag(state_health_outlays) * consumption_deflator_growth
  )

# Current 
current_inputs <- current_inputs %>%
  mutate(
    # Calculate real federal/state purchases that are NIPA consistent 
    nipa_federal_purchases_real = federal_purchases - lag(federal_purchases) * federal_purchases_deflator_growth,
    nipa_state_purchases_real = state_purchases - lag(state_purchases) * state_purchases_deflator_growth,
    
    nipa_total_purchases = federal_purchases + state_purchases,
    nipa_total_purchases_real = nipa_federal_purchases_real + nipa_state_purchases_real,
    
    # Calculate real federal/state purchases that are FIM consistent 
    fim_state_purchases = state_purchases + consumption_grants + investment_grants,
    fim_state_purchases_real = fim_state_purchases - lag(fim_state_purchases) * state_purchases_deflator_growth,
    
    fim_federal_purchases = nipa_total_purchases - fim_state_purchases,
    fim_federal_purchases_real = nipa_total_purchases_real - fim_state_purchases_real,
    
    # Calculate consumption
    consumption_real = consumption - lag(consumption) * consumption_deflator_growth,
    
    federal_non_corporate_taxes,
    federal_non_corporate_taxes_real = federal_non_corporate_taxes - lag(federal_non_corporate_taxes) * consumption_deflator_growth,
    
    state_non_corporate_taxes,
    state_non_corporate_taxes_real = state_non_corporate_taxes - lag(state_non_corporate_taxes) * consumption_deflator_growth,
    
    federal_corporate_taxes,
    federal_corporate_taxes_real = federal_corporate_taxes - lag(federal_corporate_taxes) * consumption_deflator_growth,
    
    supply_side_ira,
    supply_side_ira_real = supply_side_ira - lag(supply_side_ira) * consumption_deflator_growth,
    
    state_corporate_taxes,
    state_corporate_taxes_real = state_corporate_taxes - lag(state_corporate_taxes) * consumption_deflator_growth,
    
    federal_social_benefits,
    federal_social_benefits_real = federal_social_benefits - lag(federal_social_benefits) * consumption_deflator_growth,
    
    state_social_benefits,
    state_social_benefits_real = state_social_benefits - lag(state_social_benefits) * consumption_deflator_growth,
    
    federal_ui,
    federal_ui_real = federal_ui - lag(federal_ui) * consumption_deflator_growth,
    
    state_ui,
    state_ui_real = state_ui - lag(state_ui) * consumption_deflator_growth,
    
    federal_subsidies,
    federal_subsidies_real = federal_subsidies - lag(federal_subsidies) * consumption_deflator_growth,
    
    federal_student_loans,
    federal_student_loans_real = federal_student_loans - lag(federal_student_loans) * consumption_deflator_growth,
    
    state_subsidies,
    state_subsidies_real = state_subsidies - lag(state_subsidies) * consumption_deflator_growth,
    
    federal_health_outlays,
    federal_health_outlays_real = federal_health_outlays - lag(federal_health_outlays) * consumption_deflator_growth,
    
    state_health_outlays,
    state_health_outlays_real = state_health_outlays - lag(state_health_outlays) * consumption_deflator_growth
  )

#------------------- Contributions Figures ----------------------------#

# This section of the code pulls in the contributions and re-formats the data. 

# Load previous month's results
previous <-
  #EDIT WHEN NOT JANK (REMOVE)
  ###################################################################################
  readxl::read_xlsx(glue('results/02-2026/beta/contributions-02-2026.xlsx')) %>%
  ###################################################################################
  #WHAT IT USED TO BE 
  #readxl::read_xlsx(glue('results/{last_month_year}/beta/contributions-{last_month_year}.xlsx')) %>%
  mutate(date = yearquarter(date)) %>%
  drop_na(date) %>%
  as_tsibble(index = date) %>%
  filter_index("2024 Q1" ~ as.character(current_quarter + 8)) %>% 
  select(-id, 
         -recession)

# Load in current month's results 
current <- 
  readxl::read_xlsx(glue('results/{month_year}/beta/contributions-{month_year}.xlsx')) %>%
  drop_na(date) %>%
  mutate(date = yearquarter(date)) %>%  
  as_tsibble(index = date) %>%
  filter_index("2024 Q1" ~ as.character(current_quarter + 8)) %>%
  select(-id, 
         -recession)

#-------------- Clean the Data -------------------------------------# 

# Reshape and join the data 
previous_long <- pivot_longer(previous, cols = where(is.numeric), values_to = 'previous')
current_long <- pivot_longer(current, cols = where(is.numeric), values_to = 'current')
previous_inputs_long <- pivot_longer(previous_inputs, cols = where(is.numeric), values_to = 'previous')
current_inputs_long <- pivot_longer(current_inputs, cols = where(is.numeric), values_to = 'current')

# Join Contributions 
contributions_comparison <- inner_join(current_long,
                                       previous_long,
                                       by = c('date', 'name')) %>% 
  rename(variable = name) %>% 
  as_tibble(index = date) %>%
  mutate(date = as.Date(date)) 

inputs_comparison <- inner_join(current_inputs_long,
                                previous_inputs_long,
                                by = c('date', 'name')) %>% 
  rename(variable = name) %>% 
  as_tsibble(index = date) 

# Append 
comparison <- bind_rows(inputs_comparison, contributions_comparison)

comparison_long <-
  comparison %>%
  pivot_longer(c(previous, current),
               names_to = 'source')

# Define the "components", i.e. the data we want to include in our contributions comparison plots 
components <- c(
  
  # --- FIM ---
  "fiscal_impact_measure",
  
  # --- Federal Purchases ---
  "federal_contribution",
  "fim_federal_purchases_real",
  "nipa_federal_purchases_real",
  
  # --- State Purchases ---
  "state_contribution",
  "fim_state_purchases_real",
  "nipa_state_purchases_real",
  
  # --- Consumption ---
  "consumption_contribution",
  "consumption",
  "consumption_real",
  
  # --- Taxes ---
  "federal_non_corporate_taxes",
  "federal_non_corporate_taxes_real",
  "state_non_corporate_taxes",
  "state_non_corporate_taxes_real",
  "federal_corporate_taxes",
  "federal_corporate_taxes_real",
  "state_corporate_taxes",
  "state_corporate_taxes_real",
  "supply_side_ira",
  "supply_side_ira_real",
  
  # --- Social Benefits ---
  "federal_social_benefits",
  "federal_social_benefits_real",
  "state_social_benefits",
  "state_social_benefits_real",
  
  # --- UI ---
  "federal_ui",
  "federal_ui_real",
  "state_ui",
  "state_ui_real",
  
  # --- Subsidies ---
  "federal_subsidies",
  "federal_subsidies_real",
  "state_subsidies",
  "state_subsidies_real",
  
  # --- Student Loans ---
  "federal_student_loans",
  "federal_student_loans_real",
  
  # --- Health Outlays ---
  "federal_health_outlays",
  "federal_health_outlays_real",
  "state_health_outlays",
  "state_health_outlays_real"
)

# Define input categories for grouping in the table
input_categories <- tribble(
  ~variable,                          ~category,
  "federal_contribution",             "Federal Purchases",
  "fim_federal_purchases_real",       "Federal Purchases",
  "nipa_federal_purchases_real",      "Federal Purchases",
  "state_contribution",               "State Purchases",
  "fim_state_purchases_real",         "State Purchases",
  "nipa_state_purchases_real",        "State Purchases",
  "consumption_contribution",         "Consumption",
  "consumption",                      "Consumption",
  "consumption_real",                 "Consumption",
  "fiscal_impact_measure",            "Overall",
  "federal_non_corporate_taxes",      "Taxes",
  "federal_non_corporate_taxes_real", "Taxes",
  "state_non_corporate_taxes",        "Taxes",
  "state_non_corporate_taxes_real",   "Taxes",
  "federal_corporate_taxes",          "Taxes",
  "federal_corporate_taxes_real",     "Taxes",
  "state_corporate_taxes",            "Taxes",
  "state_corporate_taxes_real",       "Taxes",
  "supply_side_ira",                  "Taxes",
  "supply_side_ira_real",             "Taxes",
  "federal_social_benefits",          "Social Benefits",
  "federal_social_benefits_real",     "Social Benefits",
  "state_social_benefits",            "Social Benefits",
  "state_social_benefits_real",       "Social Benefits",
  "federal_ui",                       "UI",
  "federal_ui_real",                  "UI",
  "state_ui",                         "UI",
  "state_ui_real",                    "UI",
  "federal_subsidies",                "Subsidies",
  "federal_subsidies_real",           "Subsidies",
  "state_subsidies",                  "Subsidies",
  "state_subsidies_real",             "Subsidies",
  "federal_student_loans",            "Student Loans",
  "federal_student_loans_real",       "Student Loans",
  "federal_health_outlays",           "Health Outlays",
  "federal_health_outlays_real",      "Health Outlays",
  "state_health_outlays",             "Health Outlays",
  "state_health_outlays_real",        "Health Outlays"
)

#fix pesky titles not being capitalized
fim_title_case <- function(x) {
  snakecase::to_title_case(x) %>%
    str_replace_all("\\bUi\\b", "UI") %>%
    str_replace_all("\\bIra\\b", "IRA") %>%
    str_replace_all("\\bNipa\\b", "NIPA") %>%
    str_replace_all("\\bFim\\b", "FIM")
}


# Generate plots
comparison_nested <-
  comparison_long %>%
  filter(variable %in% components) %>% 
  group_by(variable) %>%
  nest() %>%
  mutate(plot = map2(.x = variable,
                     .y = data,
                     .f = ~comparison_ga(.data = .y,
                                         variable = .x)))

write_rds(comparison_nested, 'data/comparison_nested')
plots <- rlang::set_names(comparison_nested$plot, 
                          comparison_nested$variable)
write_rds(plots, 'data/plots')


# Get Table -----------------------------------------

current_summary <-
  current %>%
  pivot_longer(
    cols = where(is.numeric),
    values_to = 'Current',
    names_to = "name"
  )

previous_summary <-
  previous %>%
  pivot_longer(
    cols = where(is.numeric),
    values_to = 'Previous',
    names_to = "name"
  )

summary <- inner_join(current_summary,
                      previous_summary,
                      by = c("date", "name")) %>%
  rename(variable = name) %>%
  as_tibble() %>%                    # drop tsibble before touching date
  filter(variable %in% components) %>%
  left_join(input_categories, by = "variable") %>%
  mutate(
    Difference = Current - Previous,
    date = as.character(date),
    variable = fim_title_case(variable),
    category = factor(category, levels = c(
      "Overall",
      "Federal Purchases",
      "State Purchases", 
      "Consumption",
      "Taxes",
      "Social Benefits",
      "UI",
      "Subsidies",
      "Student Loans",
      "Health Outlays"
    ))
  ) %>%
  arrange(category, variable) %>%
  filter(date >= as.character(current_quarter - 7),
         date <= as.character(current_quarter + 8))

summary_wide <- summary %>%
  select(category, variable, date, Difference) %>%
  pivot_wider(names_from = date,
              values_from = Difference)

summary_tbl <-
  summary_wide %>%
  group_by(category) %>%
  gt(groupname_col = "category") %>%
  tab_header(
    title = md("**FIM Inputs & Contributions — Revision Summary**"),
    subtitle = md(glue("*{current_quarter - 7} through {current_quarter + 8}*"))
  ) %>%
  fmt_number(where(is.numeric), decimals = 1) %>%
  tab_style(
    style = cell_fill(color = "#e8edf5"),
    locations = cells_body(columns = as.character(current_quarter))
  ) %>%
  tab_style(
    style = cell_text(color = "red", weight = "bold"),
    locations = cells_body(
      columns = where(is.numeric),
      rows = apply(select(summary_wide, where(is.numeric)), 1, function(x) any(abs(x) > 5, na.rm = TRUE))
    )
  ) %>%
  tab_spanner(
    label = "History",
    columns = 3:10
  ) %>%
  tab_spanner(
    label = "Forecast",
    columns = 11:18
  ) %>%
  opt_row_striping() %>%
  opt_all_caps() %>%
  opt_table_font(
    font = list(google_font("Roboto"), default_fonts())
  ) %>%
  tab_options(
    heading.background.color = "royalblue4",
    heading.align = "center",
    row_group.background.color = "#D0D3D4",
    data_row.padding = px(8)
  )
















# 
# 
# # Define the comparison_ga function, which pulls in data (federal purchases 
# # contribution, for example) and generates a plot comparing the previous month's 
# # result to the current month's result
# comparison_ga <- function(.data, variable) {
#   plot_data <- .data %>% 
#     filter(variable == {{ variable }}) %>%
#     as_tibble() %>%
#   # Remove duplicates and add year/quarter info
#     group_by(date, source) %>%
#     summarise(value = mean(value, na.rm = TRUE), .groups = 'drop') %>%
#     mutate(
#       year = year(date),
#       quarter = quarter(date),
#       quarter_label = paste0("Q", quarter)
#     )
#   
#   ggplot(plot_data, aes(x = factor(quarter), y = value, fill = source)) +
#     geom_col(position=position_dodge2(reverse = TRUE)) +
#     labs(title = glue::glue("{snakecase::to_title_case(variable)}"),
#          x = NULL,
#          y = NULL,
#          fill = NULL) +
#     scale_x_discrete(labels = function(x) paste0("Q", x)) +
#     facet_grid( ~ year(date),
#                 space = "free_x",
#                 scales = "free_x",
#                 switch = "x")  +
#     theme(legend.position = 'top', 
#           plot.title = element_text(face = "bold", size = 16, 
#                                     family = "sans", 
#                                     color = "gray20"),
#           axis.text = element_text(size = 12, 
#                                    family = "sans")) +
#     guides(fill = guide_legend(reverse = TRUE)) +
#     scale_fill_manual(values = c('current' = "royalblue4", 
#                                  'previous' = "darkgray"),
#                       labels = c('Current', 'Previous'))
# }
# 
# comparison_nested <-
#   comparison_long %>%
#   filter(variable %in% components) %>% 
#   group_by(variable) %>%
#   nest() %>%
#   mutate(plot = map2(.x = variable,
#                      .y = data,
#                      .f = ~comparison_ga(.data = .y,
#                                          variable = .x)))
# 
# write_rds(comparison_nested, 'data/comparison_nested')
# plots <- rlang::set_names(comparison_nested$plot, 
#                           comparison_nested$variable)
# write_rds(plots, 'data/plots')
# 
# 
# # Get Table-----------------------------------------
# 
# current_summary <- 
#   current %>%
#   select(date, federal_contribution, state_contribution, consumption_contribution, fiscal_impact_measure) %>% 
#   pivot_longer(
#     values_to = 'Current',
#     names_to = "name", 
#     -date
#   ) 
# 
# previous_summary <-
#   previous %>% 
#   select(date, federal_contribution, state_contribution, consumption_contribution, fiscal_impact_measure) %>% 
#   pivot_longer(
#     values_to = 'Previous',
#     names_to = "name",
#     -date
#   )
# 
# summary <- inner_join(current_summary,
#                       previous_summary,
#                       by = c("date", "name")) %>% 
#   mutate(Difference = Current - Previous) %>%
#   mutate(name= case_when(
#     name == "state_contribution" ~ "State Purchases",
#     name == "federal_contribution" ~ "Federal Purchases",
#     name == "fiscal_impact_measure" ~ "Fiscal Impact Measure",
#     name == "consumption_contribution" ~ "Consumption",
#     TRUE ~ "Other"
#   )) %>%
#   mutate(name = factor(name, levels = c("Fiscal Impact Measure", "Federal Purchases", "State Purchases", "Consumption"))) %>%
#   arrange(date, name)
# 
# 
# summary_tbl <- 
#   summary %>% 
#   as_tibble() %>%
#   group_by(date) %>% 
#   mutate(date = as.character(date)) %>% 
#   gt(groupname_col = 'date') %>% 
#   tab_style(locations = cells_title(groups = "title"),
#             style = list(
#               cell_text(weight = "bold", size = 24)
#             )
#   ) %>% 
#   
#   
#   opt_row_striping() %>%
#   tab_header(title = md('FIM Components Summary')) %>%
#   fmt_percent(where(is.numeric),
#               scale_values = FALSE) %>% 
#   
#   opt_all_caps() %>%
#   opt_table_font(
#     font = list(
#       google_font("Roboto"),
#       default_fonts()))  %>%
#   tab_style(
#     locations = cells_column_labels(columns = everything()),
#     style     = list(
#       #Give a thick border below
#       cell_borders(sides = "bottom", weight = px(3)),
#       #Make text bold
#       cell_text(weight = "bold"))) %>%
#   tab_options(
#     column_labels.border.top.width = px(10),
#     # column_labels.border.top.color = "transparent",
#     # table.border.top.color = "transparent",
#     # table.border.bottom.color = "transparent",
#     heading.background.color = 'royalblue4',
#     data_row.padding = px(10),
#     source_notes.font.size = 14,
#     heading.align = "center",
#     row_group.background.color = "#D0D3D4") %>% 
#   tab_style(  style = list(
#     cell_text(weight = "bold")
#   ),
#   locations = list(cells_body(rows = name == 'Fiscal Impact Measure')))

