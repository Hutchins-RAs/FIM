##########################
# Update Comparison File #
##########################

# This script generates an html file called update-comparison that is stored in the results folder. 
# This file -- colloquially called The Manu File -- allows us to compare our FIM results to what we published in the previous month. 
# You must run fiscal_impact_BETA prior to running this script. It draws in an Excel file containing
# the FIM contributions. It generates tables and figures comparing this month's results to last month's results. 

# Set Current Quarter
current_quarter <- yearquarter(Sys.Date()) - 1

#------------------- Calculate Real Levels ----------------------------#

# Load in previous month's inputs
previous_inputs <-
  #EDIT WHEN NOT JANK (REMOVE)
  readxl::read_xlsx(glue('results/01-2026/1.22/beta/inputs-01-2026.xlsx')) %>%
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
    consumption_real = consumption - lag(consumption) * consumption_deflator_growth
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
    consumption_real = consumption - lag(consumption) * consumption_deflator_growth
  )

#------------------- Contributions Figures ----------------------------#

# This section of the code pulls in the contributions and re-formats the data. 

# Load previous month's results
previous <-
  #EDIT WHEN NOT JANK (REMOVE)
  readxl::read_xlsx(glue('results/01-2026/1.22/beta/contributions-01-2026.xlsx')) %>%
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
  
  "federal_contribution",
  "fim_federal_purchases_real",
  "nipa_federal_purchases_real",
  
  "state_contribution",
  "fim_state_purchases_real",
  "nipa_state_purchases_real",
  
  "consumption_contribution",
  "consumption",
  "consumption_real",
  
  "fiscal_impact_measure"
)

# Define the comparison_ga function, which pulls in data (federal purchases 
# contribution, for example) and generates a plot comparing the previous month's 
# result to the current month's result
comparison_ga <- function(.data, variable) {
  plot_data <- .data %>% 
    filter(variable == {{ variable }}) %>%
    as_tibble() %>%
  # Remove duplicates and add year/quarter info
    group_by(date, source) %>%
    summarise(value = mean(value, na.rm = TRUE), .groups = 'drop') %>%
    mutate(
      year = year(date),
      quarter = quarter(date),
      quarter_label = paste0("Q", quarter)
    )
  
  ggplot(plot_data, aes(x = factor(quarter), y = value, fill = source)) +
    geom_col(position=position_dodge2(reverse = TRUE)) +
    labs(title = glue::glue("{snakecase::to_title_case(variable)}"),
         x = NULL,
         y = NULL,
         fill = NULL) +
    scale_x_discrete(labels = function(x) paste0("Q", x)) +
    facet_grid( ~ year(date),
                space = "free_x",
                scales = "free_x",
                switch = "x")  +
    theme(legend.position = 'top', 
          plot.title = element_text(face = "bold", size = 16, 
                                    family = "sans", 
                                    color = "gray20"),
          axis.text = element_text(size = 12, 
                                   family = "sans")) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = c('current' = "royalblue4", 
                                 'previous' = "darkgray"),
                      labels = c('Current', 'Previous'))
}

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


# Get Table-----------------------------------------

current_summary <- 
  current %>%
  select(date, federal_contribution, state_contribution, consumption_contribution, fiscal_impact_measure) %>% 
  pivot_longer(
    values_to = 'Current',
    names_to = "name", 
    -date
  ) 

previous_summary <-
  previous %>% 
  select(date, federal_contribution, state_contribution, consumption_contribution, fiscal_impact_measure) %>% 
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
    name == "consumption_contribution" ~ "Consumption",
    TRUE ~ "Other"
  )) %>%
  mutate(name = factor(name, levels = c("Fiscal Impact Measure", "Federal Purchases", "State Purchases", "Consumption"))) %>%
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

