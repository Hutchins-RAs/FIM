
# Setup -------------------------------------------------------------------
librarian::shelf(tidyverse, gt)
my_theme <- function(data, ...){
  data %>%
    opt_row_striping() %>%
    opt_all_caps() %>%
    opt_table_font(
      font = list(
        google_font("Roboto"),
        default_fonts()))  %>%
    tab_style(
      locations = cells_title(groups = "title"),
      style     = list(
        cell_text(weight = "bold", size = 24)
      )
    ) %>%
    cols_align(where(is.numeric),
               align = 'right') %>%
    cols_align(where(is.character),
               align = 'left') %>%
    tab_style(
      locations = cells_column_labels(columns = everything()),
      style     = list(
        #Give a thick border below
        cell_borders(sides = "bottom", weight = px(3)),
        #Make text bold
        cell_text(weight = "bold")
      )
    ) %>%
    #Apply different style to the title
    tab_style(
      locations = cells_title(groups = "title"),
      style     = list(
        cell_text(weight = "bold", size = 24)
      )
    ) %>%
    tab_options(
      column_labels.border.top.width = px(5),
      column_labels.border.top.color = "#FFFFFF",
      table.border.top.color = "#FFFFFF",
      table.border.bottom.color = "#FFFFFF",
      heading.background.color = '#003A79',
      data_row.padding = px(5),
      source_notes.font.size = 12,
      heading.align = "center",
      row_group.background.color = '#D0D3D4',
      ...)
}


# Data --------------------------------------------------------------------


mpc_data_raw <- readxl::read_xlsx('data/forecast.xlsx', sheet = 'mpc',
                               range = "B2:N14")

order <- c('social_benefits', 'health_outlays', 'ui', 'ui_arp', 'rebate_checks', 'rebate_checks_arp', 'other_direct_aid_arp', 'other_vulnerable_arp', 'subsidies', 'aid_to_small_businesses_arp', 'non_corporate_taxes', 'corporate_taxes')


# Cleaning ----------------------------------------------------------------

mpc_data <-
  mpc_data_raw %>% 
  pivot_longer(-variable) %>% 
  group_by(variable) %>% 
  summarise(mpc = sum(value)) %>% 
  mutate(variable = as_factor(variable),
         variable = fct_relevel(variable, rev(order))) %>% 
  arrange(desc(variable))

mpc_descriptions <-
  tribble(~variable, ~description,
        'social_benefits', "Effect occurs smoothly in the first year.",
        'health_outlays', "Effect occurs smoothly in the first year.",
        'ui', "70 percent of the effect occur within the first two quarters and the remainder is spread out over four quarters at a decreasing rate.",
        'ui_arp', "Roughly 75 percent of the effect ocurrs within the first year and 25 percent in the second year.",
        'rebate_checks', "Half of the effect ocurrs within the first two quarters and then smoothly over six quarters.",
        'rebate_checks_arp', "Roughly 35 percent of the effect ocurrs within three quarters and then smoothly over six quarters.",
        'other_direct_aid_arp', "We apply the same timing as to the ARP rebate checks.",
        'other_vulnerable_arp', "We apply the same timing as to the ARP unemployment insurance expansion.",
        'subsidies', "Effect occurs at a slowly decreasing rate over three years.",
        'aid_to_small_businesses_arp', "Effect occurs at a slowly decreasing rate over three years.",
        'non_corporate_taxes', "60 percent of the effect ocurrs in the first year and 40 percent in the second.",
        'corporate_taxes', "Effect occurs smoothly over three years.")

mpc_table <-
  mpc_data %>% 
  left_join(mpc_descriptions, by = 'variable') %>% 
  mutate(variable = snakecase::to_title_case(variable),
         variable = stringr::str_replace(variable, 'Arp', 'ARP'),
         variable = stringr::str_replace(variable, 'Ui', "Unemployment Insurance"),
         variable = str_replace(variable, "Unemployment Insurance ARP", "ARPA Unemployment Insurance"),
         variable = str_replace(variable, "Other Direct Aid ARP", "Other ARPA Direct Aid to Families"),
         variable = str_replace(variable, "Other Vulnerable ARP", "Other ARPA Aid to Financially Vulnerable Families"),
         variable = str_replace(variable, "Aid to Small Businesses ARP", "ARPA Subsidies"),
         variable = str_replace(variable,  "Rebate Checks ARP", "ARPA Rebate Checks"))
# Table -------------------------------------------------------------------


new_order <- c(
  "Social Benefits",
  "Health Outlays",
  "Unemployment Insurance",
  "ARPA Unemployment Insurance",
  "Rebate Checks",
  "ARPA Rebate Checks",
  "Other ARPA Direct Aid to Families",
  "Other ARPA Aid to Financially Vulnerable Families",
  "Subsidies",
  "ARPA Subsidies",
  "Non Corporate Taxes",
  "Corporate Taxes"
)
base_table <-
  mpc_table %>% 
  mutate(variable = as_factor(variable),
         variable = fct_relevel(variable, rev(new_order))) %>% 
  arrange(desc(variable)) %>% 
  relocate(description, .after = 'mpc') %>% 
  gt() %>% 
  my_theme() %>% 
  fmt_percent(mpc, decimals = 0) %>% 
  tab_header(title = md("Assumed MPCs")) %>% 
  cols_align(align = 'right',
             columns = 'mpc') %>% 
  cols_align(align = 'left',
             c(variable, description)) %>% 
  cols_label(variable = 'Component',
             description = '',
             mpc = 'Total MPC')

table <-
  base_table 
gtsave(table, 'analysis/mpc_table/mpc_table.png')
