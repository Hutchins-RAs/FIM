
# Setup -------------------------------------------------------------------
librarian::shelf(tidyverse, gt)
my_theme <- function(data, ...){
  data %>%
    opt_row_striping() %>%
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

order <- c('ui', 'rebate_checks', 'subsidies', 'ui_arp', 'other_vulnerable_arp',  'rebate_checks_arp','aid_to_small_businesses_arp', 'other_direct_aid_arp', 'social_benefits', 'health_outlays', 'non_corporate_taxes', 'corporate_taxes')

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
  tribble(~variable, ~group, ~description,
  'ui', 'Legislation in 2020 (Cares Act, Families First,  December 2020 legislation and other)',"70% of the effect occurs within the first two quarters and the remainder is spread out over four quarters at a decreasing rate.",
  'rebate_checks','Legislation in 2020 (Cares Act, Families First,  December 2020 legislation and other)', "Half of the effect occurs within the first two quarters and then smoothly over six quarters.",
  'subsidies','Legislation in 2020 (Cares Act, Families First,  December 2020 legislation and other)',"Effect occurs at a slowly decreasing rate over three years.",
  'ui_arp', 'American Rescue Plan (ARP)', "Roughly 35% of the effect occurs within two quarters and the remainder is spread out over six quarters at a decreasing rate.",
  'other_vulnerable_arp','American Rescue Plan (ARP)', "We apply the same timing as to the ARP unemployment insurance expansion.",
  'rebate_checks_arp','American Rescue Plan (ARP)', "Roughly 35% of the effect occurs within three quarters and then smoothly over six quarters.",
  'aid_to_small_businesses_arp','American Rescue Plan (ARP)', "Effect occurs at a slowly decreasing rate over three years.",
  'other_direct_aid_arp','American Rescue Plan (ARP)', "We apply the same timing as to the ARP rebate checks.",
  'social_benefits', 'Prior to 2020', "Effect occurs smoothly in the first year.",
  'health_outlays', 'Prior to 2020', "Effect occurs smoothly in the first year.",
  'non_corporate_taxes', 'Prior to 2020', "60% of the effect occurs in the first year and 40% in the second.",
  'corporate_taxes', 'Prior to 2020', "Effect occurs smoothly over three years."
)


mpc_table <-
  mpc_data %>% 
  left_join(mpc_descriptions, by = 'variable') %>% 
  mutate(variable = snakecase::to_title_case(variable),
         variable = stringr::str_replace(variable, 'Arp', 'ARP'),
         variable = stringr::str_replace(variable, 'Ui', "Unemployment Insurance"),
         variable = str_replace(variable, "Unemployment Insurance ARP", "Unemployment Insurance and Other Aid to Financially Vulnerable Families (e.g., SNAP, housing assistance)"),
         variable = str_replace(variable, "Other Direct Aid ARP", "Other Direct Aid to Families (e.g., Child Tax Credit, Earned Income Tax Credit)"),
         variable = str_replace(variable, 
                                'Subsidies', 
                                'Subsidies (mostly PPP)'),
         variable = str_replace(variable, "Other Vulnerable ARP", "Other Aid to Financially Vulnerable Families (e.g., SNAP, housing assistance)"),
         variable = str_replace(variable, "Aid to Small Businesses ARP", "Subsidies (mostly PPP)"),
         variable = str_replace(variable,  "Rebate Checks ARP", "Rebate Checks and Other Direct Aid to Families (e.g., Child Tax Credit, Earned Income Tax Credit)"),
         variable = str_replace(variable,  "Non Corporate Taxes", "Non-Corporate Taxes")) %>% 
  relocate('group', .before = 'variable')
# Table -------------------------------------------------------------------


  mpc_table %>% 
  filter(variable != 'Health Outlays',
         variable != 'Other Aid to Financially Vulnerable Families (e.g., SNAP, housing assistance)',
         variable != 'Other Direct Aid to Families (e.g., Child Tax Credit, Earned Income Tax Credit)') %>% 
  mutate(variable = str_replace(variable,
                                'Social Benefits',
                                'Social Benefits and Health Outlays')) %>% 
  gt(groupname_col = 'group',
     rowname_col = 'variable') %>% 
  my_theme() %>% 
  fmt_percent(mpc, decimals = 0) %>% 
  tab_header(title = md("Assumed MPCs")) %>% 
  cols_align(align = 'center',
             columns = 'mpc') %>% 
  cols_align(align = 'left',
             c(variable, description)) %>% 
  cols_label(variable = 'Component',
             description = '',
             mpc = 'Total MPC') %>% 
  cols_width(mpc ~ px(100)) -> table

gtsave(table, 'analysis/mpc_table/mpc_table.png')
