## code to prepare `cbo-projections` dataset goes here

librarian::shelf(tidyverse, tidyxl, tsibble, rvest, lubridate)
devtools::load_all()


# Find urls -------------------------------------------------------------------------------------------------------

library(rvest)

html <- read_html('https://www.cbo.gov/data/budget-economic-data')

urls <- 
  html |> 
  html_elements("a") %>% 
  html_attr("href") |> 
  as_tibble() |> 
  rename(url = value) |> 
  filter(str_detect(url, 'economicprojections|budgetprojections')) |> 
  mutate(date = str_extract(url, '20[0-9]{2}-[0-9]{2}') |> paste0('-01') |> as_date(),
         type = str_extract(url, 'economic|budget')) 

latest_urls <-
  urls |> 
  group_by(type) |> 
  filter(date == max(date)) |> 
  pull(url)

econ_url <- str_subset(latest_urls, 'economic')
budg_url <- str_subset(latest_urls, 'budget')


## Economic projections


# Download latest data release to a temporary file so we can read it in R
temp_file <- paste0(tempfile(), '.xlsx')
download.file(econ_url, destfile = temp_file, mode = 'wb')
# Use tidyxl to make human readable spreadsheet machine readable
cbo_econ <- 
  xlsx_cells(temp_file) |> 
  filter(sheet == '1. Quarterly',
         row %in% seq(7, 158))

# Isolate the date variable to merge with data later
dates <-
  cbo_econ |> 
  filter(str_starts(character, '[0-9]{4}Q[0-9]{1}')) |> 
  select(col, date = character)
# Variable label is stored across three columns 
# The labels for the level of detail/aggregation are nested and below the header row
# To tidy this I separate them into three columns with the "level" of nesting
# Then I fill in the blank spaces below the headers so each variable is matched with all its hierarchy levels
variable_labels <-
  cbo_econ |> 
  filter(col %in% seq(1, 3)) |> 
  select(row, col, character) |> 
  pivot_wider(names_from = col,
              values_from = character) |> 
  rename(lvl1 = `1`,
         lvl2 = `2`,
         lvl3 = `3`) |> 
  fill(lvl1) |> 
  filter(!if_all(c(lvl2, lvl3), is.na)) |> 
  fill(lvl2) 

# From these labels I manually created a dictionary to match each variable used in the FIM with a Haver code
# It was straigthforward to create by using datapasta::tribble_paste(variable_labels) in the console
dictionary <-
  tibble::tribble(
    ~haver_code, ~lvl1, ~lvl2, ~lvl3,
    "gdp", "Output", "Gross Domestic Product (GDP)", NA,
    "gdph", "Output", "Real GDP", NA,
    "gdppotq", "Potential GDP and Its Components", "Potential GDP", NA,
    "gdppothq", "Potential GDP and Its Components", "Real Potential GDP", NA,
    "dc", "Prices", "Price Index, Personal Consumption Expenditures (PCE)", NA,
    "cpiu", "Prices", "Consumer Price Index, All Urban Consumers (CPI-U)", NA,
    "jgdp", "Prices", "GDP Price Index", NA,
    "unemployment_rate", "Labor", "Unemployment Rate, Civilian, 16 Years or Older", NA,
    "c", "Components of GDP (Nominal)", "Personal Consumption Expenditures", NA,
    "g", "Components of GDP (Nominal)", "Government Consumption Expenditures and Gross Investment", NA,
    "gf", "Components of GDP (Nominal)", "Government Consumption Expenditures and Gross Investment", "Federal",
    "gs", "Components of GDP (Nominal)", "Government Consumption Expenditures and Gross Investment", "State and local ",
    "ch", "Components of GDP (Real)", "Personal Consumption Expenditures", NA,
    "gh", "Components of GDP (Real)", "Government Consumption Expenditures and Gross Investment", NA,
    "gfh", "Components of GDP (Real)", "Government Consumption Expenditures and Gross Investment", "Federal",
    "gsh", "Components of GDP (Real)", "Government Consumption Expenditures and Gross Investment", "State and local "
  )

# Now we're ready to pull the data and match it with the tidy labels we created
cbo_econ_clean <-
  cbo_econ |> 
  # Only get numeric data
  select(row, col, numeric) |> 
  drop_na(numeric) |> 
  # Join data with its row label
  left_join(variable_labels, by = 'row') |>
  # Join data with date by column
  left_join(dates, by = 'col') |> 
  # Right join to filter out values without a Haver Code present in the dictionary 
  right_join(dictionary, by = c('lvl1', 'lvl2', 'lvl3')) |> 
  # Transform data to wide format
  select(date, haver_code, value = numeric) |> 
  pivot_wider(names_from = haver_code,
              values_from = value) |> 
  mutate(date = str_replace(date, 'Q', ' Q'),
         date = tsibble::yearquarter(date))


# Budget ----------------------------------------------------------------------------------------------------------


# Download latest data to a temporary file so we can read it into R
temp_file <- paste0(tempfile(), '.xlsx')
download.file(budg_url, destfile = temp_file, mode = 'wb')
# Use tidyxl to make human readable spreadsheet machine readable
budg_exp <- tidyxl::xlsx_cells(temp_file) |> 
  filter(sheet == 'Table 1-3, adjusted',
         row >= 10 & row <= 46,
         col <= 13) 

dates <- filter(budg_exp, row == 10) |> select(col, date = numeric) |> drop_na()

header_positions <-
  budg_exp |> 
  filter(character %in% c('Social Security', 'Major Health Care Programs', 'Income Security Programs', 'Federal Civilian and Military Retirement', "Veterans' Programs")) |> 
  drop_na(character) |> 
  pull(row)
budg_exp_clean <-
  budg_exp |> 
    select(row, col, character, numeric) |> 
    filter(row > 10) |> 
    mutate(value = coalesce(character, as.character(numeric))) |> 
    select(row, col, value) |> 
    left_join(dates, by = 'col') |> 
    select(-col) |> 
    pivot_wider(names_from = date,
                values_from = value) |> 
    rename(subcategory = `NA`) |> 
    drop_na(subcategory) |> 
    mutate(category = if_else(row %in% header_positions, 
                          subcategory,
                          NA_character_),
           .before = everything()) |> 
    fill(category) |> 
    drop_na() |> 
    mutate(subcategory = str_replace(subcategory, 'Medicarea', 'Medicare'),
           subcategory = str_replace(subcategory, 'Subtotala', 'Subtotal')) |> 
    pivot_longer(-c(category, row, subcategory),
                 names_to = 'date') |> 
  mutate(value = as.numeric(value))


budg_exp_clean <-
  budg_exp_clean |> 
  filter(subcategory %in% c('Subtotal', 'Medicare', 'Medicaid', 'Unemployment compensation'),
         category != 'Federal Civilian and Military Retirement') |> 
  group_by(subcategory, date) |> 
  summarise(value = sum(value), .groups = 'drop') |> 
  pivot_wider(names_from = subcategory,
              values_from = value) |> 
  mutate(social_benefits = Subtotal - Medicaid,
         date = as.numeric(date)) |> 
  select(date,
         yptmd = Medicaid,
         yptmr = Medicare,
         yptu = `Unemployment compensation`,
         gftfp = social_benefits)

budg_rev <-
  xlsx_cells(temp_file) |> 
  filter(sheet == 'Table 1-1',
         row < 29,
         col <= 13) 

dates <- 
  budg_rev |> 
  filter(row == 9) |> 
  select(col, date = numeric) |> 
  drop_na(date)

tax_positions <- 
  budg_rev |> 
  filter(character %in% c('Individual income taxes', 'Payroll taxes', 'Corporate income taxes', 'Other')) |> 
  pull(row)


budg_rev_clean <-
  budg_rev |> 
  select(row, col, numeric, character) |> 
  filter(row %in% tax_positions) |> 
  fill(character) |> 
  drop_na(numeric) |> 
  left_join(dates, by = 'col') |> 
  select(date, character, value = numeric) |> 
  pivot_wider(names_from = 'character',
              values_from = 'value') |> 
  rename(gfrpt = `Individual income taxes`,
         gfrs = `Payroll taxes`,
         gfrcp = `Corporate income taxes`,
         gfrpri = `Other`)

cbo_budg <-
  left_join(budg_exp_clean, budg_rev_clean, by = 'date') |> 
  tsibble::as_tsibble(index = date) %>%
  annual_to_quarter() %>%
  fiscal_to_calendar()

cbo_projections <-
  left_join(cbo_budg, cbo_econ_clean, by = 'date')|> 
  cola_adjustment() %>%
  smooth_budget_series() %>%
  implicit_price_deflators() %>%
  growth_rates() %>%
  alternative_tax_scenario() %>%
  mutate(id = 'projection') |> 
  format_tsibble() %>% 
  select(id, date, gdp, gdph, gdppothq, gdppotq, starts_with('j'), dc, c, ch ,ends_with('growth'), cpiu, unemployment_rate)


usethis::use_data(cbo_projections, overwrite = TRUE)
devtools::load_all()
writexl::write_xlsx(fim::cbo_projections, 'data/cbo-projections.xlsx')
