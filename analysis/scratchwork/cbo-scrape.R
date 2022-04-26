librarian::shelf(tidyverse, tidyxl)

url <- 'https://www.cbo.gov/system/files/2021-07/51118-2021-07-budgetprojections.xlsx'
temp_file <- paste0(tempfile(), '.xlsx')
download.file(url, destfile = temp_file, mode = 'wb')
cbo_budg <- tidyxl::xlsx_cells(temp_file) |> 
  filter(sheet == 'Table 1-3, adjusted',
         row >= 10 & row <= 93,
         col <= 13) 

headers <- 
  budget_projections |> 
  filter(row == 10) |> 
  select(year = numeric) |> 
  drop_na()

budget_projections |> 
  filter(str_detect(character, 'Subtotal',negate = TRUE)) |> 
  filter(character != '____')

numeric <-
  budget_projections |> 
  filter(row > 10,
         col > 1) |> 
  drop_na(numeric) |> 
  select(col, row, numeric)

character_pos <- distinct(numeric, row) |> pull(row)

budget_projections |> 
  filter(row %in% character_pos,
         data_type == 'character')

## Economic projections

url <- 'https://www.cbo.gov/system/files/2021-07/51135-2021-07-economicprojections.xlsx'
temp_file <- paste0(tempfile(), '.xlsx')
download.file(url, destfile = temp_file, mode = 'wb')
cbo_econ <- 
  xlsx_cells(temp_file) |> 
  filter(sheet == '1. Quarterly',
         row %in% seq(7, 158))
dates <-
  cbo_econ |> 
  filter(str_starts(character, '[0-9]{4}')) |> 
  select(col, date = character)

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

cbo_econ |> 
  select(row, col, numeric) |> 
  drop_na(numeric) |> 
  left_join(variable_labels, by = 'row') |> 
  left_join(dates, by = 'col') |> 
  # Right join to filter out values without a Haver Code present in the dictionary 
  right_join(dictionary, by = c('lvl1', 'lvl2', 'lvl3')) |> 
  select(date, haver_code, value = numeric) |> 
  pivot_wider(names_from = haver_code,
              values_from = value) |> 
  mutate(date = str_replace(date, 'Q', ' Q'),
         date = tsibble::yearquarter(date))
