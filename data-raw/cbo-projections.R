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

louise_cbo_override <-
  tibble::tribble(
    ~date,               ~c,              ~gf,              ~gs,
    "2022 Q1", 16476.1441705274, 1600.01632465715, 2645.04687330805,
    "2022 Q2", 16845.5519614215, 1618.37966290859, 2711.74941835993,
    "2022 Q3",  17199.622372125,  1629.4505308442, 2762.62896922833,
    "2022 Q4", 17496.4233310878, 1637.48049636548, 2807.40603938854,
    "2023 Q1", 17768.0135158782,  1649.5628807974, 2849.41805289463,
    "2023 Q2", 18006.7094790488, 1661.45755179164, 2891.26267722546,
    "2023 Q3", 18224.8800504615, 1672.78247257417, 2928.42038805108,
    "2023 Q4",  18435.268028558, 1685.44102850976, 2966.47867338683,
    "2024 Q1", 18635.7773171023, 1697.94206559536, 3002.58940392492,
    "2024 Q2", 18817.7573845816, 1708.71098242201, 3037.65879760855,
    "2024 Q3", 18889.1686480368, 1708.83840307693, 3043.86399135188,
    "2024 Q4",  18948.000347289, 1708.71098242201, 3049.78057143273,
    "2025 Q1", 19018.1409476934, 1709.73034766134, 3055.69715151359,
    "2025 Q2", 19094.7619296569, 1710.87713355558, 3061.75803842568,
    "2025 Q3", 19168.8415855188, 1711.89649879491, 3067.81892533777,
    "2025 Q4", 19244.7001696518, 1712.91586403424, 3073.87981224987,
    "2026 Q1", 19303.9130678193, 1713.68038796373, 3079.79639233072,
    "2026 Q2", 19391.2076194096, 1714.44491189323, 3085.71297241157,
    "2026 Q3", 19481.5517623218, 1715.33685647764, 3091.48524566118,
    "2026 Q4", 19568.2109823866, 1716.48364237188, 3097.11321207956,
    "2027 Q1", 19650.4228817737, 1717.63042826612, 3102.74117849793,
    "2027 Q2", 19744.8331464484, 1718.77721416037, 3108.22483808506,
    "2027 Q3", 19837.5915491572, 1719.79657939969, 3113.56419084095,
    "2027 Q4", 19929.8416866455, 1721.07078594885, 3118.90354359684,
    "2028 Q1", 20026.2850122016, 1722.59983380784, 3124.09858952149,
    "2028 Q2", 20118.4080833849, 1724.25630232175, 3129.43794227739,
    "2028 Q3", 20208.1168947717, 1725.53050887091, 3134.63298820204,
    "2028 Q4", 20303.1624909719, 1726.80471542007, 3139.97234095793,
    "2029 Q1", 20394.6502306298,  1727.8240806594, 3145.16738688258,
    "2029 Q2", 20488.4251637791, 1728.97086655364, 3150.36243280723,
    "2029 Q3", 20578.8963729964, 1729.86281113805, 3155.55747873188,
    "2029 Q4", 20669.2405159086, 1730.75475572246, 3160.75252465653,
    "2030 Q1", 20759.7117251259, 1731.64670030687, 3165.94757058118,
    "2030 Q2", 20850.5641332584,  1732.6660655462, 3171.14261650584,
    "2030 Q3", 20955.6479675599, 1733.68543078553, 3176.33766243049,
    "2030 Q4", 21059.5882051157, 1734.57737536994, 3181.53270835514,
    "2031 Q1", 21163.5284426715, 1735.46931995435, 3186.72775427979,
    "2031 Q2", 21270.5182715492, 1736.36126453876, 3191.92280020444,
    "2031 Q3", 21376.4915699863, 1737.25320912317, 3197.11784612909,
    "2031 Q4", 21483.3543325589, 1738.39999501741,  3202.1685852225
  ) |> 
  mutate(date = yearquarter(date))

# Override CBO projected deflators as per Louise's judgement. 
cbo_econ_clean <-
  cbo_econ_clean |> 
  mutate_where(date >= yearquarter('2022 Q1') & date <= yearquarter('2031 Q4'),
               c = NA_real_,
               gf = NA_real_,
               gs = NA_real_) |> 
  coalesce_join(louise_cbo_override, by = 'date') 



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
