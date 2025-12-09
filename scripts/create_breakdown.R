# create_breakdown.R
# 
# This script creates part of a helper Excel file to be published on the FIM 
# website. It contains extra information on the breakdown of the effects of the 
# OBBBA and tariffs on the various components of the FIM

# NOTE: Run this after running the fiscal_impact_BETA script and updating the
# differences workbook if necessary

# Change the values for last_date! These must be hard coded because dates that we 
# publish the FIM are out of schedule these days

# This does not edit things that are not in the contributions or differences sheet

# PART 1: Set variables ------------------

Sys.setenv(TZ = 'UTC') # Set the default time zone to UTC (Coordinated Universal Time)

# Load packages
packages <- c(
  "tidyverse", "tsibble", "lubridate", "glue", "readxl",
  "TimTeaFan/dplyover", "zoo", "TTR", "fs", "gt", 
  "openxlsx", "snakecase", "rlang", "BrookingsInstitution/ggbrookings"
)
librarian::shelf(packages)

# Load all functions in package
devtools::load_all() 

options(digits = 2) # Limit number of digits
options(scipen = 20)# Turn off scientific notation under 20 digits 

# Calculate the current date minus 7 days
current_date <- today() 

# Hard code value of last FIM update
# Change each time!
last_date <- as.Date("2025-12-05")

month_year <- glue('{format.Date(today() - 4, "%m")}-{year(today())}')
print(month_year)

# Create folder for current update in the results directory
dir_create(glue('breakdown/{current_date}')) 

# PART 2: Load existing files ------------------

# Create copy of previous breakdown workbook with formulas 
file.copy(from = glue("breakdown/{last_date}/Fiscal Impact Breakdown_{last_date}.xlsx"),
          to = glue("breakdown/{current_date}/Fiscal Impact Breakdown_{current_date}.xlsx"),
          overwrite = TRUE)

# Load current breakdown workbook 
wb <- loadWorkbook(glue("breakdown/{current_date}/Fiscal Impact Breakdown_{current_date}.xlsx"))
current_data <- readWorkbook(wb, sheet = "Table 1")

# Load effects of shutdown, tariffs, and obbba from differences workbook sheet 
shutdown_diff <- read_excel(glue("breakdown/{current_date}/differences_{current_date}.xlsx"),
                            sheet = "Shutdown Effects") %>%
  filter(date >= "2025 Q2") %>%
  select(shutdown_effect)

tariff_diff <- read_excel(glue("breakdown/{current_date}/differences_{current_date}.xlsx"),
                          sheet = "Tariff Effects") %>%
  filter(date >= "2025 Q2") %>%
  select(tariff_effect)
  
obbba_diff <- read_excel(glue("breakdown/{current_date}/differences_{current_date}.xlsx"),
                      sheet = "OBBBA Effects")
  # Components
  federal_purchases_obbba <- obbba_diff %>%
    filter(date >= "2025 Q2") %>%
    select(federal_purchases_effect)
  
  taxes_obbba <- obbba_diff %>%
    filter(date >= "2025 Q2") %>%
    select(taxes_effect)
  
  transfers_obbba <- obbba_diff %>%
    filter(date >= "2025 Q2") %>%
    select(transfers_effect)

# Load existing contributions workbook 
contributions <- read_excel(glue("results/{month_year}/beta/contributions-{month_year}.xlsx")) %>%
  filter(date >= "2025 Q2") %>%
  select(federal_purchases_contribution, state_purchases_contribution, 
         taxes_contribution, federal_student_loans_contribution,
         supply_side_ira_contribution, transfers_contribution) %>%
  mutate(federal_purchases_component = federal_purchases_contribution,
         state_purchases_component = state_purchases_contribution,
         taxes_component = taxes_contribution - federal_student_loans_contribution - supply_side_ira_contribution,
         student_loans_component = federal_student_loans_contribution,
         supply_side_ira_component = supply_side_ira_contribution,
         transfers_component = transfers_contribution,
         federal_student_loans_component = federal_student_loans_contribution,
         supply_side_ira_component = supply_side_ira_contribution)


# PART 3: Extract data and write into workbook ------------------

# Define columns to write
cols_to_write <- list(
  list(col = 5, data = contributions$federal_purchases_component),
  list(col = 6, data = contributions$state_purchases_component),
  list(col = 7, data = federal_purchases_obbba$federal_purchases_effect),
  list(col = 13, data = contributions$taxes_component),
  list(col = 14, data = taxes_obbba$taxes_effect),
  list(col = 15, data = tariff_diff$tariff_effect),
  list(col = 18, data = contributions$transfers_component),
  list(col = 19, data = transfers_obbba$transfers_effect),
  list(col = 26, data = contributions$federal_student_loans_component),
  list(col = 27, data = contributions$supply_side_ira_component)
)

# Write all at once
for (col_info in cols_to_write) {
  writeData(wb, 
            sheet = "Table 1", 
            x = as.numeric(col_info$data),
            startCol = col_info$col,
            startRow = 5,
            colNames = FALSE)
}

saveWorkbook(wb, glue("breakdown/{current_date}/Fiscal Impact Breakdown_{current_date}.xlsx"), 
             overwrite = TRUE)





