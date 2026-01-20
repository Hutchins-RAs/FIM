# differences.R
#
# This script finds the differences between two scripts that have similar 
# structure and outputs a file with three sheets: the two original sheets and
# a third with the differences. 
#
# It also accounts for files that have varying date lengths and filters for 
# relevant time periods. 
#
# To use this, update the file paths in line 33 + 34.

# PART 1: Load environment --------------------------------------------------   

Sys.setenv(TZ = 'UTC') # Set the default time zone to UTC (Coordinated Universal Time)

# Load packages
packages <- c(
  "tidyverse", "tsibble", "lubridate", "glue", 
  "TimTeaFan/dplyover", "zoo", "TTR", "fs", "gt", 
  "openxlsx", "snakecase", "rlang", "BrookingsInstitution/ggbrookings"
)
librarian::shelf(packages)

# Load all functions in package
devtools::load_all() 

options(digits = 4) # Limit number of digits
options(scipen = 20)# Turn off scientific notation under 20 digits 


# PART 2: Load files --------------------------------------------------------   

file1 <- read.xlsx(glue("results\\12-2025\\12.5\\published 12.5\\beta\\contributions-12-2025.xlsx"))
file2 <- read.xlsx(glue("results\\11-2025\\beta\\contributions-11-2025.xlsx"))


# PART 3: Find differences --------------------------------------------------   

# Filter for dates 
file1_clean <- file1 %>%
  mutate(date = yearquarter(date)) %>%
  filter(date >= yearquarter("2023 Q1") & date <= current_quarter + 8)

file2_clean <- file2 %>%
  mutate(date = yearquarter(date)) %>%
  filter(date >= yearquarter("2023 Q1") & date <= current_quarter + 8)

# Calculate differences (subtract only numeric columns, keep date)
numeric_cols <- file1_clean %>% 
  select(where(is.numeric)) %>% 
  names()

differences <- file1_clean %>%
  select(date) %>%
  bind_cols(
    file1_clean[numeric_cols] - file2_clean[numeric_cols]
  )


# PART 4: Export to Excel with three sheets ---------------------------------   

# Create workbook
wb <- createWorkbook()

# Add sheets
addWorksheet(wb, "File 1")
addWorksheet(wb, "File 2")
addWorksheet(wb, "Differences")

# Write data to sheets (convert yearquarter back to character for Excel)
writeData(wb, "File 1", file1_clean %>% mutate(date = as.character(date)))
writeData(wb, "File 2", file2_clean %>% mutate(date = as.character(date)))
writeData(wb, "Differences", differences %>% mutate(date = as.character(date)))

# Save workbook
saveWorkbook(wb, "results\\differences_output.xlsx", overwrite = TRUE)
