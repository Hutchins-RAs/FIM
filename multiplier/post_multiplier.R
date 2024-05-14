#### STEP 0: Initialize workspace ----------------------------------------------
library(readxl)
library(magrittr)
library(zoo)
library(dplyr)
# We assume your current working directory is fim/. If not, please adjust 
# accordingly before proceeding with code. You can check by running:
# getwd()

#### STEP 1: Load in FIM data --------------------------------------------------
# We use 2024-04 FIM results here. Feel free to adjust according to your needs.
fim_raw <- read_xlsx("results/04-2024/fim-04-2024.xlsx",
                    range = "A1:HU260",
                    col_names = TRUE,
                    # The first column is a date column, the second column "id"
                    # has data with either "historic" or "forecast" and is text, 
                    # the rest are values. There are 229 columns total.
                    # If you try to read in date as a date, it produced NAs. So 
                    # these will be post-processed to be parsed as dates.
                    col_types = c("text", "text", rep("numeric", times = 227)))

data <- fim_raw[c("date", "id", "gdp", "real_gdp", "gdp_deflator", "fiscal_impact")] %>%
  mutate(date = as.yearqtr(date, format = "%Y Q%q"))

