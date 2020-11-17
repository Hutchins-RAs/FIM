## code to prepare `haver` dataset goes here
# load required packages ----
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(readxl, magrittr, dplyr, usethis, here)


# clean data ----
national_accounts <- read_xlsx(here::here("inst",'extdata', "national_accounts.xlsx"))
economic_statistics <- read_xlsx(here::here('inst','extdata', 'economic_statistics.xlsx'))

haver <- left_join(national_accounts,
                   economic_statistics,
                   by = "date") %>%
  mutate(date = as.Date(date))
# Save data
usethis::use_data(haver, overwrite = TRUE)
