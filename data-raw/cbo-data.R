## code to prepare `DATASET` dataset goes here
# load required packages ----
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(readxl, magrittr, dplyr, usethis, here, data.table)

# clean data ----
econ <-
  read_xlsx(here('data-raw', 'cbo_econ_proj_quarterly.xlsx')) %>%
  mutate(date =  gsub("12/30/", "12/31/", date)) %>%
  mutate(date = as.Date(date)) 

econ_a <-
  read_xlsx(here('data-raw', 'cbo_econ_proj_annual.xlsx')) %>%
  rename(date = calendar_date)
# Budget ----------------------------------------------------------------------------------------------

budg <-
  read_xlsx(here('data-raw', 'cbo_budget_nipas_proj_annual.xlsx'))

expdate = "2025-12-30"
predate = "2025-09-30"

budg %<>% 
  bind_rows(budg, budg, budg) %>% 
  arrange(fy) %>%
  mutate(date = econ$date) %>%
  mutate(date = lag(date))

# Federal Medical Assistance Percentage (FMAP) --------------------------------------------------------------------
fmap <-
  read_xlsx(here('data-raw', 'nhe_fmap.xlsx'))

# Save data
usethis::use_data(econ, overwrite = TRUE)
usethis::use_data(econ_a, overwrite = TRUE)
usethis::use_data(budg, overwrite = TRUE)
usethis::use_data(fmap, overwrite = TRUE)