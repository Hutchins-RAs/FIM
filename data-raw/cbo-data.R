## code to prepare `DATASET` dataset goes here
# load required packages ----
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(readxl, magrittr, dplyr, usethis, here, data.table)

# clean data ----
econ <-
  read_xlsx(here::here("inst",'extdata', 'cbo_econ_proj_quarterly.xlsx')) %>%
  mutate(date =  gsub("12/30/", "12/31/", date)) %>%
  mutate(date = as.Date(date)) 

econ_a <-
  read_xlsx(here::here("inst",'extdata','cbo_econ_proj_annual.xlsx')) %>%
  rename(date = calendar_date)

# Federal Medical Assistance Percentage (FMAP) --------------------------------------------------------------------
fmap <-
  read_xlsx(here::here("inst",'extdata', 'nhe_fmap.xlsx'))

# Save data
usethis::use_data(econ, overwrite = TRUE)
usethis::use_data(econ_a, overwrite = TRUE)
usethis::use_data(fmap, overwrite = TRUE)