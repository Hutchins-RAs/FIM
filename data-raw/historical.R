## code to prepare `historical` dataset goes here

national_accounts <- readxl::read_xlsx('inst/extdata/national_accounts.xlsx') %>% 
  mutate(date = lubridate::as_date(date))
usethis::use_data(national_accounts, overwrite = TRUE)
