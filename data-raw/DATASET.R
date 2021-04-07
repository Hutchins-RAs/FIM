## code to prepare `DATASET` dataset goes here
historical <- readxl::read_xlsx('inst/extdata/national_accounts.xlsx')
usethis::use_data(historical, overwrite = TRUE)
