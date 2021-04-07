## code to prepare `historical` dataset goes here

national_accounts <- readxl::read_xlsx('inst/extdata/national_accounts.xlsx') %>% 
  mutate(id = 'historical') %>%
  millions_to_billions() %>%
  rename(cpiu = ui,
         
         ) %>%
  format_tsibble()

usethis::use_data(national_accounts, overwrite = TRUE)
