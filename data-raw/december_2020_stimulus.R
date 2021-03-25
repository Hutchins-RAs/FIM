## code to prepare `december_2020_stimulus` dataset goes here
december_2020_stimulus <- readxl::read_xlsx('inst/extdata/december_2020_stimulus.xlsx') %>%mutate(date = tsibble::yearquarter(date)) %>%  as_tsibble(index = date)

usethis::use_data(december_2020_stimulus, overwrite = TRUE)
