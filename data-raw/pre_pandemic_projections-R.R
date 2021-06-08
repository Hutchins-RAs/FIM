## code to prepare `pre_pandemic_projections.R` dataset goes here

pre_pandemic_baseline <- 
  readxl::read_xlsx('inst/extdata/projections.xlsx', 
                  sheet = 'pre-pandemic economic') %>% 
  dplyr::mutate(date = lubridate::as_date(date),
                date = tsibble::yearquarter(date)) %>% 
  mutate(across(-date,
                .fns = ~ (((.x / 100 + 1) ^ 0.25))) - 1)

usethis::use_data(pre_pandemic_baseline, overwrite = TRUE)
