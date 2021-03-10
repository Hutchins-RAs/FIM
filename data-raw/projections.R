## code to prepare `projections` dataset goes here

economic_projections <- readxl::read_xlsx('inst/extdata/projections.xlsx', sheet = 'economic') %>% 
  mutate(date = yearquarter(date)) %>%
  as_tsibble(index = date)

budget_projections <- readxl::read_xlsx('inst/extdata/projections.xlsx', sheet = 'budget') %>% 
  as_tsibble(index = fy) %>%
  annual_to_quarter() %>%
  fiscal_to_calendar()

projections <- 
  budget_projections %>%
  dplyr::left_join(economic_projections) %>%
  mutate(id = 'projection') 
  

usethis::use_data(projections, overwrite = TRUE)
