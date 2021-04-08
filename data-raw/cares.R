# ## code to prepare `cares` dataset goes here
# cares <- readxl::read_xlsx('inst/extdata/projections.xlsx', sheet = 'CARES') 
# usethis::use_data(cares, overwrite = TRUE)


projection_start <- which(fim['id'] == 'projection') %>% min()
projection_end <- which(fim[['date']] == yearquarter('2022 Q4'))
fim %>% 
  slice(projection_start:projection_end) %>% 
  rowid_to_column() %>% 
  rows_update(tibble(rowid = 1:4, nonprofit_ppp = c(378 / 1e3, rep(0, 3)),
                     nonprofit_provider_relief_fund = c(rep(43, 3), 15),
                     education_stabilization_fund = c(18, rep(0, 3)),
                     provider_relief_fund = c(rep(12, 3), 6)),
              by = 'rowid') %>% 
  full_join(fim %>% slice(-(projection_start:projection_end))) %>% 
  pull(provider_relief_fund)
