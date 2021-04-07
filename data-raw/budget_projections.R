
## code to prepare `budget_projections` dataset goes here
budget_projections <-readxl::read_xlsx('inst/extdata/budget_projections.xlsx')

saveRDS(budget_projections, 'data/budget_projections.rds')
usethis::use_data(budget_projections, overwrite = TRUE)
