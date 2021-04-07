## code to prepare `cares` dataset goes here
cares <- readxl::read_xlsx('inst/extdata/projections.xlsx', sheet = 'CARES') 
usethis::use_data(cares, overwrite = TRUE)
