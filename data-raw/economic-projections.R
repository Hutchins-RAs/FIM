if (!require("pacman")) install.packages("pacman") 
pacman::p_load(readxl, magrittr, dplyr, usethis, here, data.table)

taxpieces = c('gsrpt' ,'gsrpri', 'gsrcp' ,'gsrs')

economic_projections <-
  readxl::read_xlsx(here::here("inst",'extdata', 'economic_projections.xlsx')) 


usethis::use_data(economic_projections, overwrite = TRUE)
saveRDS(economic_projections, 'data/economic_projections.rds')
