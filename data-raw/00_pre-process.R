# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
library('tidyverse')
library('Haver')
library('readxl')
library('writexl')
library('tsibble')

source('src/functions.R')
monthly_to_quarterly <- function(df){
  df %>%
    mutate(yq = tsibble::yearquarter(date)) %>%
    as_tsibble(index = date) %>%
    select(date, yq, everything()) %>%
    index_by(yq) %>%
    mutate(
      across(
        .cols = where(is.numeric), 
        .fns = ~ mean(.x, na.rm = TRUE)
      )
    ) %>%
    filter(row_number()== n()) %>%
    ungroup() %>%
    select(-yq)
}

# 0.1 Pull Raw Data---------------------------------------------------------------

START <- "01-01-1970"

# Quarterly -------------------------------------------------------------------------------------------------------
haver.path("//ESDATA01/DLX/DATA/")
# BEA NIPAs 
names_usna <- read_excel("data/auxilliary/haver_names.xlsx")





# Economic Statistics

data2 <-
  pull_data(c("PCW", "GDPPOTHQ", "GDPPOTQ", "RECESSQ",
              'LASGOVA', 'LALGOVA', 'CPGS'), 
            "usecon",
            start.date = START)

cpi <- 
  pull_data(c('UI'), 'cpidata', start.date = START) %>%
  monthly_to_quarterly()



wla <- pull_data('YPTOLM',
                 'usna',
                 frequency = 'monthly',
                 start.date = START) %>%
  monthly_to_quarterly() %>%
  mutate(yptolm = na_if(yptolm, 'NaN'))
data1 <-
  pull_data(names_usna$code,
            "usna",
            start.date = START) %>%
  as_tibble() %>%
  left_join(wla) %>%
  left_join(cpi) %>%
  left_join(data2)



  


monthly_state_ui <- c('LICL', 'LWCL', 'LUFP','LULP','LUWC','LUWP','LUBP','LUWB','LUEX','LUD','LUWBY', 'LUBPT', 'LUFPT', 'LULPT')

state_ui <- pull_data(monthly_state_ui,
                         'usecon',
                         start.date = START) %>%
  as_tibble() %>%
  write_xlsx('data/supplementary/monthly_state_ui.xlsx')
# Write csv to current month's folder
haver_raw_list <- 
  list(national_accounts = data1,
       economic_statistics = data2)

# Write haver data to raw folder ------------------------------------------
saveRDS(data1, 'data/raw/historical.rds')

## Exporting csv with the desired file names and into the right path
output_xlsx <- function(data, names){ 
  folder_path <- "data/raw/haver/"
  write_xlsx(data, paste0(folder_path, names, ".xlsx"))
}


list(data = haver_raw_list,
     names = names(haver_raw_list)) %>% 
  purrr::pmap(output_xlsx) 
