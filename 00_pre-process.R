# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
Sys.setenv(TZ = 'UTC')
librarian::shelf(Haver, dplyr, tidyr, readxl, writexl, tsibble, purrr)
library('dplyr')
library('tidyr')
library('Haver')
library('readxl')
library('writexl')
library('tsibble')
library('purrr')
haver.path("//ESDATA01/DLX/DATA/")
devtools::load_all()

# 0.1 Pull Raw Data---------------------------------------------------------------

START <- "01-01-1970"

# Quarterly -------------------------------------------------------------------------------------------------------

# BEA NIPAs 
names_usna <- read_excel("data/haver_names.xlsx")





# Economic Statistics

usecon <- 
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

usna <-
  pull_data(names_usna$code,
            "usna",
            start.date = START) %>%
  as_tibble() %>% 
 
  # left_join(wla) %>%
  left_join(cpi) %>%
  left_join(usecon) %>% 
  # Convert SNAP from millions to billions
  mutate(gftffx = gftffx / 1e3)




monthly_state_ui <- c('LICL', 'LWCL', 'LUFP','LULP','LUWC','LUWP','LUBP','LUWB','LUEX','LUD','LUWBY', 'LUBPT', 'LUFPT', 'LULPT')

state_ui <- pull_data(monthly_state_ui,
                         'usecon',
                         start.date = START) %>%
  as_tibble() %>%
  write_xlsx('data/monthly_state_ui.xlsx')
# Write csv to current month's folder
haver_raw_list <- 
  list(national_accounts = usna,
       economic_statistics = usecon)


## Exporting csv with the desired file names and into the right path
output_xlsx <- function(data, names){ 
  folder_path <- "inst/extdata/"
  write_xlsx(data, paste0(folder_path, names, ".xlsx"))
}


list(data = haver_raw_list,
     names = names(haver_raw_list)) %>% 
  purrr::pmap(output_xlsx) 

df = usna

df = df %>%
  set_names(
    names_usna %>% 
      pull(reference) %>% 
      magrittr::extract(
        names(df) %>% 
          match(names_usna$code)
      )
  )

source('data-raw/national_accounts.R')
devtools::load_all()
fim::national_accounts %>% 
  select(-id) %>% 
  pivot_longer(-date) %>% 
  as_tibble() %>% 
  pivot_wider(names_from = date,
              values_from = value) %>% 
  openxlsx::write.xlsx('data/haver_pivoted.xlsx', overwrite = TRUE)
