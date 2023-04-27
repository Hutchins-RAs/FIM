# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
Sys.setenv(TZ = 'UTC')
librarian::shelf(Haver, dplyr, tidyr, readxl, writexl, tsibble, purrr, openxlsx)

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


# Wages Lost Assistance Program (Monthly)
wla <- pull_data('YPTOLM',
                 'usna',
                 frequency = 'monthly',
                 start.date = START) %>%
  monthly_to_quarterly() %>%
  mutate(yptolm = na_if(yptolm, NaN))
# Child Tax Credit (Monthly)

# Since Haver only pulls monthly values, you should manually input the quarterly 
# value if monthly personal income hasn't come out yet. You can find it on the 
# Effects of Selected Federal Pandemic Response Programs on Federal Government Receipts,
# Expenditures, and Saving. For example, after the release of Q1 2022 advanced estimate I ran the code chunk below to pull the monthly data, and then manually inputed the quarterly value with:
# 
# mutate_where(.data = ctc, .where = date == yearquarter('2022 Q1'), yptocm = 105.6)

ctc <- pull_data('YPTOCM',
                 'usna',
                 frequency = 'monthly',
                 start.date = START) %>%
  monthly_to_quarterly() %>% 
  mutate(yptocm = na_if(yptocm, NaN))

usna <-
  pull_data(names_usna$code,
            "usna",
            start.date = START) %>%
  as_tibble() %>% 
  left_join(cpi) %>%
  left_join(usecon) %>% 
 # left_join(child_tax_credit) %>% 
  # Convert SNAP from millions to billions
  mutate(gftffx = gftffx / 1e3) %>% 
  left_join(ctc, by = 'date')




monthly_state_ui <- c('LICL', 'LWCL', 'LUFP','LULP','LUWC','LUWP','LUBP','LUWB','LUEX','LUD','LUWBY', 'LUBPT', 'LUFPT', 'LULPT', 'LASGOVA', 'LALGOVA', 'CPGS')

state_ui <- pull_data(monthly_state_ui,
                         'usecon',
                         start.date = START) %>%
  as_tibble() %>%
  write_xlsx('data/monthly_state_ui.xlsx')




national_accounts <- 
  usna %>% 
  mutate(id = 'historical') %>%
  millions_to_billions() %>%
  rename(cpiu = ui,
         
  ) %>% 
  # Get deflator growth
  mutate(across(starts_with('j'), ~ q_g(.x), .names = '{.col}_growth')) %>% 
  format_tsibble() %>% 
  #When adding new codes to read in from Haver, make sure to relocate them at the end of the spreadsheet using the below function:
  relocate(ylwsd:gftfbdx, .after = 'jgsi_growth') %>% 
  relocate(yptocm, .after = everything())

usethis::use_data(national_accounts, overwrite = TRUE)
devtools::load_all()

# Write Haver pivoted to corresponding sheet in the forecast workbook

haver_pivoted <-
  fim::national_accounts %>% 
  select(-id) %>% 
  pivot_longer(-date) %>% 
  as_tibble() %>% 
  pivot_wider(names_from = date,
              values_from = value) 


boldHeader <- createStyle(textDecoration = 'bold') # Makes first row bold
wb <- loadWorkbook('data/forecast.xlsx')
if (!('Haver Pivoted' %in% names(wb))) addWorksheet(wb, 'Haver Pivoted')
writeData(wb, 'Haver Pivoted', haver_pivoted, headerStyle = boldHeader)
setColWidths(wb, 'Haver Pivoted', cols = 1:ncol(haver_pivoted), widths = 'auto')
saveWorkbook(wb, 'data/forecast.xlsx', overwrite = T)
# Check values and then:
# gert::git_commit_all('Haver update')
# gert::git_push()