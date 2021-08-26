## code to prepare `historical` dataset goes here
library("writexl")
library("openxlsx")
library("magrittr")
library('lubridate')
library("tidyverse")
national_accounts <- 
  
  readxl::read_xlsx('inst/extdata/national_accounts.xlsx') %>% 
  mutate(id = 'historical') %>%
  millions_to_billions() %>%
  rename(cpiu = ui,
         
         ) %>% 
  # mutate(jgdp = gdp / gdph,
  #        jc = c  / ch,
  #        jgf = gf /gfh,
  #        jgs = gs/  gsh,
  #        jgse =  jgse / 100,
  #        jgsi = jgsi / 100) %>%
  mutate(across(starts_with('j'), ~ q_g(.x), .names = '{.col}_growth')) %>% 
  format_tsibble()

usethis::use_data(national_accounts, overwrite = TRUE)


#writexl::write.xlsx(national_accounts, file = "data/forecast_06_2021.xlsx", sheetName="Haver", 'append')
## code to prepare `recessions` dataset goes here

recessions <-
  fim::national_accounts %>% 
  as_tibble() %>% 
  select(date, recession = recessq) %>% 
  mutate(
         date,
         diff = recession - dplyr::lag(recession),
         business_cycle = case_when(diff == 2 ~ 'recession_start',
                                    diff == -2 ~ 'recession_end',
                                    recession == 1 ~ 'recession',
                                    recession == -1 ~ 'expansion'),
         .keep = 'used') %>% 
  filter(business_cycle == 'recession_start' | business_cycle == 'recession_end') %>% 
  pivot_longer(business_cycle) %>% 
  mutate(date2 = as_date(date)) %>%
  pivot_wider(names_from = value, 
              values_from = date) %>% 
  select(recession_start, recession_end) %>% 
  mutate(across(any_of(c('recession_start', 'recession_end')),
                .fns = ~ coalesce(.x, dplyr::lead(.x))),
         recession_end = dplyr::lead(recession_end),
        # recession_end = coalesce(recession_end,
                                #  tsibble::yearquarter(Sys.Date()) -1),
         .keep = 'used') %>% 
  unique() %>% 
  drop_na()


usethis::use_data(recessions, overwrite = TRUE)

