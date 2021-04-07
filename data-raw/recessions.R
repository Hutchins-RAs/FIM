## code to prepare `recessions` dataset goes here
conflict_prefer("filter", "dplyr")


recessions <-
  fim::national_accounts %>% 
  as_tibble() %>% 
  select(date, recession = recessq) %>% 
  mutate(recession = coalesce(recession, -1),
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
         recession_end = coalesce(recession_end,
                                  tsibble::yearquarter(Sys.Date()) -1),
         .keep = 'used') %>% 
  unique()


usethis::use_data(recessions, overwrite = TRUE)
