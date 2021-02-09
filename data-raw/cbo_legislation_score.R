cbo_legislation_score <-
  readxl::read_xlsx(here::here('inst', 'extdata', 'pandemic_legislation.xlsx'),
            sheet = 'annual') %>%
  select(-1) %>%
  pivot_longer(-date) %>%
  pivot_wider(names_from = date, values_from = value) %>%
  rename(year = name) %>%
  janitor::clean_names() %>%
  mutate(
    across(everything(), ~ replace_na(., 0)),
    across(where(is.numeric), ~ . * 4),
    year = as.numeric(year)
  ) 

cbo_legislation_score %>%
  saveRDS('data/cbo_legislation_score.rds')
