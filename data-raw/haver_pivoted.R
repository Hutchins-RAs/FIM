fim::national_accounts %>% 
  select(-id) %>% 
  pivot_longer(-date) %>% 
  as_tibble() %>% 
  pivot_wider(names_from = date,
              values_from = value) %>% 
  openxlsx::write.xlsx('data/haver_pivoted.xlsx')
