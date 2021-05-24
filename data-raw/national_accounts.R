## code to prepare `historical` dataset goes here

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

