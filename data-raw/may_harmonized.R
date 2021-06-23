## code to prepare `may_harmonized` dataset goes here
librarian::shelf(dplyr, tsibble)
may <- readxl::read_xlsx('results/5-2021/fim-5-2021.xlsx') %>% 
  mutate(date = yearquarter(date)) %>% 
  as_tsibble(key = id,
             index = date)

may %>% 
  mutate(federal_purchases = federal_purchases + federal_non_health_grants_arp,
         federal_purchases_contribution = federal_purchases_contribution + federal_non_health_grants_arp_contribution) %>% 
  mutate(federal_ui = federal_ui + federal_ui_arp,
         federal_ui_contribution = federal_ui_contribution + federal_ui_arp_contribution) %>% 
  mutate(federal_ui = federal_ui + federal_ui_arp,
         federal_ui_contribution = federal_ui_contribution + federal_ui_arp_contribution) %>% 
  mutate(federal_health_outlays= federal_health_outlays + federal_health_grants_arp,
         federal_health_outlays_contribution = federal_health_outlays_contribution + federal_health_grants_arp_contribution)


usethis::use_data(may, overwrite = TRUE)
