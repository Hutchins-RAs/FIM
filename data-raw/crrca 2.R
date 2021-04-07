## code to prepare `crrca` dataset goes here


crrca <- readxl::read_xlsx('inst/extdata/projections.xlsx', sheet = 'crrca') %>% 
  as_tsibble(index = date) %>% 
  annual_to_quarter() %>% 
  mutate(federal_ui_timing = c(0.85, 0.15, 0, 0, rep(0.25, 4 * 11 - 4)),
         ppp_timing = c(0.5, 0.5, rep(0, 4 * 11 - 2)), 
         rebate_timing = c(1, rep(0, 4 * 11 - 1)), 
         food_timing = c(0.5, 0.5, rep(0, 4 * 11 - 2)),
         aviation_timing = c(rep(1/3, 3), rep(0, 4 * 11 - 3)),
         vaccines_timing = c(rep(0.35, 2), rep(0.15, 2), rep(0, 4 * 11 - 4))
  ) %>% 
  summarize(federal_ui = 4 * federal_ui * federal_ui_timing,
            ppp = 4 * ppp * ppp_timing,
            rebate_checks = 4 * rebate_checks * rebate_timing,
            other_crrca_federal_social_benefits = 4 * food_timing * (snap + agriculture) + eitc,
            other_crrca_subsidies = 4 * aviation_timing * aviation + employee_retention, 
            vaccines = coalesce(4 * vaccines * vaccines_timing, 0),
            other_crrca_grants = 4 / 12 * (housing_assistance + childcare + education_stabilization_fund + emergency_injury_disaster_relief + transit)) %>% 
  fill(other_crrca_grants, .direction  = 'down') %>% 
  mutate(crrca_subsidies = ppp + other_crrca_subsidies,
         crrca_grants = vaccines + other_crrca_grants) %>% 
  as_tibble()  %>% 
  mutate(date = as.character(date))
usethis::use_data(crrca, overwrite = TRUE)
