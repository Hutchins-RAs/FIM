## code to prepare `projections` dataset goes here
library(timetk)
library(tidyquant)
economic_projections <- readxl::read_xlsx('inst/extdata/projections.xlsx', sheet = 'economic') %>% 
  mutate(date = yearquarter(date)) %>%
  as_tsibble(index = date)

budget_projections <- readxl::read_xlsx('inst/extdata/projections.xlsx', sheet = 'budget') %>% 
  as_tsibble(index = fy) %>%
  annual_to_quarter() %>%
  fiscal_to_calendar() %>% 
  mutate(federal_ui_timing = case_when(date <= yearquarter('2020 Q4') ~ 0,
                                       date == yearquarter('2021 Q1') ~ 0.725,
                                       date == yearquarter('2021 Q2') ~ 0.275,
                                       date == yearquarter('2021 Q3') ~ 0,
                                       TRUE ~  1),
         federal_ui = yptu - state_ui,
         federal_ui = if_else(date >= yearquarter('2020 Q4') & date <= yearquarter('2021 Q3'),
                              4 * federal_ui_timing * federal_ui,
                              federal_ui))

medicaid_forecast <-
  readxl::read_xlsx('inst/extdata/projections.xlsx', sheet = 'budget') %>% 
  as_tsibble(index = fy) %>% 
  mutate(federal_medicaid = yptmd, .after = 'fy') %>% 
  left_join(fmap, by = 'fy') %>% 
  relocate(fmap) %>% 
  mutate(medicaid = if_else(!is.na(fmap), federal_medicaid / fmap, federal_medicaid), .before = 'federal_medicaid') %>% 
  mutate(medicaid_growth = (medicaid / lag(medicaid))^0.25 - 1, .after = 'fy') %>% 
  select(-fmap) %>% 
  as_tsibble(index = fy) %>% 
  annual_to_quarter() %>% 
  fiscal_to_calendar() %>% 
  left_join(fmap_quarterly, by = 'date') %>% 
  filter_index("2020 Q4" ~ .) %>% 
  mutate(state_medicaid = medicaid - federal_medicaid, .after = 'federal_medicaid')

combined <-
  read_data() %>% 
  rename(medicaid = yptmd) %>% 
  select(date, medicaid) %>% 
  left_join(medicaid_forecast %>% select(date, medicaid_growth, fmap), by = 'date') %>% 
  filter_index("2020 Q4" ~ .) %>% 
  project(medicaid, with = medicaid_growth) %>% 
  mutate(federal_medicaid = medicaid * fmap,
         state_medicaid = medicaid - federal_medicaid)

combined
for(i in 3:nrow(combined)){
  combined[i, 'medicaid'] = combined[i-1, 'medicaid'] * (1 + combined[i, 'medicaid_growth'])
}
  
combined %>% 
  mutate(federal_medicaid = fmap * medicaid,
         state_medicaid = medicaid - federal_medicaid)
fmap <- readxl::read_xlsx('inst/extdata/projections.xlsx', 
                          sheet = 'annual fmap') 

fmap_quarterly <- readxl::read_xlsx('inst/extdata/projections.xlsx',
                                    sheet = 'quarterly fmap') %>% 
  mutate(date = yearquarter(date))
  # mutate(date = yearquarter(date)) %>% 
  # separate(date, into = c('year', 'quarter'), sep = ' ') %>% mutate(fy = lead(year))
budget_projections %>% 
  mutate(federal_medicaid = yptmd, .after = 'date') %>% 
  left_join(fmap, by = 'fy') %>% 
  relocate(fmap) 
  mutate(medicaid = if_else(!is.na(fmap), federal_medicaid / fmap, federal_medicaid), .before = 'federal_medicaid') %>% 
  mutate(state_medicaid = medicaid - federal_medicaid, .after = 'federal_medicaid') %>% 
  mutate(across(all_of(contains('medicaid')),
                ~ zoo::rollapply(.x, width = 4, mean, fill = NA,min_obs = 1, align = 'right'))) %>% 
  mutate(across(contains('medicaid'), 
                ~ q_g(.x),
                .names = '{.col}_growth'), .after = 'state_medicaid') %>% 
  openxlsx::write.xlsx('medicaid_projections.xlsx')

cares <- readxl::read_xlsx('inst/extdata/projections.xlsx', sheet = 'CARES') %>% 
  summarize(date, cares_other_federal_social_benefits = nonprofit_ppp + nonprofit_provider_relief_fund,
            cares_other_subsidies = provider_relief_fund)

crrca <- readxl::read_xlsx('inst/extdata/projections.xlsx', sheet = 'crrca') %>% 
  as_tsibble(index = date) %>% 
  annual_to_quarter() %>% 
  mutate(federal_ui_timing = c(1, 0, 0, 0, rep(0.25, 4 * 11 - 4)),
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
         crrca_grants = vaccines + other_crrca_grants)
 

          


projections <- 
  budget_projections %>% 
  left_join(economic_projections, by = 'date') %>% 
  mutate(id = 'projection')
  

usethis::use_data(projections, overwrite = TRUE)
