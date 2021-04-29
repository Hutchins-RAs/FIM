# library(testthat)
# devtools::load_all()
# librarian::shelf('tidyverse', 'tsibble', 'readxl', 'arsenal', 'targets')
# conflicted::conflict_prefer("filter", "dplyr")
# 
# fim <- 
#   tar_read(fim) %>% 
#   dplyr::filter(id == 'historical') %>% 
#   filter_index('2000 Q1' ~ '2020 Q4')
# historical_fim <-
#   read_xlsx('inst/extdata/fim-february.xlsx')%>% 
#   mutate(date = yearquarter(date)) %>% 
#   as_tsibble(index = date) %>% 
#   rename(consumption_grants = federal_cgrants,
#          federal_contribution = federal_cont, 
#          state_contribution = state_local_cont,
#          ui = unemployment_insurance,
#          ui_post_mpc = unemployment_insurance_post_mpc,
#          ui_contribution = unemployment_insurance_cont) %>% 
#   mutate(social_benefits2 = social_benefits - ui - rebate_checks,
#          federal_social_benefits2 = federal_social_benefits - federal_unemployment_insurance_override - rebate_checks,
#          state_social_benefits2 = state_social_benefits - state_unemployment_insurance
#   ) %>% 
#   filter_index('2000 Q1' ~ '2020 Q4')
# 
# 
# # Headline ----------------------------------------------------------------
# 
# 
# test_that("Historical headline FIM is the same", {
#   expect_equal(fim$fiscal_impact,
#                historical_fim$fiscal_impact,
#                tolerance = 0.1)
# })
# 
# 
# # Components --------------------------------------------------------------
# 
# 
# test_that('Pre-covid levels are the same', {
#   expect_equal(fim$federal_social_benefits, 
#                historical_fim$federal_social_benefits)
# })
# 
# test_that('Purchases match', {
#   expect_equal(fim$consumption_grants,
#                historical_fim$consumption_grants)
# })
# 
# test_that("Pre-covid FIM components  are the same", {
#   # Purchases
#   expect_equal(fim$federal_contribution,
#                historical_fim$federal_contribution,
#                tolerance = 0.0001)
#   expect_equal(fim$state_contribution,
#                historical_fim$state_contribution,
#                tolerance = 0.0001)
#   # Transfers
#   expect_equal(fim$transfers_contribution,
#                historical_fim$transfers_cont,
#                tolerance = 0.0001)
#   expect_equal(fim$federal_transfers_contribution,
#                historical_fim$federal_transfers_cont,
#                tolerance = 0.0001)
#   expect_equal(fim$state_transfers_contribution,
#                historical_fim$state_transfers_cont,
#                tolerance = 0.001)
#   # Taxes
#   expect_equal(fim$taxes_contribution,
#                historical_fim$taxes_cont,
#                tolerance = 0.001)
# })
# 
# test_that('Rebate checks have the same level and contribution', {
#   # expect_equal(fim$ui_contribution,
#   #              historical_fim$ui_contribution)
#   expect_equal(fim$rebate_checks,
#                historical_fim$rebate_checks)
#   expect_equal(fim$rebate_checks_contribution,
#                historical_fim$rebate_checks_cont)
#   # expect_equal(fim$social_benefits_contribution,
#   #              historical_fim$social_benefits_cont)
# })
# 
# 
# test_that('Health outlays', {
#   expect_equal(fim$health_outlays_contribution,
#                historical_fim$health_outlays_cont)
# })
# 
# test_that('Subsidies',{
#   expect_equal(fim$subsidies,
#                historical_fim$subsidies)
# })
