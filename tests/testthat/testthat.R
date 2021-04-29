# library(testthat)
# library(fim)
# 
# librarian::shelf('tidyverse', 'tsibble', 'readxl', 'arsenal')
# 
# 
# df <-
#   read_xlsx('inst/extdata/fim-february.xlsx') %>% 
#   mutate(date = yearquarter(date)) %>% 
#   as_tsibble(index = date) %>% 
#   rename(consumption_grants = federal_cgrants,
#          ui = unemployment_insurance,
#          ui_post_mpc = unemployment_insurance_post_mpc,
#          ui_contribution_df = unemployment_insurance_cont) %>% 
#   mutate(social_benefits2 = social_benefits - ui - rebate_checks,
#          federal_social_benefits2 = federal_social_benefits - federal_unemployment_insurance_override - rebate_checks,
#          state_social_benefits2 = state_social_benefits - state_unemployment_insurance
#          ) %>% 
# filter_index('1999 Q4' ~ '2022 Q4')
# 
# compare <- function( variable_df1, variable_df2){
#   fim  %>% 
#     filter_index('2020 Q4' ~ '2022 Q1') %>% 
#     select(date, id, {{variable_df1}}) %>% 
#     left_join(df %>% select({{variable_df2}})) %>% 
#     mutate(diff  =  round({{variable_df1}} - {{ variable_df2 }}))
# }
# 
# tar_load(fim)
# fim <- fim %>% 
#   filter_index('1999 Q4' ~ '2022 Q4')
# 
# 
# # Taxes ---------------------------------------------------------------------------------------
# 
# 
# test_that("Taxes cont are the same", {
#   expect_equal(fim[['non_corporate_taxes_contribution']], df[['noncorp_taxes_cont']],
#                tolerance = 0.005)
#   expect_equal(fim[['corporate_taxes_contribution']], df[['corporate_taxes_cont']],
#                0.005)
#   
# })
# 
# test_that("Taxes cont are the same", {
#   expect_equal(fim[['corporate_taxes_contribution']], df[['corporate_taxes_cont']],
#                0.005)
# })
# 
# 
# # Transfers -----------------------------------------------------------------------------------
# 
# 
# test_that("Transfer contributions are the same", {
#   expect_equal(fim[['health_outlays_contribution']], df[['health_outlays_cont']],
#                0.01)
#   
#   expect_equal(fim[['federal_health_outlays_contribution']], df[['federal_health_outlays_cont']],
#                0.005)
#   
#   expect_equal(fim[['state_health_outlays_contribution']], df[['state_health_outlays_cont']],
#                0.005)
#   
#   expect_equal(fim[['state_health_outlays']], df[['state_health_outlays']],
#                0.005)
#   
#   expect_equal(fim[['add_state_health_outlays']], df[['add_state_health_outlays']],
#                0.005)
#   
# })
# 
# 
# # Health --------------------------------------------------------------------------------------
# 
# 
# test_that("Add factors are the same",{
#   expect_equal(fim[['add_state_health_outlays']], df[['add_state_health_outlays']],
#                0.005)
#   expect_equal(fim[['add_federal_health_outlays']], df[['add_federal_health_outlays']],
#                0.005)
#   
#   expect_equal(fim[['add_state_health_outlays']], df[['add_state_health_outlays']],
#                0.005)
# })
# 
# test_that("Taxes cont are the same", {
#   expect_equal(fim[['social_benefits_contribution']], df[['social_benefits_cont']],
#                0.01)
#   
#   expect_equal(fim[['federal_social_benefits_contribution']], df[['federal_social_benefits_cont']],
#                0.005)
#   
#   expect_equal(fim[['state_social_benefits_contribution']], df[['state_social_benefits_cont']],
#                0.005)
#   expect_equal(fim[['federal_social_benefits']], df[['federal_social_benefits']],
#                0.005)
#   
#   expect_equal(fim[['state_social_benefits']], df[['state_social_benefits2']],
#                0.005)
#   
#   expect_equal(fim[['add_state_social_benefits']], df[['add_state_social_benefits']],
#                0.005)
#   
# })
# 
# 
# # UI ------------------------------------------------------------------------------------------
# 
# 
# 
# test_that("Taxes cont are the same", {
#   expect_equal(fim[['ui_contribution']], df[['federal_unemployment_insurance_cont']] + df[['state_unemployment_insurance_cont']] ,
#                0.01)
#   
#   expect_equal(fim[['federal_ui_contribution']], df[['federal_unemployment_insurance_cont']],
#                0.005)
#   
#   expect_equal(fim[['state_ui_contribution']], df[['state_unemployment_insurance_cont']],
#                0.005)
#   expect_equal(fim[['federal_ui']], df[['federal_unemployment_insurance']],
#                0.005)
#   
#   expect_equal(fim[['state_ui']], df[['state_unemployment_insurance']],
#                0.005)
#   expect_equal(fim[83:94, 'state_ui'] %>% pull(), df[83:94,'state_unemployment_insurance_override'] %>% pull(),
#                0.005)
#   
#   expect_equal(fim[83:94, 'ui'] %>% pull(), df[83:94,'unemployment_insurance_override'] %>% pull(),
#                0.005)
#   
# }) 
# ### REBATE 
# test_that('Rebate checks are the same',{
#   expect_equal(fim[['rebate_checks']], df[['rebate_checks']])
#   expect_equal(fim[['rebate_checks_contribution']], df[['rebate_checks_cont']],
#                0.005)
# })
# 
# ### SUBSIDIES
# test_that('Subsidies are the same',{
#   expect_equal(fim[['subsidies']], df[['subsidies']])
#   expect_equal(fim[['subsidies_contribution']], df[['subsidies_cont']],
#                0.005)
# })
# 
# ### HEALTH
# test_that('Health outlays are the same',{
#   expect_equal(fim[['health_outlays']], df[['health_outlays']])
#   expect_equal(fim[['health_outlays_contribution']], df[['health_outlays_cont']],
#                0.005)
# })
# ## SOCIAL BENEFITS AGGREGATE
# 
# test_that('Federal UI matches',{
#   net <- df$federal_social_benefits 
#   expect_equal(fim$federal_social_benefits, net)
# })
# 
# test_that('SB Projections equivalent',{
# projections <- projections %>% filter_index('1999 Q4'~'2022 Q4') %>% 
#   mutate(federal_social_benefits = federal_social_benefits )
#   x <- df$federal_social_benefits  + df$federal_health_outlays-df$add_federal_social_benefits-df$add_federal_health_outlays-df$federal_rebate_checks
#   expect_equal(projections$federal_social_benefits, x)
# })
# 
# test_that("Social benefits are the same", {
# state_sb_total <- fim$state_social_benefits 
# 
# state_sb_net <- df$state_social_benefits 
#   
#   expect_equal(fim[['state_social_benefits']], state_sb_net,
#                0.005)
# })
# test_that('Aggregate',{
#   expect_equal(fim$rebate_checks, df$rebate_checks)
#   
#   ui <- df$federal_unemployment_insurance + df$state_unemployment_insurance
#   expect_equal(fim$ui, ui)
#   
#   expect_equal(fim$federal_social_benefits_growth, df$federal_sociall)
# })
# test_that('Aggregate SB Equal',{
#   expect_equal(fim$social_benefits, df$social_benefits)
# })
# 
# test_that('State UI matches',{
#   expect_equal(fim$state_ui, df$state_unemployment_insurance)
#   
#   expect_equal(fim$add_state_social_benefits, df$add_state_social_benefits)
# })
