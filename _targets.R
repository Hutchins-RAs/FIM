librarian::shelf('tidyverse', 'zoo', 'TTR', 'tsibble', 'targets', 'tarchetypes', 'lubridate',
                 'alistaire47/pipecleaner', 'glue', 'validate', 'fim', 'dplyover', 'tsibble')

devtools::load_all()

conflicted::conflict_prefer('filter', 'dplyr') 
conflicted::conflict_prefer('lag', 'dplyr')
# Build workflow plan data frame.

options(tidyverse.quiet = TRUE)
options(crayon.enabled = FALSE)



# Set target-specific options such as packages.
tar_option_set(error = "workspace")



# End this file with a list of target objects.
tar_plan(
  projections = 
    read_data() %>%
    define_variables() %>%
    growth_assumptions() %>%
    reallocations() %>%
    forecast() %>%
    mutate(across(where(is.numeric),
                  ~ coalesce(.x, 0))) %>% 
    get_non_corporate_taxes(),
  fim = 
    projections %>%
    add_factors() %>%
    get_overrides() %>%
    mutate(grants = consumption_grants + investment_grants,
           federal_purchases_deflator_growth = q_g(federal_purchases_deflator),
           state_purchases_deflator_growth = q_g(state_purchases_deflator),
           consumption_grants_deflator_growth = q_g(consumption_grants_deflator),
           investment_grants_deflator_growth = q_g(investment_grants_deflator),
           consumption_deflator_growth  =  q_g(consumption_deflator),
           real_potential_gdp_growth = q_g(real_potential_gdp)) %>% 
    purchases_contributions() %>% 
    mutate(social_benefits = social_benefits - ui - rebate_checks,
           federal_social_benefits = federal_social_benefits - federal_ui - rebate_checks,
           state_social_benefits = state_social_benefits - state_ui) %>% 
    
    taxes_transfers_minus_neutral() %>% 
    mpc_taxes_transfers() %>% 
      taxes_contributions() %>% 
    sum_taxes_contributions() %>% 
    transfers_contributions() %>% 
      sum_transfers_contributions() %>% 
    sum_taxes_transfers() %>% 
    get_fiscal_impact(),
  summary = 
    fim %>% 
      filter_index('2020 Q2' ~ '2021 Q1') %>% 
      select(date, id, fiscal_impact, social_benefits_contribution,
             health_outlays_contribution, subsidies_contribution,
             ui_contribution, rebate_checks_contribution),
  levels =
    fim %>% 
      filter_index('2020 Q2' ~ '2021 Q1') %>% 
      select(date, id, fiscal_impact, social_benefits,
             health_outlays, subsidies,
             ui_contribution, rebate_checks_contribution)
)
# tar_load(projections)
# mpc <- 0.7
# weights2 <- c(rep(0.08, 6), 0.15, 0.35)
# 
# rebate_summary <-
#   projections %>% 
#   add_factors() %>% 
#   get_overrides() %>% 
#   filter_index('2016 Q1' ~ '2021 Q1') %>% 
#   select(date, id,gdp, real_potential_gdp_growth, consumption_deflator_growth,
#          rebate_checks ) %>% 
#   mutate(lag = lag(rebate_checks),
#          counter = lag * (1 + real_potential_gdp_growth +  consumption_deflator_growth),
#          net  = rebate_checks -   counter,
#          mpc = mpc  *  roll_sum(x = net, width = 8, weights2,  online = FALSE),
#                                 cont  = 400  * mpc  /  lag(gdp))
#   
# tar_load(fim)
# fim_summary <-
#   fim %>% 
#   filter_index('2020 Q1' ~ '2021 Q1') 
# 
# library('XLConnect')
# 
# #loading the package
# 
# #create an Excel workbook. Both .xls and .xlsx file formats can be used.
# mywb <- loadWorkbook("summary.xlsx", create = TRUE)
# #Create a sheet called GSStock within the Excel workbook
# createSheet(mywb, name = "Contributions")
# createSheet(mywb, name = "Transfers Contributions")

# 
# #load data from the flat file into R as a data frame
# contributions <- fim_summary %>% 
#   select(date, id, fiscal_impact, federal_purchases_contribution, state_purchases_contribution, 
#          taxes_contribution, transfers_contribution) %>% 
#   rename_with(~ snakecase::to_title_case(.))
# 
# contributions_transfers <- 
#   fim_summary %>% 
#   select(date, id, all_levels(c('transfers_contribution', 'social_benefits_contribution',
#                                 'health_outlays_contribution', 'subsidies_contribution', 'ui_contribution')),
#          rebate_checks_contribution) %>% 
#   rename_with(~ snakecase::to_title_case(.))

# 
# transfers_levels <- 
#   fim_summary %>% 
#   select(date, id, real_potential_gdp_growth, consumption_deflator_growth, all_levels(c('social_benefits',
#                                 'health_outlays', 'subsidies', 'ui')),
#          rebate_checks) %>% 
#   rename_with(~ snakecase::to_title_case(.))
# 
# #write the gs_data data frame into GSStock sheet in the new workbook
# writeWorksheet(mywb, contributions,
#                , sheet = "Contributions", startRow = 1, startCol = 1)

# writeWorksheet(mywb, contributions_transfers,
#                 sheet = "Transfers Contributions", startRow = 1, startCol = 1)
# 
# writeWorksheet(mywb, transfers_levels, sheet = 'Transfers Contributions',
#                startRow = 7)
# #save the workbook to the corresponding Excel file and writes the file to disk.
# saveWorkbook(mywb)
# 


s# Next steps:
#  - add factors
#  - overrides
#  - grants contributions
#  - taxes trannsfers counterfactuals
#  - calculate mpcs
#  - taxes and transfers contributionsno
#  
#  
#  
# fim_create(projections) %>%
#   add_factors(last_date = last_hist_date) %>%
#   override_projections() %>%
#   mutate(
#     federal_cgrants = if_else(date == '2020-12-31', 303.95, federal_cgrants)
#   ) %>%
#   fill_overrides() %>%
#   contributions_purchases_grants() %>%
#   total_purchases() %>%
#   mutate(federal_cont = federal_cont - federal_grants_cont,
#          state_local_cont = state_local_cont + federal_grants_cont) %>%
#   remove_social_benefit_components() %>%
#   taxes_transfers_minus_neutral() %>%
#   calculate_mpc('subsidies') %>%
#   calculate_mpc('health_outlays') %>%
#   calculate_mpc('social_benefits') %>%
#   calculate_mpc('unemployment_insurance') %>%
#   calculate_mpc('rebate_checks') %>%
#   calculate_mpc('noncorp_taxes') %>%
#   calculate_mpc('corporate_taxes') %>%
#   taxes_contributions() %>%
#   sum_taxes_contributions() %>%
#   transfers_contributions() %>%
#   sum_transfers_contributions() %>%
#   sum_taxes_transfers() %>%
#   add_social_benefit_components() %>%
#   get_fiscal_impact() 

# rules <-
#   validate::validator(subsidies == federal_subsidies + state_subsidies,
#             health_outlays == federal_health_outlays + state_health_outlays,
#             social_benefits == federal_social_benefits + state_social_benefits,
#             
#             purchases == federal_purchases + state_purchases ,
#             ui_expansion == peuc + pua + puc + wages_lost_assistance,
#             health_grants>= medicaid_grants)
# out <- validate::confront(df, rules)
# summary(out)
