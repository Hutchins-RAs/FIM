df <-
  fim %>% 
  filter_index('2020 Q1' ~ '2021 Q1')

variables <- c('purchases', 'social_benefits', 'health_outlays', 'subsidies', 'ui', 'rebate_checks')
contributions <-
  df %>% 
  mutate(purchases_contribution = federal_purchases_contribution + state_purchases_contribution ) %>% 
  select(date, id, fiscal_impact, 
         purchases_contribution,
         taxes_contribution,
         transfers_contribution, rebate_checks_contribution, all_levels(c('social_benefits_contribution',
         'health_outlays_contribution', 'subsidies_contribution', 'ui_contribution'))
 ) %>% 
  rename_with(~ snakecase::to_title_case(.))

levels <-
  
  df %>% 
  mutate(purchases = federal_purchases + state_purchases ,
         taxes = corporate_taxes + non_corporate_taxes,
         transfers = social_benefits + subsidies + health_outlays) %>% 
  

  select(date, id, fiscal_impact, gdp, real_potential_gdp_growth, consumption_deflator_growth,
         purchases,
         taxes,
         transfers, rebate_checks, all_levels(c('social_benefits',
                                              'health_outlays', 'subsidies', 'ui'))
  ) %>% rename_with(~ snakecase::to_title_case(.))

rebate_checks <-
  fim %>% 
  taxes_transfers_minus_neutral() %>% 
  select(date, id, gdp, real_potential_gdp_growth, consumption_deflator_growth, rebate_checks, 
         rebate_checks_minus_neutral, rebate_checks_post_mpc, rebate_checks_contribution) %>% 
  filter_index('2018 Q1' ~ '2021 Q1')

social_benefits <-
  fim %>% 
  taxes_transfers_minus_neutral() %>% 
  select(date, id, gdp, real_potential_gdp_growth, consumption_deflator_growth, social_benefits, 
         social_benefits_minus_neutral, social_benefits_post_mpc, social_benefits_contribution) %>% 
  filter_index('2018 Q1' ~ '2021 Q1')
  
purchases <-
  fim %>% 
  mutate(purchases_contribution = federal_purchases_contribution + state_purchases_contribution) %>% 
  select(date, id, gdp, real_potential_gdp_growth, all_levels('purchases', 'purchases_contribution'), grants_contribution,
         grants, consumption_grants, investment_grants) %>% 
  filter_index('2018 Q1' ~ '2021 Q1')
# Excel workbook ------------------------------------------------------------------------------
librarian::shelf('openxlsx')

wb <- createWorkbook()
## Formatting can be applied simply through the write functions
## global options can be set to further simplify things
options("openxlsx.borderStyle" = "thin")
options("openxlsx.borderColour" = "#4F81BD")
options("openxlsx.numFmt" = "#,#0.00")
modifyBaseFont(wb, fontSize = 10, fontName = "Arial Narrow")


## headerStyles
hs1 <- createStyle(fgFill = "#4F81BD", halign = "CENTER", textDecoration = "Bold",
                   border = "Bottom", fontColour = "white")

## create a workbook and add a worksheet
## headerStyles

addWorksheet(wb, "Contributions")
addWorksheet(wb, "Levels")
addWorksheet(wb, "Rebate")
addWorksheet(wb, "Social Benefits")
addWorksheet(wb, "Purchases")




## writing as an Excel Table

freezePane(wb, sheet = 1, firstRow = TRUE, firstCol = TRUE) ## freeze first row and column
writeDataTable(wb, 1, contributions, startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")

freezePane(wb, sheet = 2, firstRow = TRUE, firstCol = TRUE) ## freeze first row and column
writeDataTable(wb, 2, levels, startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")



freezePane(wb, sheet = 3, firstRow = TRUE, firstCol = TRUE) ## freeze first row and column
writeDataTable(wb, 3, rebate_checks, startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")
freezePane(wb, sheet = 4, firstRow = TRUE, firstCol = TRUE) ## freeze first row and column
writeDataTable(wb, 4, social_benefits, startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")
writeDataTable(wb, 5, purchases, startRow = 1, startCol = 1, tableStyle = "TableStyleLight9")


## Save
saveWorkbook(wb, "analysis/data/derived_data/summary.xlsx", overwrite = TRUE)

openxlsx::openXL(wb)

openXL(wb) ## opens a temp version

