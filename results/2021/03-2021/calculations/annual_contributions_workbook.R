tar_load(fim)

yoy <- function (x){
  j = c()
  for (i in 4:length(x)) {
    j[i] = (((x[i] / x[i - 4])) - 1) * 100
  }
  j[1] = 0
  j
}

deannualize <- function(x){
  ( x / 100 + 1) ^ (1 / 4)
}

 fim %>% 
  select(date, fiscal_impact, arp_cont, fiscal_impact_moving_average) %>% 
  mutate(date = tsibble::yearquarter(date)) %>% 
  filter(date >= yearquarter('2018 Q4')) %>% 
  mutate(level = 100,
         fiscal_impact = deannualize(fiscal_impact),
         fim_cumulative = cumprod(fiscal_impact),
         arp_cont = deannualize(arp_cont),
         arp_cumulative = cumprod(arp_cont),
         index_arp = 100 * arp_cumulative,
         growth_arp = if_else(date > yearquarter('2019 Q4'),
                               yoy(index_arp), 
                               0),

         
         level = level * fim_cumulative,
         
         growth = if_else(date > yearquarter('2019 Q4'),
                          yoy(level),
                          0)) %>% 
  rename(arp_impact = arp_cont,
         fim_index = level,
         arp_index = index_arp,
         arp_yoy_growth = growth_arp,
         fim_yoy_growth = growth) %>% 
  summarise(date, fiscal_impact  , fim_index, fim_yoy_growth ,
         arp_impact, arp_index, arp_yoy_growth, fiscal_impact_moving_average) %>% 
  select(date, fiscal_impact_moving_average, fim_yoy_growth) %>% 
   pivot_longer(-date) %>% 
   ggplot(aes(x = date, y =value , color = name)) +
   geom_line()
   

library(openxlsx)

wb <- createWorkbook()
options("openxlsx.borderColour" = "#4F80BD")
options("openxlsx.borderStyle" = "thin")
modifyBaseFont(wb, fontSize = 12)



addWorksheet(wb, sheetName = "Year over year growth rates", gridLines = FALSE)
freezePane(wb, sheet = 1, firstRow = TRUE, firstCol = TRUE) ## freeze first row and column
writeDataTable(wb, sheet = 1, x = df,
               colNames = TRUE, rowNames = FALSE,
               tableStyle = "TableStyleLight9")

setColWidths(wb, sheet = 1, cols = "A", widths = 18)

saveWorkbook(wb, "results/3-2021/annual_contributions.xlsx", overwrite = TRUE) #