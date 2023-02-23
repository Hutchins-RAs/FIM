Sys.setenv(TZ = 'UTC') # Set the default time zone to UTC (Coordinated Universal Time)

librarian::shelf(tidyverse, tsibble, lubridate, glue, TimTeaFan/dplyover, zoo, TTR, fs, gt, openxlsx, 
                 snakecase, rlang, fredr) # Load packages
devtools::load_all() # Load all functions in package

# Set the value of 'month_year' to the current month and year (in the format "mm-yyyy")
month_year <- glue('{format.Date(today() - 7, "%m")}-{year(today())}')


if(month(today() - 7 
         -months(1)) < 10){
  last_month_year <- glue('0{month(today() - 7 -months(1))}-{year(today() - dmonths(1) - dweeks(1))}')
} else{
  last_month_year <- glue('{month(today() - 7 -months(1))}-{year(today() - dmonths(1) - dweeks(1))}')
  
}

current<- read.xlsx(glue('results/{month_year}/fim-{month_year}.xlsx'))

#get percent contributions
date_id <- current[, c("date", "id")]
current_contribution<- current[, grep("_contribution$", names(current))]
current_contribution<- cbind(date_id, current_contribution)

current_contribution$lead_id = lead(current_contribution$id) 
forecast_period<- current_contribution %>% filter(lead_id == "projection") %>% mutate(federal_rebate_checks_arp_contribution=rebate_checks_arp_contribution,
                                                                                      federal_rebate_checks_contribution = rebate_checks_contribution)

date_id <- forecast_period[, c("date", "id")]


# extract level 1 and level 2 from variable names
level1 <- sub("_.*", "", names(forecast_period))
level2 <- sub("^(federal|state)_", "", names(forecast_period))
level2 <- sub("_contribution$", "", level2) 

# create a named vector to specify the order of level 2 variables
level2_order <- c("purchases", "consumption_grants", "investment_grants", 
                  "social_benefits", "subsidies", 
                  "corporate_taxes", "non_corporate_taxes", "ui", "rebate_checks", "health_outlays", "student_loans", "other_vulnerable_arp", 
                  "rebate_checks_arp", "other_direct_aid_arp", 
                  "aid_to_small_businesses_arp")
level2_ordered <- factor(level2, levels = level2_order, ordered = TRUE)

# order the data frame by level 2 first, and then level 1 within each level 2
forecast_period_ordered <- forecast_period[, order(level2_ordered, level1)]

level1_order <- c("federal", "state")
# create a vector of all column names in the desired order
ordered_colnames <- character(length(colnames(forecast_period)))
k <- 0
for (level2_name in level2_order) {
  for (level1_name in level1_order) {
    k <- k + 1
    colname <- paste(level1_name, level2_name, "contribution", sep = "_")
    if (colname %in% colnames(forecast_period)) {
      ordered_colnames[k] <- colname
    } else {
      next
    }
  }
}
ordered_colnames <- ordered_colnames[ordered_colnames != ""]


# order the columns of the forecast_period data frame
forecast_period_ordered <- forecast_period[, ordered_colnames]


forecast_period_ordered<- cbind(date_id, forecast_period_ordered)

components<- head(forecast_period_ordered, n=9)


#openxlsx::write.xlsx(components, file = glue('results/{month_year}/components-{month_year}.xlsx'), overwrite = TRUE)


component_names<- names(components)[3:22]

compute_fiscal_impact <- function(df, component_names) {
  df$fiscal_impact <- rowSums(df[, component_names])
  return(df)
}

forchart<- compute_fiscal_impact(components, component_names)

library(ggplot2)

# select the first row and remove id and date columns
row <- forchart[1,-c(1,2)]

library(reshape2)
col<- melt(row)

  
