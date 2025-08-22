
# This script sources data sets created from fiscal_impact_beta and saves them 
# as .rda files stored in shiny/cache

packages <- c(
  "tidyverse", "tsibble", "lubridate", "glue", "readxl",
  "TimTeaFan/dplyover", "zoo", "TTR", "fs", "gt", 
  "openxlsx", "snakecase", "rlang", "BrookingsInstitution/ggbrookings"
)
librarian::shelf(packages)

# Set the default time zone to UTC (Coordinated Universal Time)
Sys.setenv(TZ = 'UTC') 

# Determine last month
last_month_year <- glue('{format.Date(today() %m-% months(1), "%m")}-{year(today() %m-% months(1))}')
print(last_month_year)

# Parse that string into a Date
last_month_date <- as.Date(paste0("01-", last_month_year), format = "%d-%m-%Y")

# Make last_month_quarter a tsibble yearquarter
last_month_quarter <- yearquarter(last_month_date)

# forecast ---------------

# Define start_quarter 
start_quarter <- last_month_quarter - 1 
# Define end_quarter = 8 quarters (2 years) ahead
end_quarter <- last_month_quarter + 7   # because numeric adds in years, 8/4 = 2 years

forecast <- read_excel(
  path = glue('results/{last_month_year}/beta/contributions-{last_month_year}.xlsx'),
  col_names = TRUE
) %>%
  mutate(
    date = yearquarter(date),       
    quarter = quarter(date)
  ) %>%
  select(
    -id, -recession, -fiscal_impact_measure, -fiscal_impact_4q_ma,
    -federal_contribution, -state_contribution, -taxes_contribution,
    -transfers_contribution, -quarter
  ) %>%
  as_tsibble(index = date) %>%
  filter_index(as.character(start_quarter) ~ as.character(end_quarter)) %>%
  mutate(
    date = yearquarter(date))

save(forecast, 
     file = glue("shiny/cache/forecast.rda"))


# hutchins_fim ---------------

# Load the interactive-mm-yyyy.csv file from the beta folder of last month's results

hutchins_fim <- read_csv(
  file = glue('C:/Users/SAhmad/Downloads/FIM/results/{last_month_year}/beta/interactive-{last_month_year}.csv'),
  col_names = TRUE,
  show_col_types = FALSE
) %>%
  # Create date variable in the yearquarter format
  mutate(date = yearquarter(paste0(year, " ", quarter))) %>%
  # Rename variables 
  rename(
    fiscal_impact_4q_ma = impact,
    fiscal_impact_measure = total
  ) %>%
  # Select variables 
  select(date, fiscal_impact_measure, fiscal_impact_4q_ma)

save(hutchins_fim, 
     file = glue("C:/Users/SAhmad/Downloads/FIM/shiny/cache/hutchins_fim.rda"))


# fim_cache ---------------

# Load from the contributions 
fim_cache <- read_excel(
  path = glue('results/{last_month_year}/beta/contributions-{last_month_year}.xlsx'),
  col_names = TRUE
) %>%
  mutate(
    date = yearquarter(date)
  ) %>%
  select(date, fiscal_impact_measure)
    
save(fim_cache, 
     file = glue("shiny/cache/fim_cache.rda"))


# historical_overrides ---------------

historical_overrides <- import_historical_overrides()

save(historical_overrides, 
     file = glue("shiny/cache/historical_overrides.rda"))

# usna ---------------

# Select variables from national accounts 
national_accounts <- import_national_accounts() %>%
  rename(medicare = yptmr,
         medicaid = yptmd,
         ui = yptu,
         social_benefits = gtfp,
         federal_purchases = gf,
         state_purchases = gs,
         federal_corporate_taxes = gfrcp,
         state_corporate_taxes = gsrcp,
         medicaid_grants = gfeghdx,
         investment_grants = gfeigx,
         federal_subsidies = gfsub,
         state_subsidies = gssub,
         rebate_checks = gftfpe,
         consumption_deflator_growth = jc_growth,
         federal_purchases_deflator_growth = jgf_growth,
         state_purchases_deflator_growth = jgs_growth,
         consumption_grants_deflator_growth = jgse_growth,
         investment_grants_deflator_growth = jgsi_growth) %>%
  dplyr::select(id, date, gdp, medicare, medicaid, ui, social_benefits, 
                federal_purchases, state_purchases, federal_corporate_taxes, 
                state_corporate_taxes, medicaid_grants, investment_grants,
                federal_subsidies, state_subsidies, rebate_checks, 
                consumption_deflator_growth, federal_purchases_deflator_growth, 
                state_purchases_deflator_growth, consumption_grants_deflator_growth,  
                investment_grants_deflator_growth)

# Select remaining variables from inputs file 
inputs <- read_excel(
    path = glue('results/{last_month_year}/beta/inputs-{last_month_year}.xlsx'),
    col_names = TRUE) %>% 
  select(date, federal_social_benefits, state_social_benefits, 
             federal_ui, state_ui, consumption_grants, rebate_checks_arp, 
             federal_non_corporate_taxes, state_non_corporate_taxes, 
             consumption, real_potential_gdp_growth) %>%
  # Convert date formula to year quarter formmat to merge
  mutate(date = yearquarter(date))

# Merge data sets 
usna <- left_join(national_accounts, inputs, by = "date")

# Save in cache folder
save(usna, 
     file = glue("shiny/cache/usna.rda"))
