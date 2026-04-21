# NOTES
# We had the wrong UI numbers in the spreadsheet.
# We never updated the wages lost assistance spending, which was higher than we expected
# in Q3. Unfortunately, the previous number was hardcoded so not sure where it came from.
# It said Total Q3 was 881,354

# UI Q3 was also
# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
library('tidyverse')
library('readxl')
library('writexl')
library('tsibble')
library('janitor')

library('lubridate')
#source('_drake.R')
#
#


# Functions -----------------------------------------------------------------------------------

mpc_education_stabilization_fund <- function(x, n){
  mpc <- 1
  weights <- c(rep(1 / n, n))
  x <- x * weights
  return(x)
}

totals <- function(df, var){
  df %>% 
    as_tibble() %>%
    select(date, var) %>%
    pivot_wider(-date, names_from = date, values_from = var) %>%
    summarise(sum(c_across(everything()))) %>%
    pull()
}

annualize <-  function(x){
  x <-  x * 4
  return(x)
}

deannualize  <- function(x){
  x <- x / 4
  return(x)
}

mpc_covid_relief_fund <- 
  function(x){
    mpc <- 1
    weights <- c(0.0583, 0.075, rep(0.1, 2), rep(0.08333, 8))
    mpc * roll::roll_sum(x, width = length(weights),
                         weights = rev(weights), online = FALSE)
  }

spread_out <-function(x){
  w <-  c(0.5,0.5)
  length <- length(w)
  x  <- rep(x, length)
  x * w
}

fill_in <- function(prev, new, growth = 0.03) {
  if_else(!is.na(new), new, prev * (1 + growth))
}


# 0.1 Pull Raw Data---------------------------------------------------------------
tar_load(projections)
START <- as_date("2018-12-31")
share_nonprofit_hospitals <- 0.5
share_government_hospitals <- 0.2
share_forprofit_hospitals <- 0.3


df_raw <- readRDS('data/cbo_legislation_score.rds') 
df <- 
  df_raw %>%
  as_tsibble(index  = year) %>%
  annual_to_quarter() %>%
  fiscal_to_calendar() 



grants_second_draw <-
  tibble(transportation = 29,
         education_second_draw = 82,
         other_healthcare =  12,
         other_spending = 54,
         funds_second_draw =  
           transportation + education_second_draw + other_healthcare + other_spending) %>%
  slice(rep(1:n(), each = 12)) %>%
  mutate(date = seq(yearquarter('2021 Q1'), length.out = 12,  by = 1)) %>%
  relocate(date, .before =  everything()) %>%
  mutate(across(where(is.numeric),
                ~  annualize(.x) / 12))

ppp_end <- yearquarter('2021 Q3')
aviation_end <- yearquarter('2021 Q4')
employee_retention_end <- yearquarter('2022 Q1')
business_meals_end <- yearquarter('2023 Q1')
subsidies_second_draw <- 
  tibble(ppp = 325,
         aviation = 16,
         employee_retention = 20,
         business_meals = 5) %>%
  slice(rep(1:n(), each = 8)) %>%
  mutate(date = seq(yearquarter('2021 Q1'), length.out = 8, by = 1),
         across(where(is.numeric), ~ annualize(.x)),
         ppp = if_else(date < ppp_end,
                       ppp / 2,
                       0),
         aviation = if_else(date < aviation_end,
                            aviation / 3, 
                            0),
         employee_retention = if_else(date < employee_retention_end,
                                      employee_retention / 4, 
                                      0),
         business_meals = if_else(date < business_meals_end,
                                  business_meals / 8,
                                  0),
         other_subsidies = c(rep(25.42, 4), rep(0, 4)),
         subsidies_second_draw = ppp + aviation + employee_retention + business_meals) 


tar_load(projections)

grants <- c('education_stabilization_fund', 'provider_relief_fund')


  projections %<>%
  as_tsibble(key = id, index = date) %>%
  filter_index('2016 Q2' ~ .) %>%
  mutate(across(c('education_stabilization_fund', 'provider_relief_fund'),
                ~if_else(id == 'projection', 
                        NA_real_,
                        .))) %>%
  coalesce_join(df, by = 'date') %>%
  rename(coronavirus_relief_fund_total  = coronavirus_relief_fund)  %>%
  mutate(nonprofit_provider_relief_fund = share_nonprofit_hospitals  * provider_relief_fund,
         coronavirus_relief_fund  =  mpc_covid_relief_fund(coronavirus_relief_fund_total),
         funds_first_draw = education_stabilization_fund + nonprofit_provider_relief_fund +  coronavirus_relief_fund) %>%
  filter_index('2020 Q1' ~ .) %>%
 left_join(grants_second_draw) %>%
  mutate(funds_second_draw =  coalesce(funds_second_draw, 0),
         funds = funds_first_draw + funds_second_draw,
         consumption_grants_adj = consumption_grants - coronavirus_relief_fund_total - nonprofit_provider_relief_fund - education_stabilization_fund,
         consumption_grants_adj = if_else(id == 'projection',
                                          lag(consumption_grants_adj) * (1 + federal_purchases_growth_cumulative),
                                          consumption_grants_adj),
         consumption_grants = consumption_grants_adj + funds)

  subsidies <- c('ppp', 'employee_retention', 'aviation')
projections %>%
  mutate(across(all_of(subsidies), 
         ~ if_else(id == 'projection', NA_real_, .x))) %>%
  coalesce_join(subsidies_second_draw, 'date') %>%
  mutate(across(c('subsidies_second_draw', 'other_subsidies'),
                ~ coalesce(.x, 0)), 
          subsidies = subsidies + subsidies_second_draw + other_subsidies)


# Quarterly -------------------------------------------------------------------------------------------------------
#haver.path("//ESDATA01/DLX/DATA/")
# BEA NIPAs
names_usna <- read_excel("data/auxilliary/spreadsheet_names.xlsx")
usna <- read_xlsx('data/raw/haver/national_accounts.xlsx') %>%
  filter(date >= as_date('2015-12-31')) %>%
  rename_haver_codes() %>%
  mutate(
    across(where(is.numeric), ~ replace_na(.x, 0)), 
    date = yearquarter(date),
    # SUBSIDIES
    other_subsidies  =  aviation + employee_retention + sick_leave,
    legislation_subsidies = ppp + other_subsidies, 
    nonprofit_subsidies = nonprofit_ppp +  nonprofit_provider_relief,
    # GRANTS
    legislation_grants = covid_relief_fund + education_stabilization_fund + provider_relief_fund,
    medicaid_grants = medicaid_grants / 1000, # MILLIONS TO BILLIONS
    non_medicaid_grants = total_grants - medicaid_grants,
    non_medicaid_or_legislation_grants = non_medicaid_grants - legislation_grants,
    other_grants = legislation_grants, # PLACEHOLDER
    federal_cgrants = non_medicaid_or_legislation_grants + other_grants,
    # HEALTH
    federal_health  = medicaid_grants + medicare,
    state_health = medicaid_total  - medicaid_grants,
    # SOCIAL BENEFITS
    
    # UNEMPLOYMENT INSURANCE
    ui = ui_bea + wages_lost_assistance,
    ui_cbo_assumed_other = 
      case_when(
        date < yearquarter('2020 Q2') ~ 0,
        date >=  yearquarter('2020 Q2') & date <=  yearquarter('2020 Q3') ~ 12,
        date >=  yearquarter('2020 Q4') & date <= yearquarter('2021 Q1') ~ 8,
        date >= yearquarter('2021 Q2') ~ 4
      ),
    federal_ui = 2 * peuc + pua + puc + wages_lost_assistance + ui_cbo_assumed_other,
    state_ui = ui - federal_ui,
    # FEDERAL SOCIAL BENEFITS
    federal_social_benefits = federal_social_benefits_nipa - medicare - state_ui,
    state_social_benefits = state_social_benefits + state_ui - medicaid_grants 
  ) 


# Forecast ------------------------------------------------------------------------------------


# Consumption Grants --------------------------------------------------------------------------



education_total_disbursed <-
  projections %>%
  totals('education_stabilization_fund')

remainder <- 
  deannualize(totals(df, 'education_stabilization_fund')) - education_total_disbursed

(x <- mpc_education_stabilization_fund(remainder, n = 4))

df <-
  tibble(
    date = seq(yearquarter('2021 Q1'), length.out = 4, by = 1),
    x = x
  )
usna %<>%
  as_tsibble(index = date) %>%
  tsibble::append_row(n = 12) %>%
  left_join(projections %>% mutate(date = yearquarter(date)) %>%
              select(date, gfeg, gfeghdx)) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))

usna %>%
  mutate(education_stabilization_fund = 
           case_when(
             date < yearquarter('2021 Q1') ~ education_stabilization_fund,
             date >= yearquarter('2021 Q1') & date <= yearquarter('2022 Q2') ~ education_total_remaining 
           ))

# State Health --------------------------------------------------------------------------------


cbo_state_health <-
  tibble(
    fy = as.numeric(c(2020:2022)),
    cbo_state_health = c(657,	726,	727),
    growth_rate = ( cbo_state_health / lag(cbo_state_health))
  )

cbo_state_health_growth <-   
  cbo_state_health %>%
  filter(fy == 2021) %>%
  pull(growth_rate)

forecasts <- 
  tibble(
    date = seq(yearquarter("2021 Q1"), length.out = 12, by = 1), # by 1 quarter
    growth = case_when(
      date == yearquarter('2021 Q1') ~ cbo_state_health_growth^0.18,
      date == yearquarter('2021 Q2') ~ cbo_state_health_growth^0.1,
      date >= yearquarter('2021 Q3') ~ NaN
    )
  ) %>%
  mutate(growth = zoo::na.locf(growth))


# Total Medicaid ------------------------------------------------------------------------------


usna %>% 
  full_join(forecasts, by = 'date') %>%
  mutate(
    medicaid_total = if_else(
      date < yearquarter('2021 Q1'),
      medicaid_total,
      lag(medicaid_total) * growth
    ),
    medicaid_total = if_else(
      date == yearquarter('2021 Q2'),
      lag(medicaid_total) * growth,
      medicaid_total
    ),
    medicaid_total = zoo::na.locf(medicaid_total),
    medicaid_grants = case_when(date < yearquarter('2021 Q1') ~ medicaid_grants,
                                date >= yearquarter('2021 Q1') & date <= yearquarter('2022 Q1') ~ medicaid_total * 0.74,
                                date > yearquarter('2021 Q1') ~ medicaid_total * 0.68),
    state_health_outlays = medicaid_total - medicaid_grants,
    federal_health_outlays = medicaid_grants + medicare
  ) 




# Misc ----------------------------------------------------------------------------------------



# fim_state_social_benefits = (nipa - medicare - ui - rebate_checks - nonprofit_subsidies) + rebate_checks + nonprofit_subsidies + federal_ui
#                           = nipa - medicare + (federal_ui - ui) 
#                           = nipa - medicare - state_ui

# Federal health
# State health
# Subsidies
# Grants
# Basically, to get the FIM social benefits we subtract medicare from NIPA social benefits
# and then we add the  difference between their ui and ours 
# 
# fim ex everything = social benefits - 
