# Setup -------------------------------------------------------------------
Sys.setenv(TZ = 'UTC')
librarian::shelf(tidyverse, tsibble, lubridate, glue, dplyover, zoo, TTR, fs, gt, openxlsx, snakecase, rlang)
devtools::load_all()

options(digits = 4) # Limit number of digits
options(scipen = 20)# Turn off scientific notation under 20 digits 

# Set dates for current and previous months
month_year <- glue('{format.Date(today() - 7, "%m")}-{year(today())}')
last_month_year <- glue('{month(today() - 7 -months(1))}-{year(today() - months(1) - weeks(1))}')

# Create update folders

update_in_progress <- TRUE

if(update_in_progress == TRUE){
  dir_create(glue('results/{month_year}')) # Create folder to store results
  dir_create(glue('results/{month_year}/input_data')) # Folder to store forecast from current update
  file_copy(path = 'data/forecast.xlsx', new_path = glue('results/{month_year}/input_data/forecast_{month_year}.xlsx'), overwrite = TRUE)
}


# Wrangle data ------------------------------------------------------------

# Since BEA put all CARES act grants to S&L in Q2 2020 we need to
# override the historical data and spread it out based on our best guess
# for when the money was spent.
overrides <- readxl::read_xlsx('data/forecast.xlsx',
                               sheet = 'historical overrides') %>% # Read in overrides
  select(-name) %>% # Remove longer name since we don't need it
  pivot_longer(-variable,
               names_to = 'date') %>% # Reshape so that variables are columns and dates are rows
  pivot_wider(names_from = 'variable',
              values_from = 'value') %>% 
  mutate(date = yearquarter(date))

current_quarter <- overrides %>% slice_max(date) %>% pull(date) # Save current quarter for later

# Load national accounts data from BEA
usna <-
  read_data() %>% # Load raw BEA data from Haver and CBO projections
  define_variables() %>%  # Rename Haver codes for clarity
  as_tsibble(key = id, index = date) %>% # Specify time series structure
  mutate_where(id == 'historical',  # Calculate GDP growth for data but take CBO for projection
               real_potential_gdp_growth = q_g(real_potential_gdp)) %>% 
  mutate( 
    # Net out unemployment insurance, rebate checks, and Medicare to apply different MPC's
    federal_social_benefits_gross = federal_social_benefits, # Save original value
    federal_social_benefits = federal_social_benefits - ui - rebate_checks - medicare - nonprofit_provider_relief_fund,
    state_social_benefits = state_social_benefits - medicaid,
    social_benefits = federal_social_benefits + state_social_benefits,
    consumption_grants = gross_consumption_grants - medicaid_grants,
  ) %>% 
  mutate(rebate_checks_arp = if_else(date == yearquarter("2021 Q1"),
                                     1348.1,
                                     0)) %>%
  mutate_where(id == 'projection',
               rebate_checks_arp = NA,
               federal_ui = NA,
               state_ui = NA) %>% 
  mutate_where(date == yearquarter('2021 Q1'),
               rebate_checks = rebate_checks - rebate_checks_arp,
               federal_social_benefits = federal_social_benefits + 203
  ) %>% 
  mutate_where(date == yearquarter("2021 Q4"),
               rebate_checks_arp = 14.2,
               rebate_checks = 0) %>% 

  mutate(consumption_grants = gross_consumption_grants - medicaid_grants,
         
         # Aggregate taxes
         corporate_taxes = federal_corporate_taxes + state_corporate_taxes,
         non_corporate_taxes = federal_non_corporate_taxes + state_non_corporate_taxes) %>% 
  mutate_where(id == 'projection',
               consumption_grants_deflator_growth = state_purchases_deflator_growth,
               investment_grants_deflator_growth = state_purchases_deflator_growth) %>% 
  mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
               consumption_grants = overrides$consumption_grants_override) 
# Forecast ----------------------------------------------------------------
forecast <- # Read in sheet with our forecasted values
  readxl::read_xlsx('data/forecast.xlsx',
                    sheet = 'forecast') %>% 
  select(-name) %>% 
  pivot_longer(-variable,
               names_to = 'date') %>% 
  pivot_wider(names_from = 'variable',
              values_from = 'value') %>% 
  mutate(date = yearquarter(date))


projections <- # Merge forecast w BEA + CBO
  coalesce_join(usna, forecast, by = 'date') %>% 
  mutate(# Coalesce NA's to 0
    across(where(is.numeric),
           ~ coalesce(.x, 0))) %>%
  mutate(
    health_outlays = medicare + medicaid,
    federal_health_outlays = medicare + medicaid_grants,
    state_health_outlays = medicaid - medicaid_grants
  ) %>% 
  mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
               federal_other_direct_aid_arp = overrides$federal_other_direct_aid_arp_override,
               federal_other_vulnerable_arp = overrides$federal_other_vulnerable_arp_override,
               federal_social_benefits = overrides$federal_social_benefits_override,
               federal_aid_to_small_businesses_arp = overrides$federal_aid_to_small_businesses_arp_override) %>% 
  mutate_where(date == yearquarter("2021 Q1"),
               federal_social_benefits = federal_social_benefits + 203) %>% 
  # FIXME: Figure out why wrong number was pulled from Haver (like 400)
  mutate_where(date == yearquarter('2021 Q4'),
               federal_ui = 11, 
               state_ui = ui - federal_ui)

# Consumption -------------------------------------------------------------

consumption <- # Compute consumption out of transfers (apply MPC's)
  projections %>%
  taxes_transfers_minus_neutral() %>%
  calculate_mpc("social_benefits") %>%
  mutate(rebate_checks_post_mpc = mpc_rebate_checks(rebate_checks_minus_neutral)) %>%
  calculate_mpc("subsidies") %>%
  calculate_mpc("health_outlays") %>%
  calculate_mpc("corporate_taxes") %>%
  calculate_mpc("non_corporate_taxes") %>% 
  mutate(across(c(federal_ui_minus_neutral, state_ui_minus_neutral),
                .fns = ~ if_else(date < yearquarter("2021 Q2"),
                                 mpc_ui(.x),
                                 mpc_ui_arp(.x)),
                .names = '{.col}_post_mpc')) %>% 
  
  mutate(across(
    .cols = all_of(
      c(
        "rebate_checks_arp",
        "federal_other_direct_aid_arp",
        "federal_other_vulnerable_arp",
        # "federal_ui_arp",
        #"state_ui_arp",
        "federal_aid_to_small_businesses_arp"
      )
    ),
    .fns = ~ .x - dplyr::lag(.x, default = 0) * (1 + real_potential_gdp_growth + consumption_deflator_growth),
    .names = "{.col}_minus_neutral"
  )) %>% 
  mutate(
    across(
      .cols = any_of(
        c("federal_ui_arp", "state_ui_arp", "federal_other_vulnerable_arp") %>% paste0("_minus_neutral")
      ),
      .fns = ~ mpc_vulnerable_arp(.x),
      .names = "{.col}_post_mpc"
    ),
    across(
      .cols = all_of(
        c("rebate_checks_arp", "federal_other_direct_aid_arp") %>% paste0("_minus_neutral")
      ),
      .fns = ~ mpc_direct_aid_arp(.),
      .names = "{.col}_post_mpc"
    ),
    
    federal_aid_to_small_businesses_arp_minus_neutral_post_mpc = mpc_small_businesses_arp((federal_aid_to_small_businesses_arp_minus_neutral))
  )

# Contribution ------------------------------------------------------------

contributions <- # Calculate contributions
  consumption %>%
  purchases_contributions() %>% 
  mutate(across(ends_with("post_mpc"),
                ~ 400 * .x / lag(gdp),
                .names = "{.col}_contribution"
  )) %>%
  rename_with(~ str_replace(.x, "_minus_neutral_post_mpc_contribution", "_contribution")) %>% 
  rename_with(~ str_replace(.x, "minus_neutral_post_mpc", "post_mpc")) %>% 
  rename_with(~ str_replace(.x, "post_mpc_contribution", "contribution")) %>% 
  sum_transfers_contributions() %>% 
  
  mutate(
    grants_contribution = consumption_grants_contribution + investment_grants_contribution,
    federal_contribution = federal_purchases_contribution + grants_contribution,
    state_contribution = state_purchases_contribution - grants_contribution
  ) %>%
  mutate(social_benefits_contribution = federal_social_benefits_contribution + state_social_benefits_contribution) %>%
  mutate(non_corporate_taxes_contribution = federal_non_corporate_taxes_contribution + state_non_corporate_taxes_contribution) %>%
  mutate(taxes_contribution = non_corporate_taxes_contribution + corporate_taxes_contribution) %>%
  mutate(
    transfers_contribution = federal_social_benefits_contribution + state_social_benefits_contribution +
      rebate_checks_contribution + rebate_checks_arp_contribution + federal_ui_contribution + state_ui_contribution +
      federal_subsidies_contribution + federal_aid_to_small_businesses_arp_contribution +  state_subsidies_contribution + federal_health_outlays_contribution +
      state_health_outlays_contribution + federal_other_direct_aid_arp_contribution + federal_other_vulnerable_arp_contribution,
    taxes_contribution = federal_non_corporate_taxes_contribution + state_non_corporate_taxes_contribution +
      federal_corporate_taxes_contribution + state_corporate_taxes_contribution
  ) %>%
  mutate(subsidies = federal_subsidies + state_subsidies,
         subsidies_contribution = federal_subsidies_contribution + state_subsidies_contribution) %>% 
  #sum_taxes_contributions() %>%
  get_fiscal_impact()


openxlsx::write.xlsx(contributions, file = glue('results/{month_year}/fim-{month_year}.xlsx'), overwrite = TRUE)
write_rds(contributions, file = 'data/contributions.rds')
usethis::use_data(contributions, overwrite = TRUE)


# Web materials  -------------------------------------------------------------

# Interactive data
interactive <- 
  contributions %>% 
  filter_index('1999 Q4' ~ as.character(current_quarter + 8)) %>% 
  mutate(consumption = transfers_contribution + taxes_contribution,
         recession = recode(recession, `-1` = 0),
         recession = replace_na(recession, 0),
         id = recode(id, 
                     historical = 0,
                     projection = 1)) %>% 
  select(date, 
         impact = fiscal_impact_moving_average,
         recession,
         total = fiscal_impact,
         federal = federal_contribution,
         state_local = state_contribution,
         consumption,
         projection = id) %>% 
  separate(date, c('year', 'quarter'))

readr::write_csv(interactive,  file = glue('results/{month_year}/interactive-{month_year}.csv'))

# Figures for website
rmarkdown::render('Fiscal-Impact.Rmd',
                  output_file = 'Fiscal-Impact.pdf',
                  clean = TRUE,
                  params = list(start = yearquarter('1999 Q4'), end = current_quarter + 8))

file_copy(path = 'Fiscal-Impact.pdf',
          new_path = glue('results/{month_year}/Fiscal-Impact-{month_year}.pdf'),
          overwrite = TRUE)

# Comparison ------------------------------------------------------------

source('scripts/revision_figures.R')
source('scripts/revision_table.R')
source('scripts/revision_deflators.R')

rmarkdown::render(input = 'index.Rmd',
                  output_file = glue('results/{month_year}/update-comparison-{month_year}'),
                  clean = TRUE)

file_copy(
  path = glue('results/{month_year}/update-comparison-{month_year}.html'),
  new_path = 'index.html',
  overwrite = TRUE
)


# State and local employment ------------------------------------------------------------------


# Template code for figuring out state and local employment decline from FRED
calculate_employment_change <- function(state, local){
  state_feb_2020 = 5303
  local_feb_2020 = 14669
  
  feb_2020 = state_feb_2020 + local_feb_2020
  
  current = state + local
  
  change <- (current / feb_2020) - 1
  
  return(change)
}

# Local govt employment: https://fred.stlouisfed.org/series/CES9093000001
# State govt employment: https://fred.stlouisfed.org/series/CES9092000001
calculate_employment_change(state = 5032, local = 13996)

