## CBO was estimating 962 total UI in 2021
library(safejoin)
crrca <-
  readxl::read_xlsx('inst/extdata/crrca.xlsx', sheet = 'ui') %>%
  as_tsibble(index  = date) %>%
  annual_to_quarter() %>%
  mutate(
    other_ui = trade_adjustment_assistance + extended_benefits,
    federal_ui_timing = c(0.725, 0.275, rep(0, 4 * 11 - 2))
  ) %>%
  summarize(federal_ui = 4 * (2 * peuc +  pua   + puc +  other_ui) * federal_ui_timing,
            state_ui) %>%
  left_join(
    readxl::read_xlsx('inst/extdata/crrca.xlsx', sheet = 'other social benefits') %>%
      mutate(date = yearquarter(date))
  ) %>%
  summarize(
    date,
    federal_ui,
    state_ui,
    other_federal_social_benefits = nonprofit_ppp + nonprofit_provider_relief + snap + eitc
  )




df <- read_data() %>%
  coalesce_join(projections, by = 'date') %>%
  
  define_variables() %>%
  
  create_override(
    var = state_purchases_growth,
    start = yearquarter('2020 Q4'),
    end = yearquarter('2022 Q1'),
    values = c(rep(0.0025, 3), 0.005, 0.0075, 0.01)
  )  %>%
  create_override(
    var = federal_social_benefits_growth,
    start = yearquarter('2021 Q1'),
    end = yearquarter('2022 Q3'),
    values = c(rep(-0.0075, 3), rep(0.015, 4))
  ) %>%
  growth_assumptions() %>%
  
  mutate(
    other_federal_social_benefits = nonprofit_ppp + nonprofit_provider_relief_fund + snap + eitc,
    
    consumption_grants = gross_consumption_grants - medicaid_grants,
    grants = consumption_grants + investment_grants,
    federal_ui = pua + 2 * peuc + puc + wages_lost_assistance,
    state_ui = ui - federal_ui,
    federal_social_benefits_aggregate = federal_social_benefits,
    federal_social_benefits = federal_social_benefits - federal_ui - medicare - rebate_checks - other_federal_social_benefits ,
    state_social_benefits = state_social_benefits - medicaid - state_ui
  ) %>%
  as_tsibble(index  = date) %>%
  forecast()

#reallocations() %>%
# Override CBO Growth Rate for Federal Social Benefits
df2 <-
  df %>%
  as_tibble() %>%
  mutate(date = as.character(date)) %>%
  safejoin::safe_left_join(
    crrca %>% as_tibble() %>%
      mutate(date = as.character(date)),
    by = 'date',
    conflict = 'patch'
  ) %>%
  mutate(
    federal_social_benefits = federal_social_benefits +  other_federal_social_benefits  + federal_ui + rebate_checks
  )
