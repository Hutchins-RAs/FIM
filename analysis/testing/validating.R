librarian::shelf('tidyverse', 'tsibble', 'readxl', 'arsenal')

df <-
  read_xlsx('inst/extdata/fim-march.xlsx') %>% 
  mutate(date = yearquarter(date)) %>% 
  as_tsibble(index = date) %>% 
  rename(consumption_grants = federal_cgrants,
         ui = unemployment_insurance,
         ui_post_mpc = unemployment_insurance_post_mpc,
         ui_contribution_df = unemployment_insurance_cont) %>% 
  mutate(social_benefits2 = social_benefits - ui - rebate_checks,
         federal_social_benefits2 = federal_social_benefits - federal_unemployment_insurance_override - rebate_checks) %>% 
  filter_index('2020 Q4' ~ '2022 Q1') 

compare <- function( variable_df1, variable_df2){
  fim  %>% 
    filter_index('2020 Q4' ~ '2022 Q1') %>% 
    select(date, id, {{variable_df1}}) %>% 
    left_join(df %>% select({{variable_df2}})) %>% 
    mutate(diff  =  round({{variable_df1}} - {{ variable_df2 }}))
}

tar_load(fim)
fim_sum <- fim %>% filter_index('2020 Q4' ~ '2021 Q4')
# Contributions -------------------------------------------------------------------------------

compare(gdp, gdp)

compare(real_potential_gdp_growth, gdppoth) 

compare(federal_purchases_deflator_growth,pi_federal )

compare(state_purchases_deflator_growth,pi_state_local)
## Total
compare(fiscal_impact, fim_bars)

## Purchases
compare(federal_purchases, federal_nom)
compare(federal_purchases_contribution, federal_nom_cont)
##  Taxes
## Transfers
compare(social_benefits_contribution, social_benefits_cont)
compare(social_benefits, social_benefits2)
  
compare(federal_social_benefits, federal_social_benefits2)
compare(social_benefits, social_benefits2)


compare(taxes_contribution, taxes_cont)
compare(corporate_taxes_contribution, corporate_taxes_cont)
compare(non_corporate_taxes_contribution, noncorp_taxes_cont)

compare(federal_corporate_taxes, federal_corporate_taxes)
df$federal_corporate_taxes
compare(federal_non_corporate_taxes,  federal_noncorp_taxes)
compare(non_corporate_taxes_post_mpc, noncorp_taxes_post_mpc)


projections %>% add_factors() %>%
  get_overrides() %>%
  mutate(grants = consumption_grants + investment_grants,
         federal_purchases_deflator_growth = q_g(federal_purchases_deflator),
         state_purchases_deflator_growth = q_g(state_purchases_deflator),
         consumption_grants_deflator_growth = q_g(consumption_grants_deflator),
         investment_grants_deflator_growth = q_g(investment_grants_deflator),
         consumption_deflator_growth  =  q_g(consumption_deflator),
         real_potential_gdp_growth = q_g(real_potential_gdp)) %>% 
  mutate(x = federal_non_corporate_taxes - dplyr::lag(federal_non_corporate_taxes) * (1 + real_potential_gdp_growth + consumption_deflator_growth)) %>% 
  filter(id == 'projection') %>% 
  select(real_potential_gdp_growth, consumption_deflator_growth, federal_non_corporate_taxes, x) 


### PURCHASES ###
# PASS
compare(federal_purchases_contribution, federal_cont_ex_grants)
# PASS
compare(state_purchases_contribution, state_local_cont_ex_grants)
# FAIL
compare(grants_contribution, federal_grants_cont)

### TAXES ##
fim_simp <- fim %>% 
  filter_index('2021 Q1' ~ '2022 Q1')
cbind(fim_simp$taxes_contribution , df$taxes_cont)
# Correct
compare(non_corporate_taxes_contribution, noncorp_taxes_cont)

# Slightly off
compare(corporate_taxes_contribution, corporate_taxes_cont)


### TRANSFERS ###

# Pass
compare(health_outlays_contribution, health_outlays_cont)
compare(federal_health_outlays_contribution, federal_health_outlays_cont)
compare(state_health_outlays_contribution, state_health_outlays_cont)


# Different
compare(social_benefits_contribution, social_benefits_cont)
compare(federal_social_benefits_contribution, federal_social_benefits_cont)
compare(state_social_benefits_contribution, state_social_benefits_cont)

fim_simp <- fim %>% filter_index('2021 Q1' ~ '2022 Q1')
cbind(fim_simp$federal_social_benefits, df$federal_social_benefits)

# Good
compare(ui_contribution, ui_contribution_df)
# Good
compare(rebate_checks_contribution, rebate_checks_cont)
# Good
compare(subsidies_contribution, subsidies_cont)
compare(fiscal_impact, fim_bars)


# Rinse ---------------------------------------------------------------------------------------
compare <- function(variable_df1, variable_df2){
  projections  %>% 
    filter_index('2021 Q1' ~ '2022 Q1') %>% 
    select(date, id, {{variable_df1}}) %>% 
    left_join(df %>% select({{variable_df2}})) %>% 
    mutate(diff  =  round({{variable_df1}} - {{ variable_df2 }}))
}


# Still need to fix: grants, health outlays, taxes, social benefits

tar_load(projections)


# Health outlays are way lower
# Fail
compare(
        health_outlays,
        health_outlays)
df$health_outlays

# Taxes
# Fail (Lower)

  compare(corporate_taxes, corporate_taxes)
df$corporate_taxes


## ROOT OF PROBLEM
compare(federal_corporate_taxes, federal_corporate_taxes)
df$federal_corporate_taxes

compare(federal_corporate_taxes_growth, federal_corporate_taxes_g)

# PASS
compare(state_corporate_taxes, state_corporate_taxes)
df$state_corporate_taxes

proj <- read_csv('inst/extdata/projections-3-2021.csv') %>% 
  mutate(date = yearquarter(date)) %>% 
  as_tsibble(index = date) %>% 
  filter_index('2020 Q3' ~ '2022 Q1') %>% 
  select(-X1)

fim_proj <- fim %>% 
  filter_index('2020 Q3' ~ '2022 Q1')
cbind(fim_proj$federal_corporate_taxes, proj$gfrcp)


# Fail (Extremely lower)
compare(non_corporate_taxes, noncorp_taxes)
compare(federal_non_corporate_taxes, federal_noncorp_taxes)
compare(state_non_corporate_taxes, state_noncorp_taxes)
df$noncorp_taxes
