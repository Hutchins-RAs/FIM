
# Unexported mpc scratch functions ----------------------------------------
librarian::shelf(tidyverse, rlang)


timing <- read_excel("data/forecast.xlsx", 
                       sheet = "mpc", range = "B2:N14") %>% 
 pivot_longer(-variable,
              names_to = 'lag') %>% 
  pivot_wider(names_from = variable,
              values_from = value) %>% 
  select(-lag)

contributions <- readr::read_rds('data/contributions.rds')

mpc_with_data <- function(data, field){
  field <- enquo(field)
  string <- as_label(field)
  
  args <- switch(as_label(field),
                 # TRANSFERS
                 
                 health_outlays = timing$health_outlays,
                 federal_health_outlays = ,
                 state_health_outlays = ,
                 
                 social_benefits =  timing$social_benefits,
                 federal_social_benefits = ,
                 state_social_benefits = ,
                 
                 
                 subsidies = timing$subsidies,
                 federal_subsidies = ,
                 state_subsidies = ,
                 
                 
                 ui = timing$ui,
                 federal_ui = ,
                 state_ui = ,
                 
                 ui_arp = timing$ui_arp,
                 federal_ui_arp = ,
                 state_ui_arp = ,
                 
                 rebate_checks = timing$rebate_checks,
                 rebate_checks_arp = timing$rebate_checks_arp,
                 
                 corporate_taxes = timing$corporate_taxes,
                 federal_corporate_taxes = ,
                 state_corporate_taxes = ,
                 
                 non_corporate_taxes = timing$non_corporate_taxes,
                 federal_non_corporate_taxes = ,
                 state_non_corporate_taxes = ,
                 
                 federal_aid_to_small_businesses_arp = timing$aid_to_small_businesses_arp,
                 federal_other_direct_aid_arp = timing$other_direct_aid_arp,
                 federal_other_vulnerable_arp = timing$other_vulnerable_arp,)
  
  function_call <- expr(roll::roll_sum({{field}}, weights = rev(args), width = length(weights), 
                                       online = FALSE,
                                       min_obs = 1))
  eval_tidy(expr = function_call, data = data)
  #return(function_call)
}

mpc_with_data(contributions,
              federal_ui)
mpc <- function(data, field){
  field <- enquo(field)
  string <- as_label(field)
 
  args <- switch(as_label(field),
                 # TRANSFERS
                 
                 
                 health_outlays = rep(0.9 * 0.25, 4),
                 social_benefits =  rep(0.9 * 0.25, 4),
                 federal_subsidies = ,
                 state_subsidies = ,
                 subsidies = 0.45 * c(0.11, 0.095, 0.09, 0.085, 0.08, 0.8, 
                                           0.08, 0.08, 0.075, 0.075, 0.075, 0.075),
                 federal_ui = ,
                 state_ui = ,
                 ui = 0.9 * c(0.35, 0.35, 0.1, 0.1, 0.05, 0.05),
                 
                 federal_ui_arp = ,
                 state_ui_arp = ,
                 ui_arp = c(0.253, 0.253, 0.161, 0.161, 0.075, 0.05, 0.025, 0.022),
                 rebate_checks = 0.7 * c(0.35, 0.15, rep(0.08, 6)),
                 
                 
                 
                 # TAXES
                 corporate_taxes = -0.4 * (rep(1/12, 12)),
                 non_corporate_taxes = -0.6 * c(rep(0.2, 2), rep(0.1, 6)))
  
  function_call <- expr(roll::roll_sum({{field}}, weights = rev(args), width = length(weights), 
                                       online = FALSE,
                                       min_obs = 1))
  # function_call$weights <- rev(timing(field))
  # function_call$width <- length(timing(field))
   eval_tidy(expr = function_call, data = data)
  #return(function_call)
}
col_list <- c('ui', 'subsidies')

for(col in col_list){
  mpc_with_data(contributions, as_label(col))
}

df %>% 
  as_tibble() %>% 
  mutate(across(c(social_benefits, subsidies, state_ui),
                ~ mpc(df, .x),
                .names = '{.col}_mpc')) %>% 
  select(date, social_benefits, subsidies,
         ends_with('mpc'))
  summarise(date, social_benefits, mpc = mpc_timing(., social_benefits))
mpc_timing(subsidies)
df %>% 
  mutate(mpc = roll::roll_sum("subsidies", width = 12, weights = mpc_timing("subsidies")))
