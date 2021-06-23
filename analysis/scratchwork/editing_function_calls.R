
# Unexported mpc scratch functions ----------------------------------------

library('tidyverse')
library('tsibble')
library('rlang')
df %>% 
  summarise(across(mpc_timing(health_outlays)))
map2_dbl(fim, names(fim %>% select(social_benefits,  subsidies)), ~mpc_call(.x, .y))

mpc_gen <-function(data, ...){
  data <- data %>% select(...)
  purrr::map_dbl(names(data), mpc_call)
}
mpc2(fim, "subsidies")
sub <- fim %>% 
  select(social_benefits,subsidies)

mpc2(fim, field = 'social_benefits')



fim %>% 
  summarise(subsidies, mpc_subsidies(subsidies))
timing <- function(x){
  switch(x, 
         health_outlays = rep(0.9 * 0.25, 4),
         social_benefits =  rep(0.9 * 0.25, 4),
         subsidies = 0.45 * c(0.11, 0.095, 0.09, 0.085, 0.08, 0.8, 
                              0.08, 0.08, 0.075, 0.075, 0.075, 0.075))
}

timing2 <- function(field){
  field <- enquo(field)
  string <- rlang::as_label(field)
  
  args <- switch(string, 
                 health_outlays = rep(0.9 * 0.25, 4),
                 social_benefits =  rep(0.9 * 0.25, 4),
                 subsidies = 0.45 * c(0.11, 0.095, 0.09, 0.085, 0.08, 0.8, 
                                      0.08, 0.08, 0.075, 0.075, 0.075, 0.075))
  return(args)
}

timing2(social_benefits)
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
