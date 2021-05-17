#' Get dollars of consumptions
#'
#' @param mpc numeric
#' @param timing numeric
#' @name consumption
NULL
#> NULL


#' @rdname consumption 
mpc  <- function(mpc =  1, timing){
  rlang::new_function(
    rlang::exprs(x = ),
    rlang::expr({
      !!mpc  * roll::roll_sum(x, width = length(!!timing), weights = rev(!!timing),
                              online = FALSE, min_obs = 1)
    }),
    rlang::caller_env()
  )
}

#' Marginal Propensity to Consume Benefits in American Rescue Plan for Vulnerable Households
#' Includes UI, Snap, Housing assistance, TANIF, WIC, Cobra Subsidies, etc
#' We assume a cumulative MPC of 0.9. Our timing assumption is that 14% goes out in Q1 and Q2 respectively, 20% in Q3 and Q4 respectively, 9% in Q5, 5% in Q6 and Q7, and 4% in Q8
#' @param x 
#'
#' @return
#' @export
#'
#' @examples

mpc_vulnerable_arp <- mpc(0.45, timing = c(rep(0.14, 2), rep(0.2, 2), 0.09, rep(0.05, 2), 0.04)) 


mpc_direct_aid_arp <- mpc(timing = c(0.18, rep(0.09, 2), rep(0.05, 7), 0.03))
# 
# mpc_direct_aid_arp <- function(x){
#   
#   mpc <- 1
#   weights <- c(rep(1/8, 8)) 
#   mpc * roll::roll_sum(x, width = length(weights), weights = rev(weights), 
#                        online = FALSE, min_obs = 1)
#   
# }
mpc_small_businesses_arp<- function(x){
  
  mpc <- 1
  weights <- c(rep(0.04, 2), rep(0.017, 10))
  mpc * roll::roll_sum(x, width = length(weights), weights = rev(weights), 
                       online = FALSE, min_obs = 1)
  
}

# mpc_small_businesses_arp <- function(x){
#   
#   mpc <- 1
#   weights <- c(rep(1/8, 8))
#   mpc * roll::roll_sum(x, width = length(weights), weights = rev(weights), 
#                        online = FALSE, min_obs = 1)
#   
# }




mpc_ui_arp <- function(x){
  
  mpc <- 1 
  weights <- c(rep(0.253, 2), rep(0.161, 2), 0.075, 0.05, 0.025, 0.022)
  mpc * roll::roll_sum(x, width = length(weights), weights = rev(weights), 
                       online = FALSE)
  
}

#' Non-health grants to S&L governments in the American Rescue Plan 
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
mpc_arp_non_health_grants<- function(df){
  mpc <- 1
  weights <- c(rep(0.07, 2),
               rep(0.049, 10),
               rep(0.0475, 7),
               0.0375)
  
  df %>% 
    mutate(non_health_grants_post_mpc = mpc * roll::roll_sum(non_health_grants, width = length(weights), weights = rev(weights), online = FALSE, min_obs = 1))
  
}
#' @rdname consumption
mpc_social_benefits <- mpc(0.9, timing = rep(1/4, 4))

#' @rdname consumption
mpc_coronavirus_relief_fund <- mpc(timing = c(0.06, 0.08, rep(0.1, 2), rep(0.08, 8)))

#' @rdname consumption
mpc_rebate_checks <- mpc(0.7, c(0.35, 0.15, rep(0.08, 6)))

#' @rdname consumption
mpc_health_outlays <- mpc(0.9, rep(1/4, 4))

#' @rdname consumption 
mpc_corporate_taxes <- mpc(-0.4, rep(1/12, 12))

#' @rdname consumption 
mpc_non_corporate_taxes <- mpc(-0.6, c(rep(0.2, 2), rep(0.1, 6)))

#' @rdname consumption 
mpc_subsidies <- mpc(0.45, timing = c(0.11, 0.095, 0.09, 0.085, rep(0.08, 4), rep(0.075, 4)))

#' @rdname consumption 
mpc_subsidies_second_draw <- function(x){
  mpc <- 0.525
  weights <- c(rep(0.0625, 4), rep(0.0750, 4), 0.0875, 0.1, 0.1125, 0.15)
  mpc * roll::roll_sum(x, width = length(weights),
                       weights = weights, online = FALSE)
}

#' @rdname consumption 
mpc_subsidies_rra <- mpc(0.525, timing = c(0.1125, 0.1, 0.0875, rep(0.075, 4), rep(0.0625, 4)))

#' @rdname consumption 
mpc_ui <- mpc(0.9, timing = c(rep(0.35, 1), 0.3, rep(0.1, 2), rep(0.05, 2)))

#' Rename mpc 
#' Ad hoc function to rename column from 'minus_neutral' to 'post_mpc'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
rename_mpc <- function(df){
  df %>% 
  rename_with(~paste0(str_remove(., 'minus_neutral'), 'post_mpc'),
              .cols = contains('minus_neutral')) 
}

mpc_taxes_transfers <- function(df){
  df %>% 
    mpc_ui() %>% 
    mpc_social_benefits() %>% 
    mpc_health_outlays() %>% 
    mpc_subsidies()  %>% 
    mpc_rebate_checks() %>% 
    mpc_corporate_taxes() %>% 
    mpc_non_corporate_taxes() %>% 
    rename_mpc()
    
}


get_all_mpc <- function(df){
  transfers <- paste0('mpc_', c('subsidies', 'ui'))
  variables <- transfers%>% all_levels() %>% paste0('_minus_neutral')
 
    mpc_fun <- eval(sym(glue::glue('mpc_{transfers}'))) 
     
}

#' Calculate mpc's
#' Create mpc
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
calculate_mpc <- function(df, taxes_transfers){
  
  net <- 'minus_neutral'
  government_levels <- c(glue::glue('{taxes_transfers}_{net}'), glue::glue('federal_{taxes_transfers}_{net}'),
                         glue::glue('state_{taxes_transfers}_{net}'))
  total <- glue::glue('{taxes_transfers}_post_mpc')
  federal <- glue::glue('federal_{taxes_transfers}_post_mpc')
  state <- glue::glue('state_{taxes_transfers}_post_mpc')
  
  if(taxes_transfers == 'subsidies'){
    second_draw <- tsibble::yearquarter('2021 Q1')
    mpc_fun <- eval(sym(glue::glue('mpc_{taxes_transfers}')))
    mpc_fun_second_draw <- eval(sym(glue::glue('mpc_{taxes_transfers}_second_draw')))
    df %>%
      dplyr::mutate(
        dplyr::across(
          .cols = tidyselect::all_of(government_levels),
          .fns = ~ if_else(date < second_draw, 
                           mpc_fun(.),
                           mpc_fun_second_draw(.)),
          .names = '{.col}_xmpc'
        )
      ) %>%
      dplyr::rename(!!total := glue::glue('{taxes_transfers}_{net}_xmpc'),
             !!federal := glue::glue('federal_{taxes_transfers}_{net}_xmpc'),
             !!state := glue::glue('state_{taxes_transfers}_{net}_xmpc'))
  }
  
  else{
    mpc_fun <- eval(sym(glue::glue('mpc_{taxes_transfers}')))
    df %>%
      dplyr::mutate(
        dplyr::across(
          .cols = tidyselect::all_of(government_levels),
          .fns = ~ mpc_fun(.),
          .names = '{.col}_xmpc'
        )
      ) %>%
      dplyr::rename(!!total := glue::glue('{taxes_transfers}_{net}_xmpc'),
             !!federal := glue::glue('federal_{taxes_transfers}_{net}_xmpc'),
             !!state := glue::glue('state_{taxes_transfers}_{net}_xmpc'))
  }
}


