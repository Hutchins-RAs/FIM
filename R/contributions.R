#' Calculate contributions of grants and purchases
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
contributions_purchases_grants <- function(df){
  map(
    alist(federal_nom, state_local_nom, federal_cgrants, federal_igrants),
    ~ contribution(df, !!.x)
  ) %>%
    reduce(left_join) %>%
    left_join(df, .)
}
#' Sum contributions of federal and state grants & purcrchases 
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
total_purchases <- function(df){
  df %>%
    mutate(federal_cont_ex_grants = federal_nom_cont,
           federal_grants_cont = federal_cgrants_cont + federal_igrants_cont,
           federal_cont = federal_nom_cont + federal_grants_cont,
           state_local_cont_ex_grants = state_local_nom_cont,
           state_local_cont = state_local_nom_cont - federal_grants_cont,
           purchases_cont = federal_cont + state_local_cont)
}


#' Calculate neutral counterfactual taxes and transfers
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
neutral <- function(.data, var){
  dplyr::lag(x) * (1 + fim$gdppoth + fim$pi_pce)
}
all_taxes_transfers <- function(){
  taxes_transfers <- c("subsidies","health_outlays", "social_benefits",
                       "noncorp_taxes", "corporate_taxes", 'rebate_checks', 
                       'unemployment_insurance')
  government_level <- c('federal', 'state')
  all_taxes_transfers <- c(glue::glue('{taxes_transfers}'), glue::glue('federal_{taxes_transfers}'),
                           glue::glue('state_{taxes_transfers}'))
  return(all_taxes_transfers)
}
#' Subtract counterfactual taxes and transfers from realized taxes and transfers
#'
#' @param df 
#'
#' @return
#' Difference between realized and counterfactual paths for taxes and transfers.
#' We assume that taxes and transfers would grow with potential gdp. Therefore,
#' the counterfactual growth is 'neutral' since it wouldn't deviate from the
#' potential growth of the economy.
#' @export
#'
#' @examples
taxes_transfers_minus_neutral <- function(df){
  df %>%
    dplyr::mutate(
      dplyr::across(.cols = all_of(all_taxes_transfers()),
             .fns = ~ . - dplyr::lag(.) * (1 + gdppoth + pi_pce),
             .names = '{.col}_minus_neutral')
    )
}


#' Apply mpcs to taxes and transfers 
#'
#' @param df 
#' @param taxes_transfers 
#'
#' @return
#' @export
#'
#' @examples
calculate_mpc <- function(df, taxes_transfers){
  
  net <- 'minus_neutral'
  government_levels <- c(glue('{taxes_transfers}_{net}'), glue('federal_{taxes_transfers}_{net}'),
                         glue('state_{taxes_transfers}_{net}'))
  total <- glue('{taxes_transfers}_post_mpc')
  federal <- glue('federal_{taxes_transfers}_post_mpc')
  state <- glue('state_{taxes_transfers}_post_mpc')
  
  if(taxes_transfers == 'subsidies'){
    second_draw <- as_date('2021-03-31')
    mpc_fun <- eval(sym(glue('mpc_{taxes_transfers}')))
    mpc_fun_second_draw <- eval(sym(glue('mpc_{taxes_transfers}_second_draw')))
    df %>%
      mutate(
        across(
          .cols = all_of(government_levels),
          .fns = ~ if_else(date < second_draw, 
                           mpc_fun(.),
                           mpc_fun_second_draw(.)),
          .names = '{.col}_xmpc'
        )
      ) %>%
      rename(!!total := glue('{taxes_transfers}_{net}_xmpc'),
             !!federal := glue('federal_{taxes_transfers}_{net}_xmpc'),
             !!state := glue('state_{taxes_transfers}_{net}_xmpc'))
  }
  else{
    mpc_fun <- eval(sym(glue('mpc_{taxes_transfers}')))
    df %>%
      mutate(
        across(
          .cols = all_of(government_levels),
          .fns = ~ mpc_fun(.),
          .names = '{.col}_xmpc'
        )
      ) %>%
      rename(!!total := glue('{taxes_transfers}_{net}_xmpc'),
             !!federal := glue('federal_{taxes_transfers}_{net}_xmpc'),
             !!state := glue('state_{taxes_transfers}_{net}_xmpc'))
  }
}

#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
taxes_contributions <- function(df){
  taxes <- c('noncorp_taxes_post_mpc', 'corporate_taxes_post_mpc')
  all_taxes <- c(glue('{taxes}'), glue('federal_{taxes}'), glue('state_{taxes}')) 
  df %>%
    mutate(
      across(
        .cols  = all_of(all_taxes),
        .fns = ~ 400 * .x / lag(gdp),
        .names = "{.col}_cont"
      )
    ) %>%
    rename_with(~ gsub('post_mpc_cont', 'cont', .x))
  
}

#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
transfers_contributions <- function(df){
  transfers <- c('social_benefits', 'health_outlays', 'subsidies',
                 'unemployment_insurance', 'rebate_checks') %>%
    paste0('_post_mpc')
  all_transfers <- c(glue('{transfers}'), glue('federal_{transfers}'), glue('state_{transfers}')) 
  df %>%
    mutate(
      across(
        .cols  = all_of(all_transfers),
        .fns = ~ 400 * .x / lag(gdp),
        .names = "{.col}_cont"
      )
    ) %>%
    rename_with(~ gsub('post_mpc_cont', 'cont', .x))
}

