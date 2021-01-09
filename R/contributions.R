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
neutral <- function(x){
  lag(x) * (1 + fim$gdppoth + fim$pi_pce)
}
#' Subtract counterfactual taxes and transfers from realized taxes and transfers
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
taxes_transfers_minus_neutral <- function(df){
  taxes_transfers <- c("subsidies","health_outlays", "social_benefits",
                       "noncorp_taxes", "corporate_taxes", 'rebate_checks', 
                       'unemployment_insurance')
  government_level <- c('federal', 'state')
  all_taxes_transfers <- c(glue('{taxes_transfers}'), glue('federal_{taxes_transfers}'),
                           glue('state_{taxes_transfers}'))
  df %>%
    mutate(
      across(.cols = all_of(all_taxes_transfers),
             .fns = ~ . - neutral(.),
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