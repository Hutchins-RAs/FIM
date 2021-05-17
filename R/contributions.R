#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
purchases_contributions <- function(df){
  df %>%
    mutate(over(c('federal_purchases', 
                  'state_purchases', 
                  'consumption_grants', 
                  'investment_grants'),
                
                .fn =  ~ 400 * (.("{.x}") - lag(.("{.x}")) * (1 + .("{.x}_deflator_growth") + real_potential_gdp_growth)) 
                / lag(gdp),
                
                .names = "{x}_contribution"),
           grants_contribution = consumption_grants_contribution + investment_grants_contribution,
           federal_contribution = federal_purchases_contribution + grants_contribution,
           state_contribution = state_purchases_contribution  - grants_contribution)
  
}

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
  dplyr::lag(x) * (1 + fim$gdppoth + fim$consumption_deflator_growth)
}
all_taxes_transfers <- function(){
  taxes_transfers <- c("subsidies","health_outlays", "social_benefits",
                       "non_corporate_taxes", "corporate_taxes",  
                       'ui')

  
  all_taxes_transfers <- c(glue::glue('{taxes_transfers}'), glue::glue('federal_{taxes_transfers}'),'rebate_checks',
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
  taxes = all_levels('corporate_taxes', 'non_corporate_taxes')
  transfers = all_levels('social_benefits', 'health_outlays', 'subsidies', 'ui', 'rebate_checks')
  df %>%
    dplyr::mutate(
      dplyr::across(.cols = any_of(all_levels(taxes, transfers)),
             .fns = ~ . - dplyr::lag(.) * (1 + real_potential_gdp_growth + consumption_deflator_growth),
             .names = '{.col}_minus_neutral')
    )
}


#' Counterfactual spending
#' 
#' @param .data 
#' @param ... 
#'
#' @return
#' @import dplyr
#' @export
#'
#' @examples
counterfactual <- function(.data, ...){
  vars  <- enquos(...)
  
  
  .data %>% 
    mutate(across(.cols  = c(!!!vars),
                  .fns = ~ lag(.x) * (1 + real_potential_gdp_growth + consumption_deflator_growth),
                  .names = "{.col}_counterfactual"),
           .after = 'date')
}

#' Contribution to GDP 
#'
#' @param .data 
#' @param ... 
#'
#' @return
#' @import dplyr
#' @export
#'
#' @examples
contribution <- function(.data, ...){
  
  vars <- enquos(...)
  
  .data %>% 
    mutate(across(.cols = c(!!!vars),
                  .fns = ~ 400 * (.x  - get(paste0(cur_column(), "_counterfactual")))/ lag(gdp)),
           .names = "{.col}_contribution")
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

taxes_contributions <- function(df){
  taxes <- all_levels(c('non_corporate_taxes_post_mpc', 'corporate_taxes_post_mpc'))
  df %>%
    mutate(
      across(
        .cols  = all_of(taxes),
        .fns = ~ 400 * .x / lag(gdp),
        .names = "{.col}_contribution"
      )
    ) %>%
    rename_with(~ gsub('post_mpc_contribution', 'contribution', .x))
  
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
                 'ui','rebate_checks') %>%
    paste0('_post_mpc') %>% 
    fim::all_levels()
  
  df %>%
    mutate(
      across(
        .cols  = any_of(transfers),
        .fns = ~ 400 * .x / lag(gdp),
        .names = "{.col}_contribution"
      )
    ) %>%
    rename_with(~ gsub('post_mpc_contribution', 'contribution', .x))
}

