
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
#' taxes_transfers_minus_neutral <- function(df){
#'   #taxes = all_levels('corporate_taxes', 'non_corporate_taxes')
#'   #transfers = all_levels('social_benefits', 
#'                          'health_outlays', 
#'                          'subsidies', 
#'                          'ui', 
#'                          'rebate_checks')
#'   df %>%
#'     dplyr::mutate(
#'       dplyr::across(.cols = any_of(all_levels(taxes, transfers)),
#'                     .fns = ~ . - dplyr::lag(.) * (1 + real_potential_gdp_growth + consumption_deflator_growth),
#'                     .names = '{.col}_minus_neutral')
#'     )
#' }



taxes_transfers_minus_neutral_fourthlag <- function(df){
  taxes = all_levels('corporate_taxes', 'non_corporate_taxes')
  transfers = all_levels('social_benefits', 'health_outlays', 'subsidies', 'ui', 'rebate_checks')
  df %>%
    dplyr::mutate(
      dplyr::across(.cols = any_of(all_levels(taxes, transfers)),
                    .fns = ~ . - dplyr::lag(.) * (1 + real_potential_gdp_growth_fourthlag + q_g_fourthlag(consumption_deflator)),
                    .names = '{.col}_minus_neutral')
    )
}


get_real_levels<- function(df){
  taxes = all_levels('corporate_taxes', 
                     'non_corporate_taxes',
                     'federal_corporate_taxes',
                     'federal_non_corporate_taxes',
                     'state_corporate_taxes',
                     'state_non_corporate_taxes')
  transfers = all_levels('social_benefits', 
                         'federal_social_benefits',
                         'state_social_benefits',
                         'health_outlays',
                         'federal_health_outlays',
                         'state_health_outlays',
                         'subsidies', 
                         'ui', 
                         'federal_ui',
                         "state_ui",
                         'rebate_checks', 
                         'rebate_checks_arp',
                         "federal_other_direct_aid_arp",
                         "federal_other_vulnerable_arp",
                         "federal_aid_to_small_businesses_arp",
                         "federal_student_loans",
                         "medicaid",
                         'medicaid_grants',
                         "medicare", 
                         "federal_transfers",
                         "state_transfers")
  federal_purchases = all_levels('federal_purchases')
  state_purchases = all_levels('state_purchases')
  consumption_grants = all_levels('consumption_grants')
  investment_grants = all_levels('investment_grants')
  df %>%
    dplyr::mutate(
      dplyr::across(.cols = any_of(all_levels(taxes, transfers)),
                    .fns = ~.  - dplyr::lag(.) * (consumption_deflator_growth),
                    .names = '{.col}_real'),
      
      dplyr::across(.cols = any_of(all_levels(federal_purchases)),
                    .fns = ~ . - dplyr::lag(.) * (federal_purchases_deflator_growth),
                    .names = '{.col}_real'),
      dplyr::across(.cols = any_of(all_levels(state_purchases)),
                    .fns = ~ . - dplyr::lag(.) * (state_purchases_deflator_growth),
                    .names = '{.col}_real'),
      dplyr::across(.cols = any_of(all_levels(consumption_grants)),
                    .fns = ~ . - dplyr::lag(.) * (consumption_grants_deflator_growth),
                    .names = '{.col}_real'),
      dplyr::across(.cols = any_of(all_levels(investment_grants)),
                    .fns = ~ . - dplyr::lag(.) * (investment_grants_deflator_growth),
                    .names = '{.col}_real')
    )
}



annualize_deflator<- function(df){
  deflators = all_levels('consumption_deflator_growth', 
                     'federal_purchases_deflator_growth',
                     'state_purchases_deflator_growth',
                     'consumption_grants_deflator_growth',
                     'investment_grants_deflator_growth')
                  
  df %>%
    dplyr::mutate(
      dplyr::across(.cols = any_of(all_levels(deflators)),
                    .fns = ~( (.)+1)^4 - 1,
                    .names = '{.col}_ann')
    )
}

deannualize_deflator<- function(df){
  deflators = all_levels('consumption_deflator_growth_ann', 
                         'federal_purchases_deflator_growth_ann',
                         'state_purchases_deflator_growth_ann',
                         'consumption_grants_deflator_growth_ann',
                         'investment_grants_deflator_growth_ann')
  
  df %>%
    dplyr::mutate(
      dplyr::across(.cols = any_of(all_levels(deflators)),
                    .fns = ~( (.)+1)^.25 - 1,
                    .names = '{.col}'),
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


scale_by_gdp <- function(.data, ...){
  
  vars <- enquos(...)
  
  .data %>% 
    mutate(across(.cols = c(!!!vars),
                  .fns = ~ 400 * .x / lag(gdp)),
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
                 'ui','rebate_checks','federal_student_loans') %>%
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

