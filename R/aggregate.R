#' Sum federal and state projections
#'
#' @param df 
#' @param total 
#' @param federal 
#' @param state 
#'
#' @return
#' @export
#'
#' @examples 


sum_projections <- function(df, total, federal, state){
  df %>%
    mutate({{total}} := if_else(historical == 0, {{federal}} + {{state}}, {{total}})
    )
}
#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
sum_taxes_contributions <- function(df){
  taxes <- c('non_corporate_taxes', 'corporate_taxes')
  df %>%
    mutate(
      taxes_cont = corporate_taxes_cont + non_corporate_taxes_cont,
      federal_taxes_cont = federal_corporate_taxes_cont + federal_non_corporate_taxes_cont,
      state_taxes_cont = state_corporate_taxes_cont + state_non_corporate_taxes_cont
    )
}
#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
sum_transfers_contributions <- function(df){
  transfers <- c('social_benefits',  'health_outlays', 'subsidies',
                 'ui', 'rebate_checks')
  df %>%
    mutate(
      transfers_cont = social_benefits_cont + health_outlays_cont + subsidies_cont + ui_cont + rebate_checks_cont,
      federal_transfers_cont = federal_social_benefits_cont + federal_health_outlays_cont + federal_subsidies_cont + federal_ui_cont + rebate_checks_cont,
      state_transfers_cont = state_social_benefits_cont + state_health_outlays_cont + state_ui_cont + state_subsidies_cont)
}
#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
sum_taxes_transfers <- function(df){
  df %>%
    mutate(
      taxes_transfers_cont = taxes_cont + transfers_cont,
      federal_taxes_transfers_cont = federal_taxes_cont + federal_transfers_cont,
      state_taxes_transfers_cont = state_taxes_cont + state_transfers_cont,
    )
}


