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
      federal_taxes_contribution = federal_corporate_taxes_contribution + federal_non_corporate_taxes_contribution,
      state_taxes_contribution = state_corporate_taxes_contribution + state_non_corporate_taxes_contribution,
      taxes_contribution = federal_taxes_contribution + state_taxes_contribution
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
    
      federal_transfers_contribution = federal_social_benefits_contribution + federal_health_outlays_contribution + federal_subsidies_contribution + federal_ui_contribution + rebate_checks_contribution + federal_student_loans_contribution,
      state_transfers_contribution = state_social_benefits_contribution + state_health_outlays_contribution + state_ui_contribution + state_subsidies_contribution,
      transfers_contribution = federal_transfers_contribution + state_transfers_contribution)
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
      federal_taxes_transfers_contribution = federal_taxes_contribution + federal_transfers_contribution,
      state_taxes_transfers_contribution = state_taxes_contribution + state_transfers_contribution,
      taxes_transfers_contribution = federal_taxes_transfers_contribution + state_taxes_transfers_contribution
    )
}

get_non_corporate_taxes <- function(df){
  df %>% 
    mutate(
      
      federal_non_corporate_taxes = federal_payroll_taxes + federal_personal_taxes + federal_production_taxes,
      state_non_corporate_taxes = state_payroll_taxes + state_personal_taxes + state_production_taxes,
      non_corporate_taxes = federal_non_corporate_taxes + state_non_corporate_taxes
    )
}


