#' Separate unemployment insurance and rebate checks from social benefits
#'
#' @param df 
#' @param component 
#'
#' @return
#' @export
#'
#' @examples
remove_social_benefit_components <- function(df){
  remove_unemployment_insurance <- function(df){
    df %>%
      mutate(social_benefits = social_benefits - unemployment_insurance,
             federal_social_benefits = federal_social_benefits - federal_unemployment_insurance,
             state_social_benefits = state_social_benefits - state_unemployment_insurance)
  }
  remove_rebate_checks <- function(df){
    df %>%
      mutate(social_benefits = social_benefits - rebate_checks,
             federal_social_benefits = federal_social_benefits - federal_rebate_checks,
             state_social_benefits = state_social_benefits - state_rebate_checks)
  }
  df %>%
    remove_unemployment_insurance() %>%
    remove_rebate_checks()
}
#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
add_social_benefit_components <- function(df){
  add_unemployment_insurance <- function(df){
    df %>%
      mutate(social_benefits = social_benefits + unemployment_insurance,
             federal_social_benefits = federal_social_benefits + federal_unemployment_insurance,
             state_social_benefits = state_social_benefits + state_unemployment_insurance)
  }
  
  add_rebate_checks <-function(df){
    df %>%
      mutate(social_benefits = social_benefits + rebate_checks,
             federal_social_benefits = federal_social_benefits + federal_rebate_checks,
             state_social_benefits = state_social_benefits + state_rebate_checks)
  }
  
  df %>%
    add_unemployment_insurance() %>%
    add_rebate_checks()
}