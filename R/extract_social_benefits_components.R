#' Separate unemployment insurance and rebate checks from social benefits
#'
#' @param df 
#' @param component 
#'
#' @return
#' @export
#'
#' @examples
spread_social_benefits <- function(df){
  
  df %>% 
    mutate(social_benefits = social_benefits - ui - rebate_checks,
           federal_social_benefits = federal_social_benefits - federal_ui - rebate_checks,
           state_social_benefits = state_social_benefits - state_ui) 
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
  df %>% 
    mutate(
      social_benefits = social_benefits + ui + rebate_checks, 
      federal_social_benefits = federal_social_benefits + federal_ui + rebate_checks,
      state_social_benefits = state_social_benefits + state_ui)
}