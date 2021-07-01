
#' Title
#'
#' @param var 
#' @param by 
#'
#' @return
#' @export
#'
#' @examples
scale <- function( var, by){
  rlang::eval_tidy(rlang::expr({{var}} / {{by}}), env = caller_env())
}


#' Title
#'
#' @param var 
#' @param deflator 
#'
#' @return
#' @export
#'
#' @examples
counterfactual <- function(var, deflator){
  equation <- rlang::expr(lag({{var}} * (1 + {{ deflator }} + real_potential_gdp_growth)))
  rlang::eval_tidy(equation, env = caller_env())
}



#' Title
#'
#' @param var 
#' @param deflator 
#'
#' @return
#' @export
#'
#' @examples
contribution <- function(var, deflator){
  
  counterfactual <- rlang::expr(400 * ( {{var}} - counterfactual({{var}}, {{deflator}})) / lag(gdp))
  
  rlang::eval_tidy(counterfactual, env = caller_env())
}

