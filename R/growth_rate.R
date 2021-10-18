#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @param period PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname growth_rate
#' @export 
growth_rate <- function(x, period) {
  rate <- c()
  if (period == "annualized") {
    rate <- (((x/lag(x))^4) - 1) * 100
    rate[1] = 0
  } 
  else if (period == "qoq") {
    rate <- (x/lag(x)) - 1
    rate[1] = rate[2]
  }
  return(rate)
}
#' @title Quarterly annualized growth rate
#' @description Calculate quarterly annualized growth rates
#' @param x A quarterly time series object
#' @return Annualized quarterly growth
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname q_a
#' @export 
q_a <- function(x) {
  j = c()
  for (i in 2:length(x)) {
    j[i] = (((x[i] / x[i - 1]) ^ 4) - 1) * 100
  }
  j[1] = 0
  j
}

#' @title Quarter over quarter growth rate
#' @description Calculates quarter over quarter growth rate.
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname q_g
#' @export 
q_g = function(x) {
  j = c()
  for (i in 2:length(x)) {
    j[i] = (((x[i]/x[i - 1])) - 1)
  }
  j[1] = j[2]
  j
}


#' Year over year growth rate
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
q_g_fourthlag <- function (x){
  j = c()
  for (i in 4:length(x)) {
    j[i] = (((x[i] / x[i - 4])) - 1) * 100
  }
  j[1] = j[5]
  j[2] = j[5]
  j[3] = j[5]
  j[4] = j[5]
}

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
yoy <- function (x){
  j = c()
  for (i in 4:length(x)) {
    j[i] = (((x[i] / x[i - 4])) - 1) 
  }
  
  j
}