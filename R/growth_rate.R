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

#' Quarterly Annualized Growth Rate
#'
#' Calculates the annualized growth rate from one quarter to the next in a 
#' quarterly time series.
#'
#' @param x A numeric vector representing a quarterly time series. Each element 
#' in `x` should represent the value of the time series for a specific quarter.
#'
#' @return A numeric vector containing the annualized growth rates for each 
#' quarter compared to the previous quarter, expressed as a proportion. The 
#' first element is always 0 by default.
#'
#' @examples
#' \dontrun{
#'   if(interactive()){
#'     # Simulate a quarterly series
#'     quarterly_series <- c(100, 105, 110, 120)
#'     # Calculate the annualized growth rates
#'     growth_rates <- q_a_optimized(quarterly_series)
#'     print(growth_rates)
#'   }
#' }
#'
#' @export 
qagr <- function(x) {
  n <- length(x) # Calculate the length of the input vector
  # Calculate the growth factor for each quarter, except for the first. Vector
  # x[-1] is every element of x except for the first element. Vector x[-n] is 
  # every element of x except for the last element. The ratio between these two
  # vectors gives the growth rate for elements 2 through n of vector x.
  growth_rate <- x[-1] / x[-n]
  
  # Annualize the growth rate
  annualized_growth_rate <- (growth_rate ^ 4 - 1)
  
  # Prepend a 0 for the first quarter, as there is no prior quarter to compare 
  # to
  result <- c(0, annualized_growth_rate)
  
  return(result)
}

### TODO: DEPRECATE THIS FUNCTION IN FAVOR of qagr()
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

#' @title Q4 over Q4 growth rate
#' @description Calculates 4 quarter lag growth rate.
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname q_g_fourthlag
#' @export 
q_g_fourthlag = function(x) {
  j = c()
  for (i in 5:length(x)) {
    j[i] = (((x[i]/x[i - 4])) - 1)
  }
  j[1] = j[5]
  j[2] = j[5]
  j[3] = j[5]
  j[4] = j[5]
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









