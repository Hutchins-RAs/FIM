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
  # TODO: Build in checks that length(x) > 1 and x is atomic
  
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
  # TODO: Standardize this first element to 0 or NA, just like the qagr() and 
  # deprecating q_a() function
  result <- c(NA, annualized_growth_rate)
  
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

#' Quarter over Quarter Growth Rate
#'
#' Calculates the quarter-over-quarter growth rate for a given numeric vector 
#' representing a quarterly time series.
#'
#' @param x A numeric vector where each element represents a value for a 
#' quarter.
#' @return A numeric vector of quarter-over-quarter growth rates, where each 
#' rate is calculated as `(current quarter / previous quarter)`. The first 
#' element is set equal to the second element's growth rate to maintain 
#' consistent vector length.
#' TODO: Change this treatment of first element to standardize with qagr() 
#' function
#' TODO: Add input data checks for x vector
#' @details This function is useful for analyzing the growth rate from one quarter to the next in a time series data. It effectively captures the immediate rate of change, allowing for the identification of trends and patterns over shorter periods.
#' @examples
#' \dontrun{
#'   # Example time series data
#'   quarterly_values <- c(100, 105, 110, 115)
#'   # Calculate quarter-over-quarter growth rates
#'   growth_rates <- qgr(quarterly_values)
#'   print(growth_rates)
#' }
#' @export
qgr <- function(x) {
  
  # TODO: Build in checks that length(x) > 1 and x is atomic
  n <- length(x) # Calculate the length of the input vector
  # Calculate the growth factor for each quarter, except for the first. Vector
  # x[-1] is every element of x except for the first element. Vector x[-n] is 
  # every element of x except for the last element. The ratio between these two
  # vectors gives the growth rate for elements 2 through n of vector x.
  growth_rate <- x[-1] / x[-n]
  
  # Prepend a the first element of growth_rate for the first quarter, as there 
  # is no prior quarter to compare to
  # TODO: Standardize this first element to 0 or NA, just like the qagr() and 
  # deprecating q_a() function
  growth_rate <- c(growth_rate[1], growth_rate)
  
  return(growth_rate)
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









