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
