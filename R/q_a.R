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
