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
