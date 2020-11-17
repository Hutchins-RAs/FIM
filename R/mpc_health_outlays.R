#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname mpc_health_outlays
#' @export 
mpc_health_outlays = function(x){
  0.9 * rollapply(x, width = 4, mean, fill = NA, align =  'right')
}
