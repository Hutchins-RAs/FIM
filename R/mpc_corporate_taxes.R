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
#' @rdname mpc_corporate_taxes
#' @export 
mpc_corporate_taxes = function(x){
  j = NA
  for(i in 12:length(x)){
    if(is.na(x[i-11])){
      j[i] = NA
    } else{
      lagstart = i-11
      lagend = i
      lags = lagstart:lagend
      j[i] = -0.4*mean(x[lags]) # distributes out to the MPC applied evenly over 12 quarters
    }
  }
  j
}
