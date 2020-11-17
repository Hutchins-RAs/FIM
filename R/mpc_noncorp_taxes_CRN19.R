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
#' @rdname mpc_noncorp_taxes_CRN19
#' @export 
mpc_noncorp_taxes_CRN19 = function(x)
{
  j = NA
  for (i in 8:length(x))
  {
    if (is.na(x[i - 7]))
    {
      j[i] = NA
    } else
    {
      lagstart = i - 7
      lagend = i - 2
      lags = lagstart:lagend
      j[i] = -0.6 * (0.2 * x[i] + 0.2 * x[i - 1] + (0.6 * 
        mean(x[lags])))
    }
  }
  j
}
