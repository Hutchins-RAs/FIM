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
#' @rdname mpc_subsidies
#' @export 
mpc_subsidies = function(x)
{
  j = NA
  for (i in 12:length(x))
  {
    if (is.na(x[i - 11]))
    {
      j[i] = NA
    } else
    {
      lagstart = i - 11
      lagend = i
      lags = lagstart:lagend
      j[i] = 0.45 * (0.11 * x[i] + 0.095 * x[i - 1] + 0.09 * 
        x[i - 2] + 0.085 * x[i - 3] + 0.08 * x[i - 4] + 
        0.08 * x[i - 5] + 0.08 * x[i - 6] + 0.08 * x[i - 
        7] + 0.075 * x[i - 8] + 0.075 * x[i - 9] + 0.075 * 
        x[i - 10] + 0.075 * x[i - 11])
    }
  }
  j
}
