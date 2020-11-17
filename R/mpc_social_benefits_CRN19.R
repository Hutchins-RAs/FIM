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
#' @rdname mpc_social_benefits_CRN19
#' @export 
mpc_social_benefits_CRN19 = function(x)
{
  j = NA
  for (i in 8:length(x))
  {
    if (is.na(x[i - 7]))
    {
      j[i] = NA
    } else
    {
      j[i] = 0.86 * (0.2879 * x[i] + 0.2498 * x[i - 1] + 
        0.19 * x[i - 2] + 0.19 * x[i - 3] + 0.0253 * 
        x[i - 4] + 0.0253 * x[i - 5] + 0.0159 * x[i - 
        6] + 0.0159 * x[i - 7])  # distributes out to 40 percent of the -0.6 MPC applied in first two quarters and the remainder evenly over last 5
    }
  }
  j
}
