#' minus_neutral
#' 
#' `minus_neutral` takes time series data, real potential gdp growth, and
#' consumption deflator growth as input. It calculates a counterfactual 
#' situation where each subsequent data is the product of the previous 
#' observation, the consumption deflator, and potential gdp growth. It then
#' calculates how quickly in excess of or below real potential gdp growth a 
#' data series is growing.
#' 
# generalized minus_neutral function
# TODO: revisit this and clean up minus neutral. Should minus neutral
# counterfactual be subtracted from x, or divided into x?
minus_neutral <- function(x, # the data in question
                          rpgg, # real potential gdp growth,
                          cdg # consumption deflator growth
) {
  output <- x - lag(x) * (1 + rpgg + cdg)
  # This is the correct, calculation, but it affects minus_neutral
  #output <- x - (lag(x) * (1 + rpgg) * (1 + cdg))
  # This optional line will make the 1970 Q1 entries equal a numeric value, rather
  # than NA, by assuming that the 1969 Q4 value for each data series was 0. 
  # Prior versions of the FIM had this setting on for pandemic-era stimulus. We
  # have chosen to remove it.
  # output <- x - lag(x, default = 0) * (1 + rpgg + cdg)
  return(output)
}