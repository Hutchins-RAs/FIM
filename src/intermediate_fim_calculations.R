# src/intermediate_fim_calculations.R

#' Calculate Post-Minus Neutral Series
#' 
#' This function takes time series data, real potential gdp growth, and
#' consumption deflator growth as input. It calculates a counterfactual 
#' situation where each subsequent data is the product of the previous 
#' observation, the consumption deflator, and potential gdp growth. It then
#' calculates how quickly in excess of or below real potential gdp growth a 
#' data series is growing.
#' real potential GDP growth the input data series is growing. The output 
#' can be interpreted as [WHAT? POPULATE THIS? WHAT UNITS? BUILLIONS USD? REAL
#' OR NOT?]
#' 
#' @param x A numeric vector representing the input series in billions USD.
#' @param rpgg A numeric vector representing the real potential GDP growth, as an
#' annualized proportion. For example, 3% annualized real potential GDP growth would
#' @param cdg A numeric vector representing the consumption deflator growth, as an
#' annualized proportion. For example, 3% annualized rconsumption deflator growth
#' would be represented as 0.03. [give negative example too]
#'
#' @return A numeric vector representing the "minus neutral" series
#' # TODO: improve this description of output. too vague
#' @export
minus_neutral <- function(x, # the data in question
                          rpgg, # real potential gdp growth,
                          cdg # consumption deflator growth
) {
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

# ---- post_mpc ----
#' Calculate Post-Marginal Propensity to Consume (MPC) Series
#'
#' This function takes a time series and marginal propensity to consume (MPC)
#' matrix as inputs and calculates the post-MPC series using using matrix 
#' multiplication.
#'
#' @param x A numeric vector representing the input series.
#' @param mpc_matrix A numeric matrix representing the MPC matrix. The number of rows
#' in the matrix should be equal to the length of the series.
#'
#' @return A numeric vector representing the post-MPC. [MORE DETIAL HOW IS
#' IT INTERPRETED?!?!]
#' @export
#'
#' @examples
#' # THIS example MPC matrix is nonsense. MAke it make sense.
#' series <- c(100, 200, 300, 400)
#' mpc_matrix <- matrix(c(0.5, 0.3, 0.2, 0.1,
#'                        0.4, 0.4, 0.1, 0.1,
#'                        0.3, 0.3, 0.3, 0.1,
#'                        0.2, 0.2, 0.2, 0.4), nrow = 4, byrow = TRUE)
#' calculate_mpc(x = series, mpc_matrix)

  # Input check that the dimensions of the matrix equal the length of the series
  if (nrow(mpc_matrix) != length(x)) {
    stop("The number of rows in the mpc_matrix must equal the length of the series.")
  }
  
  # Formatting the data as a vertical column matrix is not strictly necessary;
  # but it reinforces the point that this is matrix multiplication
  vert_x <- matrix(x, ncol = 1)
  
  # ensuring proper NA handling by converting to zeroes
  # TODO: Make only the first value of NA equal to 0. Keep the other NAs as NA
  vert_x[is.na(vert_x)] <- 0
  
  # Perform matrix multiplication
  output <- mpc_matrix %*% vert_x
  
  return(output)
}


#' Calculate Post-Minus Neutral Series
#' 
#' This function takes time series data, real potential gdp growth, and
#' consumption deflator growth as input. It calculates a counterfactual 
#' situation where each subsequent data is the product of the previous 
#' observation, the consumption deflator, and potential gdp growth. It then
#' calculates how quickly in excess of or below real potential gdp growth a 
#' data series is growing.
#' 
#' @param x A numeric vector representing the input series in billions USD.
#' @param rpgg A numeric vector representing the real potential GDP growth, as an
#' annualized proportion. For example, 3% annualized real potential GDP growth would
#' be represented as 0.03.
#' @param cdg A numeric vector representing the consumption deflator growth, as an
#' annualized proportion. For example, 3% annualized rconsumption deflator growth
#' @param gdp A numeric vector representing the GDP, in billions USD.
#'
#' @return A numeric vector representing the contribution of the input series to
#' GDP growth.
#' @export
#'
#' @examples
#' # Example usage:
#' generic_contribution(
#'   x = c(3219.7, 3249.7, 3280.6, 3311.4, 3343.7),
#'   gdp = c(29314, 29626, 29942, 30268, 30577)
#' )
scale_to_gdp <- function(x, gdp) {
  output = 400 * x / lag(gdp)
  return(output)
}

# ---- calculate_contribution ----
#' Calculate FIM Contributions
#'
#' This function calculates the the generic contribution of a time series to GDP
#' growth.
#'
#' @param x A numeric vector representing the input series in billions USD.
#' @param gdp A numeric vector representing the GDP, in billions USD.
#'
#' @return A numeric vector representing the contribution of the input series to
#' GDP growth.
#' @export
#'
#' @examples
#' # Example usage:
#' generic_contribution(
#'   x = c(3219.7, 3249.7, 3280.6, 3311.4, 3343.7),
#'   gdp = c(29314, 29626, 29942, 30268, 30577)
#' )
calculate_contribution <- function(x, mpc_matrix, rpgg, cdg, gdp) {
  # Take the input data series, x
  x %>%
    # First, apply the minus_neutral function to x, setting real potential
    # GDP growth and consumption deflator growth inputs to those specified
    # by the arguments.
    minus_neutral(x = ., rpgg = rpgg, cdg = cdg) %>%
    # Next, take the output series from minus neutral as an input for the
    # mpc function. Use the mpc matrix specified in the above arguments.
    mpc(x = ., mpc_matrix = mpc_matrix) %>%
    # Finally, take the output series from the mpc function as an input for
    # the scale to gdp function. Specify the GDP using the input from the 
    # arguments.
    scale_to_gdp(x = ., gdp = gdp)
  # After these three steps, your output will be the FIM contribution, as 
  # specified in our methodology.
}

