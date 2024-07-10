# src/levels.R
# This module defines xxx

#' Accumulate Growth with Reference Period
#' 
#' This function takes time series data on proportional quarterly annualized
#' growth rates as an input and outputs cumulative growth, relative to a reference
#' period as an output.
#' 
#' @param x A numeric vector representing the input series. Series should be
#' represented as an annualized quarterly growth rate proportion. For example,
#' four quarters of growth - with 1% annualized growth in Q1, 2% annualized 
#' growth in Q2, -2% annualized growth in Q3, and 2.5% annualized growth in Q4 
#' would be represented as c(0.01, 0.02, -0.02, 0.025).
#' @param ref_period A whole number representing an index value of the series that
#' is used as the reference value. This represents the period of the series around
#' which growth rates are centered, and therefore will be assigned a value of 1.
#'
#' @export
accumulate_growth <- function(x, ref_period) {
  # Ensure that the reference period is within the bounds of the series
  if (ref_period < 1 || ref_period > length(x)) {
    stop("Reference period is out of bounds of the input series.")
  }
  
  # Convert annualized quarterly growth rates to cumulative growth factors
  growth_factors <- (1 + x)^(1/4)
  
  # Calculate the cumulative product of growth factors up to each point in the series
  cumulative_growth <- cumprod(growth_factors)
  
  # Normalize the cumulative growth to the reference period
  normalized_cumulative_growth <- cumulative_growth / cumulative_growth[ref_period]
  
  return(normalized_cumulative_growth)
}