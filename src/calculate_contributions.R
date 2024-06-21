#' Calculate Federal Purchases Contribution
#'
#' This function calculates the contribution of federal purchases to GDP growth.
#'
#' @param x A numeric vector representing the input series for federal purchases,
#' in billions USD.
#' @param fpdg A numeric vector representing the federal purchases deflator growth,
#' as an annualized proportion. For example, 1% annualized federal purchases deflator
#' growth would be represented as 0.01.
#' @param rpgg A numeric vector representing the real potential GDP growth, as an
#' annualized proportion. For example, 3% annualized real potential GDP growth would
#' be represented as 0.03.
#' @param gdp A numeric vector representing the GDP, in billions USD.
#'
#' @return A numeric vector representing the contribution of federal purchases to
#' GDP growth.
#' @export
#'
#' @examples
#' # Example usage:
#' federal_purchases_contribution(
#'   x = c(1890.1, 1902.2, 1915.2, 1929.3, 1943.6),
#'   fpdg = c(0.0064488, 0.0063423, 0.0062568, 0.0061065, 0.0054337),
#'   rpgg = c(0.005559, 0.005580, 0.005601, 0.005596, 0.005586),
#'   gdp = c(29314, 29626, 29942, 30268, 30577)
#' )
federal_purchases_contribution <- function(x, fpdg, rpgg, gdp) {
  # Ensure input vectors are numeric and of the same length
  stopifnot(is.numeric(x), is.numeric(fpdg), is.numeric(rpgg), is.numeric(gdp))
  stopifnot(length(x) == length(fpdg), length(fpdg) == length(rpgg), length(rpgg) == length(gdp))
  
  # Calculate the output
  output <- 400 * (x - lag(x) * (1 + fpdg + rpgg)) / lag(gdp)
  
  return(output)
}


#' Calculate Consumption Grants Contribution
#'
#' This function calculates the contribution of consumption grants to GDP growth.
#'
#' @param x A numeric vector representing the input series for consumption grants,
#' in billions USD.
#' @param cgdg A numeric vector representing the consumption grants deflator growth,
#' as an annualized proportion. For example, 1% annualized consumption grants deflator
#' growth would be represented as 0.01.
#' @param rpgg A numeric vector representing the real potential GDP growth, as an
#' annualized proportion. For example, 3% annualized real potential GDP growth would
#' be represented as 0.03.
#' @param gdp A numeric vector representing the GDP, in billions USD.
#'
#' @return A numeric vector representing the contribution of consumption grants to
#' GDP growth.
#' @export
#'
#' @examples
#' # Example usage:
#' consumption_grants_contribution(
#'   x = c(1890.1, 1902.2, 1915.2, 1929.3, 1943.6),
#'   cgdg = c(0.0064488, 0.0063423, 0.0062568, 0.0061065, 0.0054337),
#'   rpgg = c(0.005559, 0.005580, 0.005601, 0.005596, 0.005586),
#'   gdp = c(29314, 29626, 29942, 30268, 30577)
#' )
consumption_grants_contribution <- function(x, cgdg, rpgg, gdp) {
  # Ensure input vectors are numeric and of the same length
  stopifnot(is.numeric(x), is.numeric(cgdg), is.numeric(rpgg), is.numeric(gdp))
  stopifnot(length(x) == length(cgdg), length(cgdg) == length(rpgg), length(rpgg) == length(gdp))
  
  # Calculate the output
  output <- 400 * (x - lag(x) * (1 + cgdg + rpgg)) / lag(gdp)
  
  return(output)
}
