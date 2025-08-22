
minus_neutral <- function(x, # the data in question
                          rpgg, # real potential gdp growth,
                          dg # consumption deflator growth
) {
  output <- x - lag(x) * (1 + rpgg) * (1 + dg)
  return(output)
}

#' This function takes a time series and marginal propensity to consume (MPC)
#' matrix as inputs and calculates the post-MPC series using using matrix 
#' multiplication.
mpc <- function(x, mpc_matrix) {
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

scale_to_gdp <- function(x, gdp) {
  output = 100*((1 + x / lag(gdp))^4-1)
  return(output)
}

#' This function calculates the generic contribution of a time series to GDP
#' growth. 
contribution_purchases <- function(
    x, # our data series 
    rpgg, # real potential GDP growth
    dg, # deflator growth
    gdp #gdp
) {
  
  # Actual Growth Minus Counterfactual Growth
  result <- minus_neutral_purchases(x = x, rpgg = rpgg, dg = dg)
  
  # Apply Scale to GDP Function
  output <- scale_to_gdp_purchases(x = x, result = result, gdp = gdp)
  return(output)
}


contribution_transfers <- function(x, # our data
                                   c, # consumption 
                                   rpgg, # real potential GDP growth
                                   dg, # deflator growth
                                   gdp # gdp 
) {
  
  # Define Counterfactual
  counterfactual <- as.numeric(c - x + lag(x) * (1 + rpgg + dg))
  
  # Define Minus Neutral
  minus_neutral <- (c / lag(c))^4 - (counterfactual / lag(c))^4
  
  # Scale to GDP
  output <- 100 * minus_neutral * (lag(c) / lag(gdp))
  
  return(output)
}


level <- function(x, mpc_matrix = NULL, rpgg, dg, gdp) {
  # If mpc_matrix is not NULL, apply the mpc function first
  if (!is.null(mpc_matrix)) {
    x <- x %>%
      mpc(x = ., mpc_matrix = mpc_matrix)
  }
  
  # Apply the minus_neutral function to x, setting real potential GDP growth
  # and deflator growth inputs to those specified by the arguments.
  result <- x %>%
    minus_neutral(x = ., rpgg = rpgg, dg = dg)
  
  # Apply the scale_to_gdp function
  result %>%
    scale_to_gdp(x = ., gdp = gdp)
}

# ===========================
# Unit-Level Functions (PURCHASES)
# ===========================

# DEFINE MINUS NEUTRAL FUNCTION (PURCHASES)
minus_neutral_purchases <- function(x, # the data in question
                                    rpgg, # real potential gdp growth,
                                    dg # consumption deflator growth
) {
  output <- (x/lag(x) - dg)^4 - (1+rpgg)
  return(output)
}

# DEFINE SCALE_TO_GDP FUNCTION (PURCHASES)
# Scale to GDP
scale_to_gdp_purchases <- function(x, # the data in question, 
                                   gdp, # GDP
                                   result)
{
  output = 100*result*(lag(x)/lag(gdp))
  return(output)
}


# ===========================================
# Unit-Level Functions (TAXES AND TRANSFERS)
# ===========================================

# Define counterfactual Consumption
# We subtract the actual policy impulse from consumption and replace it with our 
# 'counterfactual' impulse - the last quarter's impulse grown at the rate of potential.
t_counterfactual <- function(x,  # Our data series 
                             c, # Personal Consumption Expenditures
                             rpgg, # Real Potential GDP Growth (quarterly)
                             dg # Deflator Growth (quarterly)
) {
  
  
  counterfactual <- as.numeric(c - x + lag(x)*(1+rpgg+dg))
  return(counterfactual)
}

# Define minus neutral
minus_neutral_t <- function(c, 
                            counterfactual
) {
  minus_neutral <- (c/lag(c))^4 - (counterfactual/lag(c))^4 
  return(minus_neutral)
}

# Scale results to GDP 
scale_to_gdp_t <- function(minus_neutral,
                           gdp,
                           c
) {
  result <- 100*minus_neutral*(lag(c)/lag(gdp))
  return(result)
}
