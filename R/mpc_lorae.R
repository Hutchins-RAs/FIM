#new mpc_vulnerable_arp func 

# The mpcs variable contains how much of the impulse is spent x quarters after
# disbursement of the funds. For example, an mpc = c(0.3, 0.2, 0.1) means that 
# if $100 is disbursed in 2020 Q1, then $30 is spent in 2020 Q1, an additional 
# $20 is spent in 2020 Q2, and an additional $10 is spent in 2020 Q3, for a total
# of $60 spent within the first 3 quarters of the fiscal injection.

# The x variable indicates how much money was injected in each period. Let's say 
# that the first element of the vector represents 2020 Q1. If x = c(100, 0, 0, 0, 100),
# that means that $100 was injected in 2020 Q1, and then nothing was injected for
# the next 3 quarters. Then, an additional $100 was injected in 2021 Q1.

# The roll_sum calculates a rolling dot product. Let's use the example above, with
# mpcs = c(0.3, 0.2, 0.1) and 
# x = c(100, 0, 0, 0, 100)
# Here's a visualization of what happens
#   NA   NA    100    0     0    0    100   NA   NA
#  [0.1  0.2   0.3]
#       [0.1  0.2   0.3]
#             [0.1  0.2   0.3]
#                   [0.1  0.2   0.3]
#                         [0.1  0.2   0.3]
#                               [0.1  0.2   0.3]
#                                     [0.1  0.2   0.3]
# 2020 Q1 fiscal impulse is the following dot product:
# <NA, NA, 100> * <0.1, 0.2, 0.3> where NAs are treated as 0s
# = $30
# 2020 Q2 fiscal impulse:
# <NA, 100, 0> * <0.1, 0.2, 0.3> = $20
# And skipping to the end... 2021 Q3 fiscal impulse is...
# <100, NA, NA> * <0.1, 0.2, 0.3> = $10
# You get the idea. Implicitly by assigning the MPCs vector a length of 3, we assume
# that the impulse on the economy goes to 0 after 3 quarters. In our code, our
# MPCs vectors are longer to represent a more gradual spending pattern of the recipients
# of the transfer.


mpc_lorae <- function (x, # A vector of cash disbursement data
                       mpc) # A vector of MPCs
  {
  1 * roll::roll_sum(x, 
                    width = length(mpc), 
                    weights = rev(mpc), 
                    online = FALSE, 
                    min_obs = 1)
}

#both of these functions are the same and can probably be combined into one function.

mps_lorae <- function(x, mps){
  roll::roll_sum(x,
                 width = length(mps),
                 weights = rev(mps),
                 online = FALSE,
                 min_obs = 1)
}


mps <- function(x, mps){
  roll::roll_sum(x, # We don't divide by 4, since we assume the input is already not annualized
                 width = length(mps),
                 weights = rev(mps),
                 online = FALSE,
                 min_obs = 1)
}
  

### roll_mpc: an mpc function that accommodates regime switches
# Sample input below
# roll_mpc(x = c(100, 100, 100, 100),
#          mpc_series = c("mpc01", "mpc01", "mpc02", "mpc02"),
#          mpc_list = list(
#            mpc01 = c(0.8, 0.2),
#            mpc02 = c(0.2, 0.2, 0.2, 0.2, 0.2),
#            mpc03 = c(0.5, 0.5)
#          ))
  
roll_mpc <- function(x, mpc_series, mpc_list) {
  ## Notes on arguments:
  #  x is the time series of disbursements. Must be atomic vector and equal
  # length to mpc_series
  #
  # mpc_series is the time series of mpc regimes applied to x. mpcs are referred 
  # to as character strings. For example, if x = c(100, 100, 100, 100) and mpc01 
  # is applied to the first 2 quarters and mpc02 is applied to the second two 
  # quarters, then mpc_series = c("mpc01", "mpc01", "mpc02", "mpc02")
  #
  # mpc_list is the "dictionary" or "key" connecting what mpc vector each mpc
  # name refers to. This list must include all unique strings in mpc_series. So, 
  # for example, if mpc01 = c(0.8, 0.2) and mpc02 = c(0.2, 0.2, 0.2, 0.2, 0.2), then 
  # mpc_list = list(mpc01 = c(0.8, 0.2), 
  #                 mpc02 = c(0.2, 0.2, 0.2, 0.2, 0.2))
  #
  ## Input argument pre-checks
  # Check if x is a vector
  if (!is.atomic(x)) {
    stop("x must be an atomic vector.")
  }
  # Check if if mpc_series is a vector
  if (!is.atomic(mpc_series)) {
    stop("mpc_series must be an atomic vector.")
  }
  # Check if mpc_list is a list
  if (!is.list(mpc_list)) {
    stop("mpc_list must be a list.")
  }
  # Check if x and mpc_series are the same length
  if (length(x) != length(mpc_series)) {
    stop("x and mpc_series must have equal lengths.")
  }
  # Check if all mpc names in mpc_series are also in mpc_list
  if (any(unique(mpc_series) %in% names(mpc_list))) {
    # reads TRUE if all elements of mpc_series in mpc_list. Do nothing here.
  } else {
    # If above condition is false, throw an error.
    stop("mpc_series contains entries not listed in mpc_list.")
  }

  ## "Meat" of the function
  # Each nth value of the output vector, which has the same length as the input data
  # vector x, is the result of the dot product of the mpc vector, written backwards, 
  # and the previous n elements of the x vector. If the length of the mpc vector
  # exceeds the number of available data entries from x, then the mpc vector of 
  # length n will be truncated to the length of available entries from x.
  #
  # For more intuition on why this calculation leads to practical spending calculations
  # out of time series disbursements, please read comments preceding mpc_lorae() in `mpc_lorae.R`.
  #
  # Initialize the result vector
  result <- c()
  # Loop through each element of the x vector
  for (i in seq_along(x)){
    ## To calculate result, perform a dot product: v1 %*% v2
    # v1 is the mpc vector corresponding with the i-th element of mpc_series, 
    # but written in reverse order
    mpc_name <- mpc_series[i] # get the name of the mpc vector used on this data point
    v1 <- rev(mpc_list[[mpc_name]]) # extract the mpc vector and reverse it
    n <- length(v1) # length of v1 vector is crucial for following logic
    # if for the i-th element of mpc_series, the corresponding mpc vector (written
    # in reverse order), v1, exceeds the number of entries so far of x, then 
    # truncate the v1 vector to only include its last i elements.
    if (i < n) { 
      v1 <- tail(v1, i) # keep only the last i elements of v1
      n <- length(v1) # redefine the length of v1 to match corrected length
    }
    # v2 is of a length matching the length of v1. It contains consecutive
    # elements leading up to and including the i-th element of x. 
    v2 <- tail(x, n) # choose the last n data points of x
    dot <- v1 %*% v2 # dot product v1 and v2
    result <- c(result, dot) # append new datum to results vector
  }
  
  return(result)
}


# A more efficient way to do this than a for loop is matrices. Here's an example
# of applying an mpc of (0.8, 0.2) to a vector of (100, 100, 100, 100)
# 
# mat1 <- matrix(c(100, 100, 100, 100), nrow = 1, ncol = 4)
# 
# mat2 <- matrix(c(0.8, 0, 0, 0, 
#                  0.2, 0.8, 0, 0, 
#                  0, 0.2, 0.8, 0, 
#                  0, 0, 0.2, 0.8), 
#                nrow = 4, ncol = 4)
# 
# mat1 %*% mat2



  