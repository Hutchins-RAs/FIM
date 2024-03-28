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
generate_mpc_matrix <- function(mpc_series, mpc_list) {
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
  # name refers to. This list must include all unique strings in mpc_series, otherwise,
  # the function will stop and present an error. So, for example, if 
  # mpc01 = c(0.8, 0.2) and mpc02 = c(0.2, 0.2, 0.2, 0.2, 0.2), then 
  # mpc_list = list(mpc01 = c(0.8, 0.2), 
  #                 mpc02 = c(0.2, 0.2, 0.2, 0.2, 0.2))
  # Please note that any "extra" mpcs not named in mpc_series (e.g. "mpc03") are
  # simply ignored and will not cause errors.
  #
  ## Input argument pre-checks
  # Check if if mpc_series is an atomic vector
  if (!is.atomic(mpc_series)) {
    stop("mpc_series must be an atomic vector.")
  }
  # Check if mpc_list is a list
  if (!is.list(mpc_list)) {
    stop("mpc_list must be a list.")
  }
  # Check if all mpc names in mpc_series are also in mpc_list
  if (any(unique(mpc_series) %in% names(mpc_list))) {
    # reads TRUE if all elements of mpc_series in mpc_list. Do nothing here.
  } else {
    # If above condition is false, throw an error.
    stop("mpc_series contains entries not listed in mpc_list.")
  }
  
  ## "Meat" of the function
  # For more intuition on why this calculation leads to practical spending calculations
  # out of time series disbursements, please read comments preceding mpc_lorae() in `mpc_lorae.R`.
  # TODO: write up an R Markdown notebook or some other documentation explaining
  # how mpc_matrices work and how mpcs work in the FIM more generally.
  #
  # Initialize an empty matrix of appropriate dimension. This matrix will eventually
  # contain the function output
  dim <- length(mpc_series)
  mpc_matrix <- matrix(0, nrow = dim, ncol = dim) # initialized matrix
  
  # Iterate through each period's MPC regime
  for (i in seq_len(dim)) {
    mpc_name <- mpc_series[i] # get the name of the mpc vector used on this data point
    # v1 is the mpc vector corresponding with the i-th element of mpc_series, 
    # but written in reverse order. It comprises the nonzero portion of any mpc
    # matrix row, and its contents are truncated to fit the dimensions of the matrix
    v1 <- rev(mpc_list[[mpc_name]]) # extract the mpc vector and reverse it
    n <- length(v1) # length of v1 vector is crucial for following logic
    if (i < n) { 
      v1 <- tail(v1, i) # keep only the last i elements of v1
      n <- length(v1) # redefine the length of v1 to match corrected length
    }
    
    # use v1, i, and n to overwrite matrix entries (which were initialized as 0)
    # each row corresponds to a separate mpc period, so the row being overwritten
    # is indexed by i. The diagonal entry will be changed by the mpc vector, as will
    # n entries to the left of each diagonal entry.
    row_index <- i
    col_start_index <- i - n + 1
    col_end_index <- i
    mpc_matrix[row_index, col_start_index:col_end_index] <- v1
  }
  return(mpc_matrix)
}

## Example usage of generate_mpc_matrix
mpc_series <- c("mpc01", "mpc01", "mpc02", "mpc02")
mpc_list <- list(mpc01 = c(0.8, 0.2), mpc02 = c(0.2, 0.2, 0.2, 0.2, 0.2))
mpc_matrix <- generate_mpc_matrix(mpc_series, mpc_list)

data_matrix <- matrix(c(100, 100, 100, 100), nrow = 4, ncol = 1)

# Multiply the mpc_matrix by the data_matrix to obtain the effect of the data series
# on consumption
mpc_matrix %*% data_matrix
  