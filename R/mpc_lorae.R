# TODO: Fold all of this documentation, as well as mpc regime switchover examples
# in root/mpc_switchover_examples.xlsx, into one large RMarkdown or other 
# file.

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
  
# FUNCTIONAL CHECKS SAVED FOR LATER
  # #  x is the time series of disbursements. Must be atomic vector and equal
  # # length to mpc_series
  # ## Input argument pre-checks
  # # Check if x is a vector
  # if (!is.atomic(x)) {
  #   stop("x must be an atomic vector.")
  # }
  # # Check if x and mpc_series are the same length
  # if (length(x) != length(mpc_series)) {
  #   stop("x and mpc_series must have equal lengths.")
  # }



# ##  Proof of concept: MPC regimes as matrices
# # Here's an example of applying an mpc of (0.8, 0.2) to a data vector of 
# # c(100, 100, 100, 100), which represents a $100billion quarterly disbursement
# # disbursed over the course of 4 quarters, where recipients spend 80% of the money
# # in the quarter they receive it and the remaining 20% in the following quarter.

# # a single column of data
# x_vector <- matrix(c(100, 100, 100, 100), nrow = 4, ncol = 1)
# 
# # a matrix applying mpcs to each column
# mpc_matrix <- matrix(c(0.8, 0, 0, 0,
#                        0.2, 0.8, 0, 0,
#                        0, 0.2, 0.8, 0,
#                        0, 0, 0.2, 0.8),
#                      nrow = 4, ncol = 4, byrow = TRUE)
#
# # Multiply the mpc_matrix by the data_matrix to obtain the effect of the disbursement
# # on consumption.
# mpc_matrix %*% data_matrix
#
# # Because of the simplicity of matrix algebra, the same mpc matrix can be applied
# # to multiple data vectors at the same time.
# data_matrix <- matrix(c(100, 100, 100, 100, 100, 0, 0, 0), nrow = 4, ncol = 2)
# mpc_matrix %*% data_matrix


#' Generate MPC Matrix
#'
#' This function generates a matrix representing the marginal propensity to consume (MPC)
#' regimes applied over a series of periods. It constructs a matrix where each row 
#' corresponds to a period's MPC regime, allowing for the application of different MPCs 
#' across different periods. The function facilitates dynamic MPC calculations over time,
#' supporting regime switches and varying consumption patterns.
#'
#' @param mpc_series An atomic vector of character strings, each representing the MPC 
#'        regime applied to a corresponding period in the data series. The length of 
#'        `mpc_series` must match the length of the data series it is applied to.
#' @param mpc_list A list mapping MPC regime names (as used in `mpc_series`) to their 
#'        corresponding MPC vectors. Each entry in `mpc_list` should be a numeric vector 
#'        representing the portion of an impulse spent across subsequent periods.
#' @return A square matrix of dimensions equal to the length of `mpc_series`, where each 
#'         row represents the MPC effects for a specific period, allowing for the application
#'         of these MPCs through matrix multiplication with a data series vector.
#' @examples
#' mpc_series <- c("mpc01", "mpc01", "mpc02", "mpc02")
#' mpc_list <- list(mpc01 = c(0.8, 0.2), mpc02 = c(0.2, 0.2, 0.2, 0.2, 0.2))
#' mpc_matrix <- generate_mpc_matrix(mpc_series, mpc_list)
#' data_matrix <- matrix(c(100, 100, 100, 100), nrow = 4, ncol = 1)
#' # Apply the MPC matrix to the data matrix to calculate the effect on consumption
#' mpc_matrix %*% data_matrix
#'
#' @export
generate_mpc_matrix <- function(mpc_series, mpc_list) {
  ## Notes on arguments:
  # mpc_series is the time series of mpc regimes applied to the data series. 
  # mpcs are referred to as character strings. For example, if mpc regime "mpc01"
  # is applied to the first 2 quarters and mpc regime "mpc02" is applied to the 
  # second two quarter of a four-period data series, then 
  # mpc_series = c("mpc01", "mpc01", "mpc02", "mpc02")
  # Note: the length of mpc_series must match the length of the data series to which
  # it is applied - otherwise, the output mpc_matrix from this function will not
  # be able to multiply with the original data series.
  #
  # mpc_list is the "dictionary" or "key" connecting what mpc vector each mpc
  # name refers to. This list must include all entries in mpc_series, otherwise,
  # the function will stop and present an error. So, for example, if 
  # mpc01 = c(0.8, 0.2) and mpc02 = c(0.2, 0.2, 0.2, 0.2, 0.2), then 
  # mpc_list = list(mpc01 = c(0.8, 0.2), 
  #                 mpc02 = c(0.2, 0.2, 0.2, 0.2, 0.2))
  # Please note that any "extra" mpcs not named in mpc_series (e.g. "mpc03") are
  # simply ignored and will not cause errors.
  #
  ## Input argument pre-checks
  # TODO: add a check to see if vector entries in mpc_list are of length 1 or greater
  # Check if if mpc_series is an atomic vector
  if (!is.atomic(mpc_series)) {
    stop("mpc_series must be an atomic vector.")
  }
  # Check if mpc_list is a list
  if (!is.list(mpc_list)) {
    stop("mpc_list must be a list.")
  }
  # Check if all mpc names in mpc_series are also in mpc_list
  if (!all(mpc_series %in% names(mpc_list))) {
    stop("mpc_series contains entries not listed in mpc_list.")
  }
  
  ## "Meat" of the function
  # For more intuition on why the matrix formed by this function represents mpcs.
  # please read comments preceding mpc_lorae() in `mpc_lorae.R`.
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
    
    # Use v1, i, and n to overwrite matrix entries (which were initialized as 0).
    # Each row corresponds to a separate mpc period, so the row being overwritten
    # is indexed by i. The diagonal entry is changed by the mpc vector, as are the
    # (n-1) entries to the left of each diagonal entry.
    row_index <- i
    col_start_index <- i - n + 1
    col_end_index <- i
    # overwrite certain indices of the matrix with the v1 vector
    mpc_matrix[row_index, col_start_index:col_end_index] <- v1
  }
  return(mpc_matrix)
}

# TODO: add mpc examples in documentation for TAXES, not just for DISBURSEMENTS.
#' Simple MPC Matrix
#'
#' This function generates a matrix representing one simple marginal propensity 
#' to consume (MPC) regime applied over a series of `dim` periods. It constructs one 
#' lower triangular matrix.
#'
#' @param mpc_vector A vector of a marginal propensity to consume. For example,
#' if consumers spend 80% of disbursed funds in the period it was disbursed and
#' 20% of it in the next period, then the mpc_vector representing this pattern is 
#' mpc_vector = (0.8, 0.2). If consumers don't spend any of the money in the period
#' it was disbursed but then consume half of it the following period and the next 
#' half in the third period, then mpc_vector = c(0, 0.5, 0.5)
#' @param dim The dimension of the (square) output matrix, which should equal the
#' number of periods in the data series to which the mpc_matrix will eventually 
#' be applied.
#' @return A square matrix of dimensions `dim` where each row represents the MPC 
#'         effects for a specific period, allowing for the application
#'         of these MPCs through matrix multiplication with a data series vector.
#' @examples
#' here
#' @export
mpc_matrix <- function(mpc_vector, dim) {
  ## Input argument pre-checks
  # Check if mpc_vector is a numeric vector
  if (!is.numeric(mpc_vector)) {
    stop("mpc_vector must be a numeric vector.")
  }
  # Check if dim is a single positive integer
  if (!is.numeric(dim) || length(dim) != 1 || dim <= 0 || dim %% 1 != 0) {
    stop("dim must be a single positive integer.")
  }
  
  # Check if the length of mpc_vector exceeds dim
  if (length(mpc_vector) > dim) {
    n <- length(mpc_vector) - dim
    warning(glue::glue("The length of mpc_vector exceeds the specified dim by {n}. ",
                       "The last {n} elements of mpc_vector do not appear in the",
                       "output matrix."))
    mpc_vector <- head(mpc_vector, -n) # Adjust mpc_vector to fit within dim
  }
  
  ## "Meat" of the function
  # Produce a vector v by appending zeroes to mpc_vector such that the total
  # length of v is equal to (dim + 1).
  v <- c(mpc_vector, rep(0, times = dim - length(mpc_vector)+1)) 
  # Populate a matrix, column-wise, that has a width of (dim+1) and a length of 
  # (dim) using vector v. This deposits desired values in the lower triangle.
  M <- matrix(v, nrow=dim,ncol=dim+1, byrow = FALSE)
  # Keep all but the last column to produce a square matrix.
  M <- M[,1:dim]
  # Overwrite any entries in the upper triangle with zeroes to produce a lower 
  # triangular matrix
  M[upper.tri(M)] <- 0
  return(M)
}

# TODO: add mpc examples in documentation for TAXES, not just for DISBURSEMENTS.
#' Composite Marginal Propensity to Consume (MPC) Matrix
#'
#' Generates a matrix representing complex, variable MPC regimes over a series
#' of periods. Constructs a lower triangular matrix by combining MPC matrices
#' from specified regimes. Intended for applying varying MPC effects across
#' different time periods.
#'
#' @param mpc_vector_list A list where each element is a named vector representing
#'        an MPC regime (e.g., how consumers distribute spending of disbursed funds
#'        over time). Names correspond to MPC regime identifiers.
#'        Example: `mpc_vector_list$mpc01 = c(0.315, 0.315, 0.090)`
#'        signifies that under regime `mpc01`, consumers spend 31.5% of cash in the
#'        first period, another 31.5% in the next, followed by 9%.
#' @param mpc_series A character vector indicating the MPC regime applied in each
#'        period. Length equals the number of periods and the dimension of the
#'        output matrix. Example: `mpc_series = c("mpc01", "mpc01", "mpc02", "mpc02")`
#'        applies `mpc01` to the first two periods and `mpc02` to the next two.
#' @return A square lower triangular matrix representing the composite MPC effect
#'         over time. Matrix multiplication with a data series vector applies
#'         the MPC effects.
#' @examples
#' # Define MPC regimes
#' mpc_vector_list = list(mpc01 = c(0.8, 0.2), mpc02 = c(0.2, 0.2, 0.2, 0.2, 0.2))
#' # Define series of regimes
#' mpc_series = c("mpc01", "mpc01", "mpc01", "mpc02", "mpc02", "mpc02")
#' # Generate composite MPC matrix
#' comp_mpc_matrix = comp_mpc_matrix(mpc_vector_list, mpc_series)
#' @export
comp_mpc_matrix <- function(mpc_vector_list, mpc_series) {
  ## Input argument pre-checks
  # TODO: add a check to see if vector entries in mpc_list are of length 1 or greater
  # Check if if mpc_series is an atomic vector
  if (!is.atomic(mpc_series)) {
    stop("mpc_series must be an atomic vector.")
  }
  # Check if mpc_list is a list
  # TODO: check if it is a list of vectors
  if (!is.list(mpc_vector_list)) {
    stop("mpc__vector_list must be a list of vector.")
  }
  # Check if all mpc names in mpc_series are also in mpc_list
  if (!all(mpc_series %in% names(mpc_vector_list))) {
    stop("mpc_series contains entries not listed in mpc_vector_list.")
  }

  ## "Meat" of the function
  # Determine the dimensions based on the length of mpc_series
  dim <- length(mpc_series)
  # Initialize the composite MPC matrix
  comp_mpc_mat <- matrix(0, nrow = dim, ncol = dim)
  
  # Iterate over each unique regime in mpc_series
  unique_regimes <- unique(mpc_series)
  for (regime in unique_regimes) {
    mpc_vector <- mpc_vector_list[[regime]]
    if (!is.null(mpc_vector)) {
      # Construct the individual MPC matrix for the current regime
      mpc_mat <- mpc_matrix(mpc_vector, dim)
      # Identify rows to be replaced based on the current regime
      rows_to_replace <- which(mpc_series == regime)
      # Apply the regime's effects for all its occurrences at once
      comp_mpc_mat[rows_to_replace, ] <- mpc_mat[rows_to_replace, ]
    }
  }
  
  return(comp_mpc_mat)
}
