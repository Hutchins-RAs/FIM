
#  Year object creation function
#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
as_year <- function(x = integer()) {
  x <- vctrs::vec_cast(x, integer())
  vctrs::new_vctr(x, class = "year")
}

# Declare this class as a valid index
index_valid.year <- function(x) TRUE

# Compute the interval of a year input
interval_pull.year <- function(x) {
  tsibble::new_interval(
    year = tsibble::gcd_interval(vctrs::vec_data(x))
  )
}

# Specify how sequences are generated from years
seq.year <- function(from, to, by, length.out = NULL, along.with = NULL, ...) {
  from <- vctrs::vec_data(from)
  if (!rlang::is_missing(to)) {
    vctrs::vec_assert(to, as_year())
    to <- vctrs::vec_data(to)
  }
  as_year(NextMethod())
}

# Define `+` operation as needed for `new_data()`
vec_arith.year <- function(op, x, y, ...) {
  as_year(vctrs::vec_arith(op, vctrs::vec_data(x), vctrs::vec_data(y), ...))
}