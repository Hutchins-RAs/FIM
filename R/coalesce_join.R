#' Coalesce Join Two Data Frames
#'
#' This function performs a specified join (default is full join) on two data 
#' frames and then coalesces overlapping columns, prioritizing non-missing values
#' from the first data frame.
#'
#' @param x A data frame.
#' @param y A data frame.
#' @param by A character vector of variables to join by. If `NULL`, the default,
#' joins by all common variables in `x` and `y`.
#' @param suffix A character vector of length 2 specifying the suffixes to be 
#' added to overlapping column names from `x` and `y`. Default is `c(".x", ".y")`.
#' @param join The join function to use. Default is `dplyr::full_join`.
#' @param ... Additional arguments passed to the join function.
#'
#' @return A data frame with coalesced overlapping columns.
#' @importFrom dplyr full_join coalesce bind_cols
#' @importFrom purrr map_dfc
#' @examples
#' df1 <- data.frame(
#'   id = 1:3,
#'   value = c(10, 20, 30),
#'   stringsAsFactors = FALSE
#' )
#' 
#' df2 <- data.frame(
#'   id = 2:4,
#'   value = c(NA, 40, 50),
#'   stringsAsFactors = FALSE
#' )
#' 
#' coalesce_join(df1, df2, by = "id")
#' 
#' @export
coalesce_join <- function(
    x, y, 
    by = NULL, suffix = c(".x", ".y"), 
    join = dplyr::full_join, ...) {
  
  # Perform the join operation using the specified join function
  joined <- join(x, y, by = by, suffix = suffix, ...)
  
  # Get the union of the column names from both of the joined data frames
  cols <- union(names(x), names(y))
  
  # Identify columns that need to be coalesced (i.e., columns with suffixes)
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  
  # Remove suffixes from the column names and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  # Coalesce columns from the joined data frame (i.e., combine .x and .y columns)
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  # Bind the coalesced columns to the joined data frame and return only the 
  # desired columns
  dplyr::bind_cols(joined, coalesced)[cols]
}
