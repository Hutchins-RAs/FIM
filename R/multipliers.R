#' Multipliers
#' Wrapper around roll::roll_sum() with sensible defaults
#' @param x 
#' @param weights 
#'
#' @return
#' @export
#'
#' @examples
multipliers <- function(x, weights){
  roll::roll_sum(x, width = length(weights), weights = rev(weights), online = FALSE, min_obs = 1)
}