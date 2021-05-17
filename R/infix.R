#' Title
#'
#' @param lhs 
#' @param rhs 
#'
#' @return
#' @export
#'
#' @examples
`%||%` <- function(lhs, rhs){
  if(!is.null(lhs)){
    lhs
  } else {
    rhs
  }
}