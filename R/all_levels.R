#' Return column name at all levels of govt 
#'
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
all_levels <- function(...){
  
  get_levels <- function(string){
    c(string, as_federal(string), as_state(string))
  }
  l <- list(...)
  lapply(l, get_levels) %>% 
    unlist()
}