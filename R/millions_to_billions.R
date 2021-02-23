#' Millions to billions 
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
millions_to_billions <- function(df){
  df %>% 
    dplyr::mutate(
      dplyr::across(.cols = any_of(c('gftfbusx', 'gfeghhx', 'gfeghdx', 'gfeigx')),
             .fns = ~ . / 1000)
    )
}