#' Millions to billions 
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples gftfbusx
millions_to_billions <- function(df){
  df %>% 
    mutate(
      across(.cols = c('gftfbusx', 'gfeghhx', 'gfeghdx', 'gfeigx'),
             .fns = ~ . / 1000)
    )
}