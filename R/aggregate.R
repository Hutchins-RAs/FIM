#' Aggregate federal and state components 
#'
#' @param df 
#' @param total 
#' @param federal 
#' @param state 
#'
#' @return
#' @export
#'
#' @examples 
#' projections %>%
#'    aggregate(gtfp, gftfp, gstfp) 
aggregate <- function(df, total, federal, state){
  df %>%
    mutate({{total}} := if_else(date > last_hist_date, {{federal}} + {{state}}, {{total}})
    )
}