#' Sum federal and state projections
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
#'    sum_projections(gtfp, gftfp, gstfp) 

sum_projections <- function(df, total, federal, state){
  df %>%
    mutate({{total}} := if_else(date > last_hist_date, {{federal}} + {{state}}, {{total}})
    )
}
#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
sum_taxes_contributions <- function(df){
  taxes <- c('noncorp_taxes', 'corporate_taxes')
  df %>%
    mutate(
      taxes_cont = rowSums(select(., .dots = all_of(str_glue('{taxes}_cont')))),
      federal_taxes_cont = rowSums(select(., .dots = all_of(str_glue('federal_{taxes}_cont')))),
      state_taxes_cont = rowSums(select(., .dots = all_of(str_glue('state_{taxes}_cont'))))
    )
}
#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
sum_transfers_contributions <- function(df){
  transfers <- c('social_benefits',  'health_outlays', 'subsidies',
                 'unemployment_insurance', 'rebate_checks')
  df %>%
    mutate(
      transfers_cont = rowSums(select(., .dots = all_of(str_glue('{transfers}_cont'))), na.rm = TRUE),
      federal_transfers_cont = rowSums(select(., .dots = all_of(str_glue('federal_{transfers}_cont'))), na.rm = TRUE),
      state_transfers_cont = rowSums(select(., .dots = all_of(str_glue('state_{transfers}_cont'))), na.rm = TRUE)
    )
}
#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
sum_taxes_transfers <- function(df){
  df %>%
    mutate(
      taxes_transfers_cont = taxes_cont + transfers_cont,
      federal_taxes_transfers_cont = federal_taxes_cont + federal_transfers_cont,
      state_taxes_transfers_cont = state_taxes_cont + state_transfers_cont,
    )
}


