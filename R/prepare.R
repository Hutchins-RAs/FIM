#' Prepare data for fim web interactive 
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
prepare_interactive <- function(df){
  df %>% 
    tsibble::filter_index("1999 Q4" ~ .) %>% 
    dplyr::mutate(
      yrq = date,
      projection = dplyr::if_else(id == "historical",
                           0,
                           1)
    ) %>%
    tidyr::separate(yrq, c("year", "quarter")) %>%
    dplyr::select(year, quarter, impact = fiscal_impact_moving_average, recession,
           total = fiscal_impact, 
           federal  = federal_contribution, 
           state_local = state_contribution,
           consumption = taxes_transfers_contribution,
           projection) %>%
    dplyr::mutate(recession = dplyr::if_else(is.na(recession),
                               0,
                               recession))
}
#' Prepare results for figures
#'
#' @return
#' @export
#'
#' @examples
prepare_results <- function(){
  # Create folder for current month's update
  
  dir.create(here('results', fim::get_current_month()))
  
  # Write csv to current month's folder
  results <- 
    list(fim = fim,
         fim_interactive = fim_interactive,
         projections = projections)
  
  output_xlsx <- function(data, names){ 
    write_xlsx(data, glue('results/{fim::get_current_month()}/{names}.xlsx'))
  }
  
  list(data = results, 
       names = names(results)) %>%
    purrr::pmap(output_xlsx) 
}
