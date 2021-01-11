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
    dplyr::filter(date >= "1999-12-31") %>% 
    dplyr::mutate(
      yrq = zoo::as.yearqtr(date),
      projection = dplyr::if_else(historical == 1,
                           0,
                           1),
      taxes_transfers_subsidies_cont = taxes_transfers_cont
    ) %>%
    tidyr::separate(yrq, c("year", "quarter")) %>%
    dplyr::select(year, quarter, fim_bars_ma, recession,
           fim_bars, 
           federal_cont, state_local_cont,
           taxes_transfers_subsidies_cont,
           projection) %>%
    dplyr::rename(
      "total" = fim_bars,
      "impact" = fim_bars_ma,
      "federal" = federal_cont,
      "state_local" = state_local_cont,
      "consumption" = taxes_transfers_subsidies_cont
    ) %>% 
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
