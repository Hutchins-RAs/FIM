#' Convert annual data to quarterly
#'
#' @param df 
#' @param var 
#'
#' @return
#' @export
#'
#' @examples
annual_to_quarter <- function(df, var){
  
  year_to_quarter <- function(df, year){
    n <- 
      df %>%
      dplyr::n_distinct()
    q <- glue::glue('Q{rep(1:4, {n})}')
    df %>%
      tidyr::uncount(4) %>%
      dplyr::mutate(yq = base::paste({{year}}, q) %>% 
               yearquarter(fiscal_start = 12)
      ) %>%
      tsibble::as_tsibble(index = yq)
  }
  df %>%
    year_to_quarter({{var}}) %>%
    dplyr::mutate(date = glue::glue('{year(yq)}-{month(yq)}') %>% 
             zoo::as.yearmon() %>% lubridate::as_date() + lubridate::months(1) - lubridate::days(1),
           yq = tsibble::yearquarter(yq)
    ) %>%
    dplyr::select(-{{var}}) %>%
    dplyr::select(date, yq, tidyselect::everything())
}
