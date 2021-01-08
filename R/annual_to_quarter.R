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
      n_distinct()
    q <- glue('Q{rep(1:4, {n})}')
    df %>%
      uncount(4) %>%
      mutate(yq = paste({{year}}, q) %>% 
               yearquarter(fiscal_start = 12)
      ) %>%
      as_tsibble(index = yq)
  }
  df %>%
    year_to_quarter({{var}}) %>%
    mutate(date = glue('{year(yq)}-{month(yq)}') %>% 
             as.yearmon() %>% as_date() + months(1) - days(1),
           yq = yearquarter(yq)
    ) %>%
    select(-{{var}}) %>%
    select(date, yq, everything())
}
