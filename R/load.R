

get_cbo_projections <- function(){
  fim::projections %>% 
    cola_adjustment() %>%
    smooth_budget_series() %>%
    implicit_price_deflators() %>%
    growth_rates() %>%
    alternative_tax_scenario() %>%
    format_tsibble() %>% 
    select(id, date, gdp, gdph, gdppothq, gdppotq, starts_with('j'), dc, c, ch ,ends_with('growth'), cpiu, federal_ui, state_ui, unemployment_rate)
}

safe_quarter  <- function(df){
  df %>% 
    as_tibble() %>% 
    mutate(date = as.character(date))
}

undo_safe_quarter <- function(df){
  df %>% 
    mutate(date = yearquarter(date)) %>% 
    as_tsibble(index = 'date')
}
#' Title
#'
#' @return
#' @export
#'
#' @examples
read_data <- function(){

  projections <- get_cbo_projections()
  
  fim::national_accounts %>%
    coalesce_join(projections, by = 'date') %>% 
    as_tsibble(key = id, index = date)
}



#' Load contributions used in figures
#'
#' @return
#' @export
#'
#' @examples
load_contributions <- function(){
  start <- lubridate::as_date("2000-01-01")
  end <- lubridate::as_date("2022-12-31")
  

  
  readxl::read_xlsx(glue::glue(drake::file_in("results/{get_current_month()}/fim-{get_current_month()}.xlsx"))) %>%
    dplyr::select(date, fiscal_impact, fiscal_impact_moving_average,
           tidyselect::ends_with('cont'), recession) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>% 
    dplyr::filter(date > start & date <= end)
}
