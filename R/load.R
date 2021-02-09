
get_cbo_projections <- function(){
  budget_projections <- 
    readRDS('data/budget_projections.rds') %>%
    as_tsibble(index = fy) %>%
    annual_to_quarter() %>%
    fiscal_to_calendar()
  economic_projections <- 
    readRDS('data/economic_projections.rds') %>%
    mutate(date = yearquarter(date)) %>%
    as_tsibble(index = date)
  
  budget_projections %>%
    dplyr::left_join(economic_projections) %>%
    mutate(id = 'projection') %>%
    mutate(date = lubridate::as_date(date)) %>%
    smooth_budget_series() %>%
    cola_adjustment() %>%
    implicit_price_deflators() %>%
    growth_rates() %>%
    alternative_tax_scenario() %>%
    format_tsibble() 
}
#' Title
#'
#' @return
#' @export
#'
#' @examples
load_unemployment_insurance_override <- function(){
  readxl::read_excel(drake::file_in("data/add-ons/LSFIM_KY_v7.xlsx"),
                     sheet = "FIM Add Factors") %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::select(date, tidyselect::contains('unemployment_insurance'))
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
read_data <- function(){
  historical <- 
    readRDS('data/historical.rds') %>%
    mutate(id = 'historical') %>%
    millions_to_billions() %>%
    rename(cpiu = ui) %>%
    format_tsibble()
  last_date <- 
    historical %>%
    pull(date) %>%
    max()
  projections <- get_cbo_projections()
  
  historical %>%
    coalesce_join(projections, by = 'date') 
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
