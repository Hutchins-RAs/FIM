#' Title
#'
#' @return
#' @export
#'
#' @examples
load_economic_projections <- function(){
  readxl::read_xlsx(here::here('data/raw/cbo/cbo_econ_proj_quarterly.xlsx')) %>%
    dplyr::mutate(date = lubridate::as_date(date))
}
#' Title
#'
#' @return
#' @export
#'
#' @examples
load_budget_projections <- function(){
  readxl::read_xlsx(here('data/raw/cbo/cbo_budget_nipas_proj_annual.xlsx')) %>%
    as_tsibble(index = fy)
}
#' Title
#'
#' @return
#' @export
#'
#' @examples
load_cbo_projections <- function(){
  budget <- load_budget_projections()
  economic <- load_economic_projections()
  
  budget %>%
    annual_to_quarter(fy) %>%
    left_join(economic)
}
#' Title
#'
#' @return
#' @export
#'
#' @examples
load_unemployment_insurance_override <- function(){
  readxl::read_excel("documentation/COVID-19 Changes/September/LSFIM_KY_v6_round2.xlsx", 
             sheet = "FIM Add Factors") %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::select(date, contains('unemployment_insurance'))
}
#' Title
#'
#' @return
#' @export
#'
#' @examples
load_national_accounts <- function(){
  readxl::read_xlsx(here::here('data/raw/haver/national_accounts.xlsx'))
}
#' Title
#'
#' @return
#' @export
#'
#' @examples
load_economic_statistics <- function(){
  readxl::read_xlsx(here::here('data/raw/haver/economic_statistics.xlsx'))
}
#' Title
#'
#' @return
#' @export
#'
#' @examples
load_haver_data <- function(){
  national_accounts <- load_national_accounts()
  economic_statistics <- load_economic_statistics()
  dplyr::left_join(national_accounts,
            economic_statistics,
            by = "date") 
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
  
  current_month <- glue('{month(today())}-{year(today())}')
  
  readxl::read_xlsx(glue("results/{get_current_month()}/fim.xlsx")) %>%
    select(date, fiscal_impact, fiscal_impact_moving_average,
           ends_with('cont'), recession) %>%
    mutate(date = lubridate::as_date(date)) %>% 
    filter(date > start & date <= end)
}
