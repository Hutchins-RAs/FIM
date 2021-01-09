#' Title
#'
#' @return
#' @export
#'
#' @examples
load_economic_projections <- function(){
  read_xlsx(here('data/raw/cbo/cbo_econ_proj_quarterly.xlsx')) %>%
    mutate(date = as_date(date))
}
#' Title
#'
#' @return
#' @export
#'
#' @examples
load_budget_projections <- function(){
  read_xlsx(here('data/raw/cbo/cbo_budget_nipas_proj_annual.xlsx')) %>%
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
  read_excel("documentation/COVID-19 Changes/September/LSFIM_KY_v6_round2.xlsx", 
             sheet = "FIM Add Factors") %>%
    mutate(date = as_date(date)) %>%
    select(date, contains('unemployment_insurance'))
}
#' Title
#'
#' @return
#' @export
#'
#' @examples
get_haver_data <- function(){
  # Load U.S. national accounts and economic statistics data into the Global Environment
  haver_data_path <-
    here('data/raw/haver/')
  haver_data_names <- 
    haver_data_path  %>%
    list.files() %>%
    .[str_detect(., ".xlsx")]
  
  # Load raw Haver data into global environment
  haver_data_names %>%  
    purrr::map(function(file_name){ # iterate through each file name
      assign(x = str_remove(file_name, ".xlsx"), 
             value = read_xlsx(paste0(haver_data_path,"/", file_name), na = 'NA') %>%
               mutate(date = as.Date(date)),
             envir = .GlobalEnv)
    }) 
  # Merge quarterly and annual data 
  # change hist and aa to haver quarterly and annual
  left_join(national_accounts,
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
  start <- as_date("2000-01-01")
  end <- as_date("2022-12-31")
  
  current_month <- glue('{month(today())}-{year(today())}')
  
  readxl::read_xlsx(glue("results/{get_current_month()}/fim.xlsx")) %>%
    select(date, fiscal_impact, fiscal_impact_moving_average,
           ends_with('cont'), recession) %>%
    mutate(date = as_date(date)) %>% 
    filter(date > start & date <= end)
}
