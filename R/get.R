#' Calculate fiscal impact
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
get_fiscal_impact <- function(df){
  df %>%
    mutate(fiscal_impact = federal_cont + state_cont + taxes_transfers_cont,
           fim_bars = fiscal_impact,
           fiscal_impact_moving_average = SMA(na.locf(fiscal_impact, na.rm = F), n=4),
           fim_bars_ma = fiscal_impact_moving_average
    )
}
#' Retrieve contributions of transfers
#'
#' @return
#' @export
#'
#' @examples
get_transfers <- function(){
  fim %>%
    taxes_contributions() %>%
    sum_taxes_contributions() %>%
    transfers_contributions() %>%
    sum_transfers_contributions() %>%
    sum_taxes_transfers() %>% 
    select(date, transfers_cont, social_benefits_cont, health_outlays_cont,
           subsidies_cont, unemployment_insurance_cont, rebate_checks_cont,
           federal_transfers_cont, federal_social_benefits_cont,
           federal_health_outlays_cont, federal_subsidies_cont,
           federal_unemployment_insurance_cont, federal_rebate_checks_cont,
           state_transfers_cont, state_social_benefits_cont,
           state_health_outlays_cont, state_subsidies_cont,
           state_unemployment_insurance_cont, state_rebate_checks_cont) %>% 
    mutate(sum = federal_transfers_cont + state_transfers_cont,
           diff = transfers_cont - sum) %>% filter(date > '2019-12-31') 
}
#' Retrieve contribution of taxes
#'
#' @return
#' @export
#'
#' @examples
get_taxes <- function(){
  fim %>%
    taxes_contributions() %>%
    sum_taxes_contributions() %>%
    filter(date > '2019-12-31') %>%
    select(date, taxes_cont, corporate_taxes_cont, 
           noncorp_taxes_cont, federal_taxes_cont, 
           federal_corporate_taxes_cont, 
           federal_noncorp_taxes_cont,
           state_taxes_cont, state_corporate_taxes_cont,
           state_noncorp_taxes_cont) %>%
    mutate(total = taxes_cont, 
           sum = federal_taxes_cont + state_taxes_cont,
           diff = total - sum)
}

#' Current month
#' Helper function to retrieve the current months date. 
#' This is particularly helpful when navigating across the
#' results and reports directories.
#' @return
#' @export
#'
#' @examples
get_current_month <- function(){
  current_date <- lubridate::today()
  glue(month(current_date),'-',year(current_date))
}

#' Previous month
#' Helper function to retrieve the previous months date. 
#' This is particularly helpful when navigating across the
#' results and reports directories.
#' @return
#' @export
#'
#' @examples
get_previous_month <- function(){
  previous_date <- lubridate::today() - months(1)
  glue(month(previous_date), '-', year(previous_date))
}

#' Get upper bound for y axis
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
max_y <- function(df){
  df %>%
    select(fiscal_impact) %>%
    max() %>%
    ceiling() + 1  
}

#' Skim contributions
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
skim_contributions <- function(df){
  df %>%
    select(date, ends_with('cont')) %>% 
    filter(date > '2019-12-31') %>%
    skimr::skim()
}



#' Title
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
get_last_hist_date <- function(df) { 
  df %>%
  pull(date) %>%
  max()
}