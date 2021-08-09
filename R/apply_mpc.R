#' Title
#'
#' @param path 
#' @param sheet 
#' @param range 
#'
#' @return
#' @export
#'
#' @examples
read_mpc_file <- function(path = 'data/forecast.xlsx', sheet = 'mpc', range = 'B2:N14'){
  readxl::read_excel(path = path, 
                     sheet = sheet, range = range) %>% 
    tidyr::pivot_longer(-variable,
                        names_to = 'lag') %>% 
    tidyr::pivot_wider(names_from = variable,
                       values_from = value) %>% 
    dplyr::select(-lag)
}



#' Title
#'
#' @param var 
#' @param data 
#'
#' @return
#' @export
#' @importFrom rlang enquo as_label 
#'
#' @examples
get_timing <- function(timing, var){

  var <- enquo(var)
  string <- as_label(var)
  
  args <- switch(as_label(var),
                 # TRANSFERS
                 
                 health_outlays = timing$health_outlays,
                 federal_health_outlays = ,
                 state_health_outlays = ,
                 health_outlays_counterfactual = ,
                 federal_health_outlays_counterfactual = ,
                 state_health_outlays_counterfactual = ,
                 
                 social_benefits =  timing$social_benefits,
                 federal_social_benefits = ,
                 state_social_benefits = ,
                 social_benefits_counterfactual =  timing$social_benefits,
                 federal_social_benefits_counterfactual = ,
                 state_social_benefits_counterfactual = ,
                 
                 
                 subsidies = timing$subsidies,
                 federal_subsidies = ,
                 state_subsidies = ,
                 subsidies_counterfactual = timing$subsidies,
                 federal_subsidies_counterfactual = ,
                 state_subsidies_counterfactual = ,
                 
                 
                 ui = timing$ui,
                 federal_ui = ,
                 state_ui = ,
                 ui_counterfactual = ,
                 federal_ui_counterfactual = ,
                 state_ui_counterfactual = ,
                 
                 ui_arp = timing$ui_arp,
                 federal_ui_arp = ,
                 state_ui_arp = ,
                 ui_arp_counterfactual = timing$ui_arp,
                 federal_ui_arp_counterfactual = ,
                 state_ui_arp_counterfactual = ,
                 
                 
                 rebate_checks = timing$rebate_checks,
                 rebate_checks_arp = timing$rebate_checks_arp,
                 rebate_checks_counterfactual = timing$rebate_checks,
                 rebate_checks_arp_counterfactual = timing$rebate_checks_arp,
                 
                 corporate_taxes = timing$corporate_taxes,
                 federal_corporate_taxes = ,
                 state_corporate_taxes = ,
                 corporate_taxes_counterfactual = timing$corporate_taxes,
                 federal_corporate_taxes_counterfactual = ,
                 state_corporate_taxes_counterfactual = ,
                 
                 non_corporate_taxes = timing$non_corporate_taxes,
                 federal_non_corporate_taxes = ,
                 state_non_corporate_taxes = ,
                 non_corporate_taxes_counterfactual = timing$non_corporate_taxes,
                 federal_non_corporate_taxes_counterfactual = ,
                 state_non_corporate_taxes_counterfactual = ,
                 
                 federal_aid_to_small_businesses_arp = timing$aid_to_small_businesses_arp,
                 federal_other_direct_aid_arp = timing$other_direct_aid_arp,
                 federal_other_vulnerable_arp = timing$other_vulnerable_arp,
                 federal_aid_to_small_businesses_arp_counterfactual = timing$aid_to_small_businesses_arp,
                 federal_other_direct_aid_arp_counterfactual = timing$other_direct_aid_arp,
                 federal_other_vulnerable_arp_counterfactual = timing$other_vulnerable_arp)
  
  return(args)
}

#' Title
#'
#' @param data 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
mpc_apply <- function(data, timing_file = NULL, ...){
  vars <- rlang::enquos(..., .named = TRUE)
  timing <- timing_file %||% read_mpc_file()
  vars <- purrr::map(vars,
                     function(var) {
                       rlang::eval_tidy(rlang::expr(roll::roll_sum(
                         {{var}},
                         weights = rev(get_timing(timing, {{var}})),
                         width = length(get_timing(timing, {{var}})),
                         online = FALSE,
                         min_obs = 1
                       )),
                       data = data)
                     })
  names(vars) <- paste0(names(vars), '_consumption')
  
  data %>%
    dplyr::mutate(!!!vars, .before = 'gdp')
  
  #rlang::eval_tidy(call, data = data)
}