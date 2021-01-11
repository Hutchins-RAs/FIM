#' Include add factors
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
add_factors <- function(df){
  #load add factor file
  add_factors <- readxl::read_excel("documentation/COVID-19 Changes/September/LSFIM_KY_v6_round2.xlsx", 
                            sheet = "FIM Add Factors") %>%
    mutate(
      date = lubridate::as_date(date)
    ) 
  df %>% 
    dplyr::full_join(add_factors %>% dplyr::select(-tidyselect::ends_with('override')) %>% 
                       filter(date > '2020-09-30'),
              by = "date") %>%
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::starts_with('add_'),
      .fns = ~ if_else(is.na(.x), 
                       0,
                       .x)
    )
    ) %>%
    dplyr::mutate(
      #calculate new variables by adding the add factors
      state_health_outlays  = state_health_outlays + add_state_health_outlays,
      state_social_benefits  = state_social_benefits + add_state_social_benefits,
      state_noncorp_taxes  =  state_noncorp_taxes + add_state_noncorp_taxes,
      state_corporate_taxes  = state_corporate_taxes + add_state_corporate_taxes,
      
      federal_health_outlays  = federal_health_outlays + add_federal_health_outlays,
      federal_social_benefits  = federal_social_benefits + add_federal_social_benefits,
      # federal_noncorp_taxes  = federal_noncorp_taxes + add_federal_noncorp_taxes,
      # federal_corporate_taxes  = federal_corporate_taxes + add_federal_corporate_taxes,
      federal_subsidies  = federal_subsidies + add_federal_subsidies,
      federal_cgrants = federal_cgrants + add_federal_cgrants,
      
      #new category totals
      health_outlays  = state_health_outlays  + federal_health_outlays ,
      social_benefits  = state_social_benefits  + federal_social_benefits ,
      noncorp_taxes  = state_noncorp_taxes  + federal_noncorp_taxes ,
      corporate_taxes  = state_corporate_taxes  + federal_corporate_taxes ,
      subsidies   = state_subsidies + federal_subsidies,
      state_local_nom = state_local_nom + add_state_purchases,
      federal_nom = add_federal_purchases + federal_nom,
      federal_rebate_checks = federal_rebate_checks + add_rebate_checks,
      rebate_checks = rebate_checks + add_rebate_checks
    )
}