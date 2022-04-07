#' Include add factors
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
add_factors_deprecated <- function(df, last_date){
  #load add factor file
  add_factors <- readxl::read_excel("data/add-ons/LSFIM_KY_v7.xlsx", 
                            sheet = "FIM Add Factors") %>%
    mutate(
      date = lubridate::as_date(date)
    ) 
  df %>% 
    dplyr::full_join(add_factors %>% dplyr::select(-tidyselect::ends_with('override')) %>% 
                       filter(date > last_date),
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
      
      federal_health_outlays  = federal_health_outlays + add_federal_health_outlays,
      federal_social_benefits  = federal_social_benefits + add_federal_social_benefits,
      # federal_noncorp_taxes  = federal_noncorp_taxes + add_federal_noncorp_taxes,
      # federal_corporate_taxes  = federal_corporate_taxes + add_federal_corporate_taxes,
      federal_subsidies  = federal_subsidies + add_federal_subsidies,
      federal_cgrants = federal_cgrants + add_federal_cgrants,
      state_local_nom = state_local_nom + add_state_purchases,
      federal_nom = add_federal_purchases + federal_nom,
      
      
      #new category totals
      health_outlays  = state_health_outlays  + federal_health_outlays ,
      social_benefits  = state_social_benefits  + federal_social_benefits ,
      noncorp_taxes  = state_noncorp_taxes  + federal_noncorp_taxes ,
      corporate_taxes  = state_corporate_taxes  + federal_corporate_taxes ,
      subsidies   = state_subsidies + federal_subsidies,
      federal_rebate_checks = federal_rebate_checks + add_rebate_checks,
      rebate_checks = rebate_checks + add_rebate_checks
    )
}

#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
# add_factors <- function(df){
#   #load add factor file
#   
#   state <- c('health_outlays', 'social_benefits') %>% as_state()
#   federal <- c('health_outlays', 'social_benefits', 'subsidies') %>% as_federal()
#   other <- c('consumption_grants', 'rebate_checks')
#   my_variables <- c(federal, state, other)
  # add_factors <- readxl::read_excel("inst/extdata/add_factors.xlsx",
  #                                   sheet = "FIM Add Factors") %>%
    # mutate(date = tsibble::yearquarter(date)) %>%
    # as_tsibble(index = date)
  # df %>%
  #   dplyr::full_join(add_factors,
  #                    by = "date") %>%
  #   dplyr::mutate(dplyr::across(
  #     .cols = tidyselect::starts_with('add_'),
  #     .fns = ~ coalesce(.x, 0)
  #   ),
#     dplyover::over(all_of(my_variables),
#                    ~ if_else(id == 'projection', 
#                              .("{.x}") + .("add_{.x}"),
#                              .("{.x}"))
#     )
#     
#     ) %>% 
#     dplyr::mutate(
# 
#       
#       
#       #new category totals
#       health_outlays  = state_health_outlays  + federal_health_outlays ,
#     )
# }


add_factors <- function(df) {
  add_factors <- readxl::read_excel("inst/extdata/add_factors.xlsx",
                                    sheet = "FIM Add Factors") %>%
    mutate(date = tsibble::yearquarter(date)) %>%
    as_tsibble(index = date)

  df %>%
    dplyr::full_join(add_factors,
                     by = "date") %>%
    dplyr::mutate(dplyr::across(
      .cols = tidyselect::starts_with('add_'),
      .fns = ~ coalesce(.x, 0)
    ),
    dplyr::across(
      .cols = tidyselect::starts_with('add_'),
      .fns = ~ if_else(id == 'historical',
                       0,
                       .x)
    )) %>% dplyr::mutate(
    state_health_outlays = state_health_outlays +
      add_state_health_outlays,
    state_social_benefits = state_social_benefits +
      add_state_social_benefits,
    federal_health_outlays = federal_health_outlays +
      add_federal_health_outlays,
    federal_social_benefits = federal_social_benefits +
      add_federal_social_benefits,
    federal_subsidies = federal_subsidies +
      add_federal_subsidies,
    consumption_grants = consumption_grants + add_consumption_grants,
    state_purchases = state_purchases +
      add_state_purchases,
    federal_purchases = add_federal_purchases +
      federal_purchases,
    health_outlays = state_health_outlays +
      federal_health_outlays,
    social_benefits = state_social_benefits +
      federal_social_benefits,
    subsidies = state_subsidies +
      federal_subsidies,
    rebate_checks = rebate_checks + add_rebate_checks
  )
}

