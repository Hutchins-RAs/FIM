
#' Subsidies growth rate assumptions 
#' 
#' Grow subsidies with real potential  GDP
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
subsidies_growth <- function(df){
  df %>% 
    mutate(
      gfsub_g = gdppothq_g,
      gssub_g = gdppothq_g
    )
}
#' Purchases assumptions
#' 
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
purchases_growth <- function(df){
  capExpiration <- "2021-09-30"
  df %>%
    mutate(gf_g = if_else(date > capExpiration,
                          gdppothq_g + jgdp_g,
                          gf_g)
    )
}
#' Transfers assumptions
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
transfers_growth <- function(df){
  df %>%
    mutate(
      gftfpnet_g = gftfp_g, 
      gstfp_g = gs_g,
      gstfpnet_g =  gs_g,
    )
}
#' Health outlays growth assumptions 
#'  Grow Federal Medicaid grants to states and Health & Hospital grants
#'  with Medicaid 
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
health_growth <- function(df){
  df %>%
    mutate(
      #  Health & Hospital grants to states growth with Medicaid
      gfeghhx_g = yptmd_g, 
      # Medicaid grants to states grow with medicaid
      gfeghdx_g = yptmd_g, 
    )
}
#' Grants growth rate assumptions 
#' 
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
grants_growth <- function(df){
  df %>%
    mutate(
      # Aid to S&L grow with federal purchases
      grants_growth = federal_purchases_growth, 
      # Capital grants to state and local gov'ts grow with federal purchases
      gfeigx_g = gf_g
    )
}
#' Deflators growth rate assumptions
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
deflators_growth <- function(df){
  df %>%
    mutate(
      jgsi_g = jgs_g,
      # Consumption deflator grows with overall deflator for S&L
      jgse_g = jgs_g
    )
}


create_override <- function(df, var, start, end, values){
  start <- tsibble::yearquarter(start)
  end <- tsibble::yearquarter(end)
  override <- 
    tibble(date = df %>%
            filter_index(start ~ end) %>% 
             pull(date),
           '{{var}}' := values
    )
  df %>%
    rows_update(override, by = 'date')
}

#' All growth assumptions
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
growth_assumptions <- function(df){

  
  

  cap_expiration <- tsibble::yearquarter('2021 Q3')
  df %>%
    mutate(subsidies_growth = real_potential_gdp_growth,
           federal_subsidies_growth = real_potential_gdp_growth,
           state_subsidies_growth = real_potential_gdp_growth,
           
           federal_purchases_growth = if_else(date > cap_expiration,
                                              real_potential_gdp_growth + gdp_deflator_growth,
                                              federal_purchases_growth),
           
           
           
           federal_social_benefits_growth = federal_social_benefits_growth,
           state_social_benefits_growth = state_purchases_growth,
           
           health_grants_growth = medicaid_growth,
           medicaid_grants_growth = medicaid_growth,
           
           grants_growth = federal_purchases_growth,
           investment_grants_growth = federal_purchases_growth,
           consumption_grants_growth  =  federal_purchases_growth,
           consumption_deflator_growth = consumption_deflator_growth,
           consumption_grants_deflator_growth = coalesce(consumption_grants_deflator_growth, state_purchases_deflator_growth), 
           investment_grants_deflator_growth = coalesce(investment_grants_deflator_growth, state_purchases_deflator_growth))
}
#' Override state purchases
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
state_purchases_growth_override <- function(df){
  #   projections %>% filter(date < '2020-12-31') %>% count(n()) %>% pull()
  #   rate <- c(rep(0.0025,3), 0.005,0.0075,0.01)
  #   projections %>% mutate(replacement_rate = if_else(date >= '2020-12-31' & date <= '2022-03-31',
  #   )
}

#' State taxes
#'
#' We assume that S&L taxes growth with GDP.
#' @param df 
#' Quarterly tax data comes from Haver.
#' @return
#' @export
#'
#' @examples
state_taxes <- function(df){
  df %>% 
    dplyr::left_join(load_haver_data() %>%
                dplyr::select('date', 'gsrpt', 'gsrpri', 'gsrcp', 'gsrs'),
              all.x = F) %>%
    dplyr::filter(date > '2016-12-31') %>%
    dplyr::mutate(
      dplyr::across(
        .cols = tidyselect::all_of(c("gsrpt", "gsrpri", "gsrcp", "gsrs")),
        .fns = ~ zoo::na.locf(. / gdp) * gdp
      )
    )
}
