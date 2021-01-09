#' Subsidies growth rate assumptions 
#'
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
#'
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
      gfeg_g = gf_g, 
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
#' All growth assumptions
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
growth_assumptions <- function(df){
  df %>%
    purchases_growth() %>%
    transfers_growth() %>%
    health_growth() %>%
    subsidies_growth() %>%
    grants_growth() %>%
    deflators_growth()
  
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
#' We assume that state and local taxes growth with GDP.
#' @param df 
#' Quarterly tax data comes from Haver.
#' @return
#' @export
#'
#' @examples
state_taxes <- function(df){
  df %>% 
    dplyr::left_join(hist %>%
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
