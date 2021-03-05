#' Forecast period
#'
#' @param df 
#' @param last_historical_date 
#'
#' @return numbers of rows in projection period
#' @export
#'
#' @examples
get_forecast_period <- function(df, last_historical_date){
  forecastPeriod <- which(df$date > last_historical_date)
  return(forecastPeriod)
}
#' Make forecast
#' Use growth rates to iteratively forecast
#' 
#'
#' @param df 
#' @param forecast_period 
#'
#' @return
#' @export
#'
#' @examples
make_forecasts <- function(df){
  df %>%
    forecast_series(gdp)%>%
    forecast_series(gdph) %>%
    forecast_series(jgdp) %>%
    forecast_series(gdppotq) %>%
    forecast_series(gdppothq) %>%
    forecast_series(g) %>%
    forecast_series(gs) %>%
    forecast_series(gf) %>%
    forecast_series(jgf) %>%
    forecast_series(jgs) %>%
    forecast_series(jgse) %>%
    forecast_series(jgsi) %>%
    forecast_series(gfeg) %>%
    forecast_series(gfeghhx) %>%
    forecast_series(gfeghdx) %>%
    forecast_series(gfeigx) %>%
    forecast_series(gfrpt) %>%
    forecast_series(gsrpt) %>%
    forecast_series(gfrs) %>%
    forecast_series(gsrs) %>%
    forecast_series(gfrcp) %>%
    forecast_series(gsrcp) %>%
    forecast_series(gfrpri) %>%
    forecast_series(gsrpri) %>%
    forecast_series(gftfp) %>%
    forecast_series(gstfp) %>%
    forecast_series(yptmd) %>%
    forecast_series(yptmr) %>%
    forecast_series(gssub) %>%
    forecast_series(gfsub) %>%
    forecast_series(c) %>%
    forecast_series(jc)
}

#' Title
#'
#' @param df 
#' @param comp 
#'
#' @return
#' @export
#'
#' @examples
forecast_series <- function(df, comp){
  comp <- ensym(comp)
  comp_string <- rlang::as_string(enexpr(comp))
  comp_cum_growth_string <- paste0(comp_string, '_g_cumulative_growth')
  comp_cum_growth <- rlang::sym(comp_cum_growth_string)
  comp_forecast <- rlang::sym(paste0(comp_string, '_forecast'))
  df %>%
    group_by(forecast_period) %>%
    mutate('{{comp}}_forecast' := {{comp}} *(!!(comp_cum_growth)),
           '{{comp}}' := if_else(forecast_period == 0,
                                 {{comp}},
                                 {{comp}} * (!!(comp_cum_growth)))) %>%
    ungroup()
}
#' Calculate cumulative growth rate
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
make_cumulative_growth_rates <- function(df){
df %>%
  mutate(
    across(
      .cols = all_of(paste0(c("gdp","gdph","jgdp","gdppotq","gdppothq", "g","gf","gs","jgf",
                              "jgs", "jgse","jgsi","gfeg","gfeghhx","gfeghdx","gfeigx","gfrpt","gsrpt",
                              "gfrs","gsrs","gfrcp","gsrcp","gfrpri","gsrpri","gftfp","gstfp","yptmd","yptmr",
                              "gssub","gfsub","c","jc"),'_g')),
      .fns = ~ cumprod(1 + .),
      .names = '{.col}_cumulative_growth'
    )
  ) %>%
  ungroup()
}
forecast <- function(df){
  gdp <- c('gdp', 'real_gdp', 'gdp_deflator',
                 'real_potential_gdp', 'consumption', 'consumption_deflator')
  purchases <- c('purchases', 'federal_purchases', 'state_purchases',
                  'federal_purchases_deflator', 'state_purchases_deflator')
  grants  <- c('health_grants', 'medicaid_grants', 'investment_grants', 'consumption_grants',
                'consumption_grants_deflator', 'investment_grants_deflator')
  health <- c('medicaid', 'medicare')
  transfers <- c('social_benefits', 'subsidies')  %>% government_levels()
  taxes <- c('personal_taxes', 'production_taxes', 'payroll_taxes', 'corporate_taxes')
  federal_taxes <- taxes  %>% as_federal()
  state_taxes <- taxes %>% as_state()
  variables <- c(gdp, purchases, grants,  health, transfers, federal_taxes) 
  
  df %>%
    tidyr::fill(variables) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(dplyr::across(all_of(glue::glue('{variables}_growth')),
                  ~  get_cumulative_growth(.x),
                  .names = "{.col}_cumulative"
                  )) %>%
    ungroup() %>%
    mutate(
           dplyover::over(all_of(variables),
                ~ if_else(id == 'projection', 
                          lag(.("{.x}")) * (.("{.x}_growth_cumulative")),
                          .("{.x}"))
           )
    ) %>%
    mutate(
           across(all_of(state_taxes),
                  ~ if_else(id == 'projection',
                            zoo::na.locf(. / gdp) * gdp,
                            .)
                  )
    )
    
}




get_cumulative_growth <- function(x){
  
  x <- cumprod(1 + x)
  return(x)
}

government_levels <- function(x){
  c(glue::glue('federal_{x}'), glue::glue('state_{x}'))
}

as_federal <- function(x){
  x <- glue::glue('federal_{x}')
  return(x)
}

as_state <- function(x){
  x <- glue::glue('state_{x}')
  return(x)
}
