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
  mutate(forecast_period = if_else(date <= last_hist_date, 0, 1)) %>%
  group_by(forecast_period) %>%
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