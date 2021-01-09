#' Forecast period
#'
#' @return numbers of rows in projection period
#' @export
#'
#' @examples
get_forecast_period <- function(df, last_historical_date){
  forecastPeriod <- which(df$date > last_historical_date)
  return(forecastPeriod)
}
#' Title
#' 
#' 
#'
#' @return
#' @export
#'
#' @examples
make_forecast <- function(df, forecast_period, components) {
  for(f in forecast_period) {
    df[f,components] = df[f-1, components]  * (1 + df[f, paste0(components, "_g")])
  }
}