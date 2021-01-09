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
#' @param components 
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