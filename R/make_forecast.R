#' Forecast period
#'
#' @return numbers of rows in projection period
#' @export
#'
#' @examples
get_forecast_period <- function(df){
  forecastPeriod <- which(df$date > last_hist_date)
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
make_forecast <- function(df) {
  for(f in get_forecast_period(df)) {
    df[f,components] = df[f-1, components]  * (1 + df[f, paste0(components, "_g")])
  }
}