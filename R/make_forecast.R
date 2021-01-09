#' Title
#'
#' @return
#' @export
#'
#' @examples
make_forecast <- function(){
  for(f in get_forecast_period()){
    projections[f,components] = projections[f-1, components]  * (1 + projections[f, paste0(components, "_g")])
  }
}