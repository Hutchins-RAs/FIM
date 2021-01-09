#' Title
#'
#' @return
#' @export
#'
#' @examples
make_forecast <- function(){
  for(f in get_forecast_period()){
    prepare_projections[f,components] = prepare_projections[f-1, components]  * (1 + prepare_projections[f, paste0(components, "_g")])
  }
}