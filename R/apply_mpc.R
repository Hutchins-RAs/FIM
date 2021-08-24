

#' Title
#'
#' @param data 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
mpc_apply <- function(data, timing_file = NULL, ...){
  vars <- rlang::enquos(..., .named = TRUE)
  timing <- timing_file %||% read_mpc_file()
  vars <- purrr::map(vars,
                     function(var) {
                       rlang::eval_tidy(rlang::expr(roll::roll_sum(
                         {{var}},
                         weights = rev(get_timing(timing, {{var}})),
                         width = length(get_timing(timing, {{var}})),
                         online = FALSE,
                         min_obs = 1
                       )),
                       data = data)
                     })
  names(vars) <- paste0(names(vars), '_consumption')
  
  data %>%
    dplyr::mutate(!!!vars, .before = 'gdp')
  
  #rlang::eval_tidy(call, data = data)
}