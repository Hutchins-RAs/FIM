#' Title
#'
#' @param .data 
#' @param ... 
#'
#' @return
#' @import dplyr
#' @export
#'
#' @examples
purchases_contributions2 <- function(.data, ...) {
  # Capture arbitrary list of arguments
  #
  vars <- enquos(...)
  
  .data %>%
    mutate(across(c(!!!vars),
                  ~ 400 * (.x - lag(.x) * (1 + get(
                    paste0(cur_column(), "_deflator_growth")) +
                     real_potential_gdp_growth
                  )) / lag(gdp),
                  .names =  "{.col}_contribution"))
}