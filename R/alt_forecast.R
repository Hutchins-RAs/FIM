
#'Coalesce missing observations with projected growth rate 
#'
#' @param .data 
#' @param ... 
#'
#' @return
#' 
#'
#' @examples
coalesce_growth<-function(.data,...){
  vars <- enquos(...)
  .data %>% 
    dplyr::mutate(dplyr::across(c(!!!vars),
                  ~ dplyr::coalesce(.x, 1 + get(paste0(dplyr::cur_column(), "_growth")))))
}

#' Forecast using projected growth rates
#'
#' @param .data 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
forecast2  <- function(.data, ...){
  vars  <- enquos(...)

  .data %>% 
    coalesce_growth(!!!vars) %>% 
    dplyr::filter(dplyr::between(dplyr::row_number(), dplyr::last(which(id == 'historical')), n())) %>% 
    dplyr::select(date, id, !!!vars) %>% 
    dplyr::mutate(dplyr::across(where(is.numeric),  ~ purrr::accumulate(.x, `*`))) %>%
    coalesce_join(.data, by = 'date')
}


#' Project series  with a growth rate
#' 
#' @param .data 
#' @param ... 
#' @param with 
#'
#' @return
#' @export
#'
#' @examples
# project <- function(.data, ..., with){
#   vars <- enquos(...)
#   with <- enquo(with)
#   .data %>% 
#     dplyr::mutate(dplyr::across(c(!!!vars), 
#                   ~ dplyr::if_else(id == 'projection',
#                             NA_real_,
#                             .x))) %>% 
#     dplyr::mutate(dplyr::across(c(!!!vars),
#                                 ~ dplyr::coalesce(.x, 1 + {{ with }}))) %>% 
#     dplyr::filter(dplyr::between(dplyr::row_number(), dplyr::last(which(id == 'historical')), n())) %>% 
#     dplyr::mutate(dplyr::across(c(!!!vars),  ~ purrr::accumulate(.x, `*`))) %>% 
#     coalesce_join(.data, by = c('date', 'id'))
# }

project <- function(.data, ..., with = NULL, from = NULL){
  
  
  ### Setup
  from <- from %||% (yearquarter(Sys.Date()) - 1)
  from <- yearquarter(from)
  vars <- enquos(...)
  with <- enquo(with) %||% purrr::modify(cols, ~glue::glue('{.x}_growth')) 
  key  <- tsibble::key_vars(.data)
  
  ### Ensure that forecast period is filled with NA's  and then the desired growth rate
  .data %>% 
    dplyr::mutate(dplyr::across(c(!!!vars), 
                                ~ dplyr::if_else(date > from,
                                                 !!with,
                                                 .x))) %>% 
    dplyr::filter(date >= from)  %>% 
    ### Forecast 
    dplyr::mutate(dplyr::across(c(!!!vars), ~ purrr::accumulate(.x, `*`))) %>% 
    coalesce_join(.data, by = c('date', key))
}


