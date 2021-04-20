
#'Coalesce missing observations with projected growth rate 
#'
#' @param .data 
#' @param ... 
#'
#' @return
#' @export
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

