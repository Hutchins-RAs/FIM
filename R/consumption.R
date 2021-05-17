#' Get dollars of consumptions
#'
#' @param mpc numeric
#' @param timing numeric
#' @name consumption
NULL
#> NULL


#' @rdname consumption 
consumption  <- function(mpc =  1, timing){
  rlang::new_function(
    rlang::exprs(x = ),
    rlang::expr({
      !!mpc  * roll::roll_sum(x, width = length(!!timing), weights = rev(!!timing),
                              online = FALSE, min_obs = 1)
    }),
    rlang::caller_env()
  )
}

#' @rdname consumption
consumption_social_benefits <- consumption(0.9, timing = rep(1/4, 4))

#' @rdname consumption
consumption_coronavirus_relief_fund <- consumption(timing = c(0.06, 0.08, rep(0.1, 2), rep(0.08, 8)))

#' @rdname consumption
consumption_rebate_checks <- consumption(0.7, c(rep(0.08, 6), 0.15, 0.35))

#' @rdname consumption
consumption_health_outlays <- consumption(0.9, rep(1/4, 4))

#' @rdname consumption 
consumption_corporate_taxes <- consumption(-0.4, rep(1/12, 12))

#' @rdname consumption 
consumption_non_corporate_taxes <- consumption(-0.6, c(rep(0.2, 2), rep(0.1, 6)))

#' @rdname consumption 
consumption_subsidies <- consumption(0.45, timing = c(0.11, 0.095, 0.09, 0.085, rep(0.08, 4), rep(0.075, 4)))

#' @rdname consumption 
consumption_subsidies_second_draw <- function(x){
  consumption <- 0.525
  weights <- c(rep(0.0625, 4), rep(0.0750, 4), 0.0875, 0.1, 0.1125, 0.15)
  consumption * roll::roll_sum(x, width = length(weights),
                       weights = weights, online = FALSE)
}

#' @rdname consumption 
consumption_subsidies_rra <- consumption(0.525, timing = c(0.1125, 0.1, 0.0875, rep(0.075, 4), rep(0.0625, 4)))

#' @rdname consumption 
consumption_ui <- consumption(0.9, timing = c(rep(0.35, 2), rep(0.1, 2), rep(0.05, 2)))

#' Rename consumption 
#' Ad hoc function to rename column from 'minus_neutral' to 'post_consumption'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
rename_consumption <- function(df){
  df %>% 
    rename_with(~paste0(str_remove(., 'minus_neutral'), 'post_consumption'),
                .cols = contains('minus_neutral')) 
}

consumption_taxes_transfers <- function(df){
  df %>% 
    consumption_ui() %>% 
    consumption_social_benefits() %>% 
    consumption_health_outlays() %>% 
    consumption_subsidies()  %>% 
    consumption_rebate_checks() %>% 
    consumption_corporate_taxes() %>% 
    consumption_non_corporate_taxes() %>% 
    rename_consumption()
  
}


get_all_consumption <- function(df){
  transfers <- paste0('consumption_', c('subsidies', 'ui'))
  variables <- transfers%>% all_levels() %>% paste0('_minus_neutral')
  
  consumption_fun <- eval(sym(glue::glue('consumption_{transfers}'))) 
  
}

#' Calculate consumption's
#' Create consumption
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
calculate_consumption <- function(df, taxes_transfers){
  
  net <- 'minus_neutral'
  government_levels <- c(glue::glue('{taxes_transfers}_{net}'), glue::glue('federal_{taxes_transfers}_{net}'),
                         glue::glue('state_{taxes_transfers}_{net}'))
  total <- glue::glue('{taxes_transfers}_post_consumption')
  federal <- glue::glue('federal_{taxes_transfers}_post_consumption')
  state <- glue::glue('state_{taxes_transfers}_post_consumption')
  
  if(taxes_transfers == 'subsidies'){

    consumption_fun <- eval(sym(glue::glue('consumption_{taxes_transfers}')))
    consumption_fun_second_draw <- eval(sym(glue::glue('consumption_{taxes_transfers}_second_draw')))
    df %>%
      dplyr::mutate(
        dplyr::across(
          .cols = tidyselect::all_of(government_levels),
          .fns = ~ if_else(date < tsibble::yearquarter('2021 Q1'), 
                           consumption_fun(.),
                           consumption_fun_second_draw(.)),
          .names = '{.col}_consumption'
        )
      ) %>%
      dplyr::rename(!!total := glue::glue('{taxes_transfers}_{net}_consumption'),
                    !!federal := glue::glue('federal_{taxes_transfers}_{net}_xconsumption'),
                    !!state := glue::glue('state_{taxes_transfers}_{net}_xconsumption'))
  }
  else{
    consumption_fun <- eval(sym(glue::glue('consumption_{taxes_transfers}')))
    df %>%
      dplyr::mutate(
        dplyr::across(
          .cols = tidyselect::all_of(government_levels),
          .fns = ~ consumption_fun(.),
          .names = '{.col}_consumption'
        )
      ) %>%
      dplyr::rename(!!total := glue::glue('{taxes_transfers}_{net}_consumption'),
                    !!federal := glue::glue('federal_{taxes_transfers}_{net}_consumption'),
                    !!state := glue::glue('state_{taxes_transfers}_{net}_consumption'))
  }
}

manu <- function(.data, .x){

  
  if(.x == 'subsidies'){
    
    consumption_fun <- eval(sym(glue::glue('consumption_{taxes_transfers}')))
    consumption_fun_second_draw <- eval(sym(glue::glue('consumption_{taxes_transfers}_second_draw')))
    .data %>%
      dplyr::mutate(
        dplyr::across(
          .cols = tidyselect::all_of(government_levels),
          .fns = ~ if_else(date < tsibble::yearquarter('2021 Q1'), 
                           consumption_fun(.),
                           consumption_fun_second_draw(.)),
          .names = '{.col}_consumption'
        )
      ) 
  }
    else{
      consumption_fun <- eval(sym(glue::glue('consumption_{.x}')))
      .data %>%
        dplyr::mutate(
          dplyr::across(
            .cols = tidyselect::all_of(all_levels(.x)),
            .fns = ~ consumption_fun(.),
            .names = '{.col}_consumption'
          )
        ) 
    
  }
}


