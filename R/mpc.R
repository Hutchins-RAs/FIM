# MPC Functions -------------------------------------------------------------------------------
### Transfers -----------------------------------------------------------------------------------
#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
mpc_social_benefits = function(df){
  df %>% 
    mutate(
      across(
        .cols = all_levels('social_benefits_minus_neutral'),
        .fns = ~ 0.9 * rollapply(.x, width = 4, mean, partial = TRUE, fill = NA, align =  'right')
      )
    ) 
}

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
mpc_rebate_checks <- function(df){
  mpc <- 0.7
  weights <- c(rep(0.08, 6), 0.15, 0.35)
  
  df %>% 
    mutate(
      across(
        any_of(all_levels('rebate_checks_minus_neutral')),
        ~ mpc * roll::roll_sum(.x, width = length(weights), 
                               weights = weights,
                               online = FALSE)
      )
    )

}

#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
mpc_health_outlays = function(df){
  mpc <- 0.9
  
  df %>% 
    mutate(
      across(
        all_levels('health_outlays_minus_neutral'),
        ~  mpc * rollapply(.x, width = 4, mean, fill = NA, align =  'right')
      )
    )
  
}



# 
# mpc_factory <- build_factory(
#   function(x,  ...){
#     mpc  * roll::roll_sum(x, online = FALSE, ...)
#   },
#   mpc,
#   online = FALSE,
#   fill = NA,
#   align = 'right',
#   .pass_dots = TRUE
# )

# mpc_subsidies_second_draw_exp <- mpc_factory(0.525, width = 12, weights =  c(rep(0.0625, 4), rep(0.0750, 4), 0.0875, 0.1, 0.1125, 0.15))




### Taxes ---------------------------------------------------------------------------------------
#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
mpc_corporate_taxes <- function(df){
  mpc <- -0.4
  df %>% 
    mutate(
      across(
        any_of(all_levels('corporate_taxes')),
        ~ mpc * zoo::rollapply(.x, width = 12, mean, fill = NA, align =  'right')
      )
    )
}
#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
mpc_non_corporate_taxes <- function(df){
  mpc <- -0.6
  weights <- c(rep(0.1, 6), rep(0.2, 2))
  df %>% 
    mutate(
      across(
        (all_levels('non_corporate_taxes_minus_neutral')),
        ~ mpc * roll::roll_sum(.x, width = length(weights),
                               weights = weights, online = FALSE)
      )
    )
}


#' Marginal propensity to consume for Subsidies 
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
mpc_subsidies  <- function(df){
  
  mpc <- 0.45
  weights <- c(rep(0.075,4), rep(0.08,4), 0.085, 0.09, 0.095, 0.11)
  
  mpc_second_draw <- 0.525
  weights_second_draw <- c(rep(0.0625, 4), rep(0.0750, 4), 0.0875, 0.1, 0.1125, 0.15)

  second_draw_start <- tsibble::yearquarter('2021 Q1')
  df %>% 
     dplyr::mutate(
       dplyr::across(
         .cols  = all_levels('subsidies_minus_neutral'), 
         ~ if_else(date < second_draw_start,
                   mpc * roll::roll_sum(.x, width = length(weights), 
                                        weights = weights, online = FALSE),
                      mpc_second_draw * roll::roll_sum(.x, width = length(weights_second_draw),
                                            weights = weights_second_draw, online = FALSE)),
         .names = "{.col}"
       )
     ) 
}



#' Marginal propensity to consume for Subsidies 
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
mpc_ui <- function(df){
  weights <- c(rep(0.05, 2), rep(0.1, 2), rep(0.35, 2))
  df %>% 
    mutate(
      across(all_levels('ui_minus_neutral'),
             ~ get_mpc(0.9, .x, weights = weights, width = length(weights), online = FALSE))
    ) 
}
#' Rename mpc 
#' Ad hoc function to rename column from 'minus_neutral' to 'post_mpc'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
rename_mpc <- function(df){
  df %>% 
  rename_with(~paste0(str_remove(., 'minus_neutral'), 'post_mpc'),
              .cols = contains('minus_neutral')) 
}
get_mpc <- function(mpc, ...){
  dots <- list(...)
  mpc * roll::roll_sum(...)
}

get_fun <- function(df, x){
 fun <- eval(sym(paste0('mpc_', {{x}})))
return(fun)
}



foo3 <- function(.data, ...){
  user_exprs <- enexprs(...)
  funs <- list(syms(paste0('mpc_', user_exprs)))
  
  .data %>% map_df(~invoke_map(funs, ,.), .id="id")
}

foo2 <- function(x){
  function_names <- eval(sym(paste0('mpc_', x)))
  return(function_names)
  
  
}
mpc_transfers <- function(df, ...){
  user_exprs <- enexprs(...)
  function_names <- syms(paste0('mpc_', user_exprs))
  
  variables <- all_levels(user_exprs)
  mpc_functions <- eval(sym(glue::glue('mpc_{variables}')))

}

mpc_taxes_transfers <- function(df){
  df %>% 
    mpc_ui() %>% 
    mpc_social_benefits() %>% 
    mpc_health_outlays() %>% 
    mpc_subsidies()  %>% 
    mpc_rebate_checks() %>% 
    mpc_corporate_taxes() %>% 
    mpc_non_corporate_taxes() %>% 
    rename_mpc()
    
}


get_all_mpc <- function(df){
  transfers <- paste0('mpc_', c('subsidies', 'ui'))
  variables <- transfers%>% all_levels() %>% paste0('_minus_neutral')
 
    mpc_fun <- eval(sym(glue::glue('mpc_{transfers}'))) 
     
}

#' Calculate mpc's
#' Create mpc
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
calculate_mpc <- function(df, taxes_transfers){
  
  net <- 'minus_neutral'
  government_levels <- c(glue::glue('{taxes_transfers}_{net}'), glue::glue('federal_{taxes_transfers}_{net}'),
                         glue::glue('state_{taxes_transfers}_{net}'))
  total <- glue::glue('{taxes_transfers}_post_mpc')
  federal <- glue::glue('federal_{taxes_transfers}_post_mpc')
  state <- glue::glue('state_{taxes_transfers}_post_mpc')
  
  if(taxes_transfers == 'subsidies'){
    second_draw <- tsibble::yearquarter('2021 Q1')
    mpc_fun <- eval(sym(glue::glue('mpc_{taxes_transfers}')))
    mpc_fun_second_draw <- eval(sym(glue::glue('mpc_{taxes_transfers}_second_draw')))
    df %>%
      dplyr::mutate(
        dplyr::across(
          .cols = tidyselect::all_of(government_levels),
          .fns = ~ if_else(date < second_draw, 
                           mpc_fun(.),
                           mpc_fun_second_draw(.)),
          .names = '{.col}_xmpc'
        )
      ) %>%
      dplyr::rename(!!total := glue::glue('{taxes_transfers}_{net}_xmpc'),
             !!federal := glue::glue('federal_{taxes_transfers}_{net}_xmpc'),
             !!state := glue::glue('state_{taxes_transfers}_{net}_xmpc'))
  }
  else{
    mpc_fun <- eval(sym(glue::glue('mpc_{taxes_transfers}')))
    df %>%
      dplyr::mutate(
        dplyr::across(
          .cols = tidyselect::all_of(government_levels),
          .fns = ~ mpc_fun(.),
          .names = '{.col}_xmpc'
        )
      ) %>%
      dplyr::rename(!!total := glue::glue('{taxes_transfers}_{net}_xmpc'),
             !!federal := glue::glue('federal_{taxes_transfers}_{net}_xmpc'),
             !!state := glue::glue('state_{taxes_transfers}_{net}_xmpc'))
  }
}

