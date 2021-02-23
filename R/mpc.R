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
mpc_social_benefits = function(x){
  0.9 * rollapply(x, width = 4, mean, partial = TRUE, fill = NA, align =  'right')
}
#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
mpc_ui <- function(x){
  mpc <- 0.9
  weights <- c(rep(0.05, 2), rep(0.1, 2), rep(0.35, 2))
  mpc * roll::roll_sum(x, width = length(weights), 
                       weights = weights, online = FALSE)
}
#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
mpc_rebate_checks <- function(x){
  mpc <- 0.7
  weights <- c(rep(0.08, 6), 0.15, 0.35)
  mpc * roll::roll_sum(x, width = length(weights),
                       weights = weights, online = FALSE)
}
#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
mpc_subsidies <- function(x) {
  mpc <- 0.45
  weights <- c(rep(0.075,4), rep(0.08,4), 0.085, 0.09, 0.095, 0.11)
  mpc * roll::roll_sum(x, width = length(weights), 
                       weights = weights, online = FALSE)
}
#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
mpc_health_outlays = function(x){
  0.9 * rollapply(x, width = 4, mean, fill = NA, align =  'right')
}
#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
mpc_subsidies_second_draw <- function(x){
  mpc <- 0.525
  weights <- c(rep(0.0625, 4), rep(0.0750, 4), 0.0875, 0.1, 0.1125, 0.15)
  mpc * roll::roll_sum(x, width = length(weights),
                       weights = weights, online = FALSE)
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
mpc_corporate_taxes <- function(x){
  mpc <- -0.4
  mpc * zoo::rollapply(x, width = 12, mean, fill = NA, align =  'right')
}
#' Title
#'
#' @param x 
#'
#' @return
#' @export
#'
#' @examples
mpc_noncorp_taxes <- function(x){
  mpc <- -0.6
  weights <- c(rep(0.1, 6), rep(0.2, 2))
  mpc * roll::roll_sum(x, width = length(weights),
                       weights = weights, online = FALSE)
}

mpc_non_corporate_taxes <- mpc_noncorp_taxes
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

