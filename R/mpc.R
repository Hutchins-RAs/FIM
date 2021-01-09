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
mpc_unemployment_insurance <- function(x){
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
  mpc * rollapply(x, width = 12, mean, fill = NA, align =  'right')
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
