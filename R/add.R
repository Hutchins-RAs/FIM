#' Get dollars of consumptions
#'
#' @param mpc numeric
#' @param timing numeric
#' @name consumption
NULL
#> NULL



#' @rdname consumption 
mpc  <- function(mpc =  1, timing){
  rlang::new_function(
    rlang::exprs(x = ),
    rlang::expr({
      !!mpc  * roll::roll_sum(x, width = length(!!timing), weights = rev(!!timing),
                              online = FALSE, min_obs = 1)
    }),
    rlang::caller_env()
  )
}