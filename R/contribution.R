#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param .data data frame with components needed to calculate contributions
#' @param var  federal_nom, state_local_nom, federal_cgrants, federal_igrants
#' @return Contribution of var to real gdp growth
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso 
#'  \code{\link[rlang]{as_string}},\code{\link[rlang]{sym}}
#' @rdname contribution
#' @export 
#' @importFrom rlang as_string sym
contribution <- function(.data, var){
  var <- ensym(var) # quote expression
  var_string <- rlang::as_string(enexpr(var)) # convert quoted expression to string
  deflator_string <- paste0(var_string, "_pi") # create string with the name of relevant deflator
  deflator <- rlang::sym(deflator_string) # convert deflator string to symbol
  
  ## Calculate contribution
  .data %>%
    mutate(
      "{{ var }}_cont" := 400 * ({{ var }} - (1  + !!(deflator) + gdppoth) * lag({{ var }}) ) / lag(gdp)
    ) %>%
    select(date, !!paste0(var_string, "_cont"))
}
