#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param series PARAM_DESCRIPTION
#' @param database PARAM_DESCRIPTION
#' @param start.date PARAM_DESCRIPTION, Default: '1970-01-01'
#' @param frequency PARAM_DESCRIPTION, Default: 'quarterly'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname pull_data
#' @export 
pull_data <- function(series, database, start.date = '1970-01-01', frequency = "quarterly") {
  q <- haver.data(series, database, eop.dates = T, start = as.Date(start.date, 
                                                                   f = "%m-%d-%Y"))
  q <- data.frame(date = as.Date(rownames(q)), q)
  
  for (j in 2:ncol(q)) {
    for (k in 4:nrow(q)) {
      if (is.na(q[k, j])) {
        q[k, j] = mean(q[c(k - 1, k - 2, k - 3), j])  # if data is missing use 3-qtr moving average
        
      }
    }
  }
  q
}
