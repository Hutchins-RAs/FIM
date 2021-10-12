#' Show in excel 
#'
#' @param .data 
#'
#' @return 
#' @export
#'
#' @examples
#' mtcars |>
#'   show_in_excel() |>
#'   dplyr::select(1:2)
show_in_excel <- function(.data){
  if(interactive()){ # avoid unwanted Excel executions
    tmp <- tempfile(fileext = '.csv') #extension embedded directly
    readr::write_excel_csv(.data, tmp) 
    
    fs::file_show(tmp)
  }
  .data # so that we can continue piping
}