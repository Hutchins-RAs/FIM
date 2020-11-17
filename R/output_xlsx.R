#' @title Output xlsx
#' @description Export multiple xlsx files
#' @param data list of data frame objects to iterate over
#' @param names list of names for data frames
#' @return Export excel workbooks
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname output_xlsx
#' @export 
output_xlsx <- function(data, names){ 
  folder_path <- paste0("results/", thismonth, '/')
  write_xlsx(data, paste0(folder_path, names, ".xlsx"))
}
