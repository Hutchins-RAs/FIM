#' Load contributions used in figures
#'
#' @return
#' @export
#'
#' @examples
load_contributions <- function(){
  start <- as_date("2000-01-01")
  end <- as_date("2022-12-31")
  
  current_month <- glue('{month(today())}-{year(today())}')
  
  readxl::read_xlsx(glue("results/{get_current_month()}/fim.xlsx")) %>%
    select(date, fiscal_impact, fiscal_impact_moving_average,
           ends_with('cont'), recession) %>%
    mutate(date = as_date(date)) %>% 
    filter(date > start & date <= end)
}