
format_tsibble <- function(df){
  df %>%
    mutate(date = yearquarter(date)) %>%
    relocate(id, .before = date) %>%
    as_tsibble(key = id, index = date)
}

annual_to_quarter <- function(df){
  year <-
    df %>%
      index_var()
  min <-
    df %>%
    select(rlang::enexpr(year)) %>%
    min() 
  
  max <- 
    df %>%
    select(rlang::enexpr(year)) %>%
    max()
  start <- yearquarter(glue::glue('{min} Q1'))
  end <- yearquarter(glue::glue('{max} Q4'))
  x <- seq(start,  end, by = 1)
  
  df %>%
    as_tibble() %>%
    slice(rep(1:n(), each=4)) %>%
    mutate(date = yearquarter(x, fiscal_start =  1)) %>%
    relocate(date, .before =  everything()) %>%
    select(-rlang::enexpr(year)) %>%
    as_tsibble(index = date)
}

fiscal_to_calendar <- function(df){
  index <-
    df %>%
    index_var()  
  index <- rlang::ensym(index)
  df %>%
    mutate("{{index}}" := {{index}} - 1)
    
}  

monthly_to_quarterly <- function(df){
  df %>%
    mutate(yq = tsibble::yearquarter(date)) %>%
    as_tsibble(index = date) %>%
    select(date, yq, everything()) %>%
    index_by(yq) %>%
    mutate(
      across(
        .cols = where(is.numeric), 
        .fns = ~ mean(.x, na.rm = TRUE)
      )
    ) %>%
    filter(row_number()== n()) %>%
    ungroup() %>%
    select(-yq)
}
