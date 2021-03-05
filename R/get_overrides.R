get_overrides <- function(df){
  variables <-
    df %>% 
    as_tibble() %>% 
    select(ends_with('override')) %>% 
    names() %>% 
    str_remove('_override')
  df %>%  
    mutate(dplyover::over(all_of(variables),
                          ~ coalesce(.('{.x}_override'), .('{.x}'))
    )
    )
}
