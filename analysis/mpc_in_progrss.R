contribution %>% 
  map(c('ui', 'health_outlays'), 
         calculate_mpc)
mtcars %>% modify_at(c("drat", "qsec", "gear"), as.character)

df <- tibble(federal_ui = runif(10, 1, 100))
mpc <- function(.data, .x){
 
}
df %>% 
  mpc(x)

foo <- function(.data, cols){
funcs <-   sym(paste0('mpc_', enexpr(cols)))  #mpc_ui, mpc_social_benefits

.data <- as_tibble(.data)
output <- summarise(.data,
          date, 
          !!paste0(enexpr(cols), '_spending') := eval(funcs)({{cols}}),
          {{cols}}) 

if(is_tsibble(.data)){
  index <- tsibble::index_var(.data)
  output %>% as_tsibble(index = index)
} else{
  output
}
  


}

foo(contribution, health_outlays)

contribution %>% 
  purrr::modify_at(vctrs::vec_c('ui', 'social_benefits',
                                'health_outlays'), foo)

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