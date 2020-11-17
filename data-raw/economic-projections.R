if (!require("pacman")) install.packages("pacman") 
pacman::p_load(readxl, magrittr, dplyr, usethis, here, data.table)

taxpieces = c('gsrpt' ,'gsrpri', 'gsrcp' ,'gsrs')

econ <-
  read_xlsx(here::here("inst",'extdata', 'cbo_econ_proj_quarterly.xlsx')) %>%
  mutate(date =  gsub("12/30/", "12/31/", date)) %>%
  mutate(date = as.Date(date)) 

  econ %<>%
  # Implicit price deflators
  mutate(jgf =  gf/gfh,
         jgs = gs/gsh,
         jc = c/ch)  %>%
  # Growth rates
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~ q_g(.x),
      .names = "{.col}_g"
    )
  ) %>%
# S&L Taxes
left_join(haver %>%
         select(date, all_of(taxpieces)),
       all.x = F) %>%
# Growth rate of S&L Taxes
mutate(
  across(
    .cols = all_of(taxpieces),
    .fns = ~ q_g(.),
    .names = "{.col}_g"
  )
) 

  econ %>%
    mutate(gsrpt_g = if_else(date > last(haver$date),
                             gdp_g,
                             gsrpt_g))
taxpieces_growth <- paste0(taxpieces, '_g')
  econ %>%
    mutate(across(
      .cols = all_of(taxpieces_growth),
      .fns = ~ if_else(date > last(haver$date), gdp_g, .)
      
    ))

usethis::use_data(econ, overwrite = TRUE)