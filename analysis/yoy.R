yoy <- function (x){
  j = c()
  for (i in 4:length(x)) {
    j[i] = (((x[i] / x[i - 4])) - 1) * 100
  }
  j[1] = j[5]
  j[2] = j[5]
  j[3] = j[5]
  j[4] = j[5]
}

read_data() %>%
  define_variables() %>%
  as_tsibble(key = id, index = date) %>%
  mutate_where(id == 'historical',
               real_potential_gdp_growth = q_g_fourthlag(real_potential_gdp)) 


read_data() %>%
  define_variables() %>%
  as_tsibble(key = id, index = date) %>%
  mutate_where(id == 'historical',
               real_potential_gdp_growth = yoy(real_potential_gdp)) %>% 
  select(real_potential_gdp_growth)
