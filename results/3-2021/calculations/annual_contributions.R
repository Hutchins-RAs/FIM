tar_load(fim) 
yoy <- function (x){
  j = c()
  for (i in 4:length(x)) {
    j[i] = (((x[i] / x[i - 4])) - 1) * 100
  }
  j[1] = 0
  j
}

fim %>% 
  select(date, gdp, fiscal_impact, taxes_transfers_cont, federal_nom, fiscal_impact_moving_average) %>% 
  mutate(date, gdp, 
            
            taxes_transfers_level = taxes_transfers_cont * lag(gdp),
            roll_avg = roll::roll_mean(taxes_transfers_level, width = 4, min_obs = 1, online = TRUE),
            growth = yoy(roll_avg ) / 100,
            fim_level = fiscal_impact * lag(gdp),
            fim_roll_avg = roll::roll_mean(fim_level, width = 4, min_obs = 1, online = TRUE),
            fim_growth = yoy(fim_roll_avg) / 100)  %>% 
  filter(date > '2019-12-31') %>% 
  
  mutate(
    taxes_transfers_level = 0.25 * taxes_transfers_cont * lag(gdp),
    taxes_transfers_level_growth = yoy(taxes_transfers_level) / 100,
    fim_level = 0.25 * fiscal_impact * lag(gdp) ,
    fim_level_growth = yoy(fim_level) / 100) %>% 
  filter(date > '2019-12-31') %>% 
  select(date, fim_growth, fiscal_impact_moving_average) %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line()
