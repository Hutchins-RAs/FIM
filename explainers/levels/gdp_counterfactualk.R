cbo <- fim::projections 
data <-
  contributions %>% 
  select(-real_gdp) %>% 
  as_tibble() %>% 
  left_join(cbo %>% select(date, real_gdp = gdph), by = 'date') %>% 
  select(date, gdp = real_gdp, fiscal_impact ) %>% 
  mutate(fiscal_impact = fiscal_impact,
         gdp_growth = (gdp / lag(gdp)),
         gdp_cfct_growth = gdp_growth - fiscal_impact / 400) %>% 
  filter(date >= yearquarter("2019 Q4"),
         date <= yearquarter("2023 Q2")) %>% 
  mutate(gdp_cfct = if_else(date == min(date),
                            gdp,
                            gdp_cfct_growth)) %>% 
  mutate(gdp_cfct = purrr::accumulate(gdp_cfct, `*`)) 


data %>% 
  mutate(gdp_cfct_growth = (gdp_cfct / lag(gdp_cfct)),
         growth_diff = 400 * (gdp_growth - gdp_cfct_growth)) %>% View()

data %>% 
  pivot_longer(c(gdp, gdp_cfct)) %>% 
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line()

contributions %>% 
  as_tibble() %>% 
  left_join(cbo %>% select(date, real_gdph = gdph), by = 'date')

consumption_alt_long %>% 
  filter_index("2020 Q1" ~ "2023 Q2") %>% 
  as_tibble() %>%
  group_by(date, real_gdp) %>% 
  summarise(net = sum(net), .groups = "drop") %>% 
  mutate(gdp_cfct = real_gdp - net) %>% 
  mutate(real_gdp_growth = real_gdp / lag(real_gdp),
         gdp_cfct_growth = gdp_cfct / lag(gdp_cfct),
         growth_diff = (real_gdp_growth - gdp_cfct_growth)) %>% 
  left_join(contributions %>% select(date, fiscal_impact)) %>% 
  mutate(fim_diff = (fiscal_impact - growth_diff) / 400)%>% View()
