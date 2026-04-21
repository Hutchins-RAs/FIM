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


# GDP counterfactual using FIM --------------------------------------------
library('ggbrookings')
theme_set(theme_brookings())
update_geom_defaults('line', list(size = 1.5))
data %>% 
  mutate(gdp_cfct_growth = (gdp_cfct / lag(gdp_cfct)),
         growth_diff = 400 * (gdp_growth - gdp_cfct_growth)) %>% View()

data %>% 
  pivot_longer(c(gdp, gdp_cfct)) %>% 
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  scale_color_brookings(labels = c('Real GDP', 'Real GDP Counterfactual')) + 
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_x_yearquarter(date_breaks = '3 months') +
  labs(title = 'Real GDP and counterfactual with no fiscal impact',
       x = '', 
       y = 'Billions')

contributions %>% 
  as_tibble() %>% 
  left_join(cbo %>% select(date, real_gdph = gdph), by = 'date')

consumption_alt_long %>% 
  filter_index("2020 Q1" ~ "2023 Q2") %>% 
  as_tibble() %>%
  group_by(date, real_gdp, gdp, gdp_deflator) %>% 
  summarise(net = sum(net), .groups = "drop") %>% 
  mutate(gdp_deflator = gdp_deflator / 100) %>% 
  mutate(gdp_cfct = (gdp - net) / gdp_deflator) %>% 
  mutate(real_gdp_growth = real_gdp / lag(real_gdp),
         gdp_cfct_growth = gdp_cfct / lag(gdp_cfct),
         growth_diff = (real_gdp_growth - gdp_cfct_growth)) %>% 
  left_join(contributions %>% select(date, fiscal_impact)) %>% 
  mutate(fim_diff = (fiscal_impact - growth_diff * 400))%>% 
  openxlsx::write.xlsx('explainers/levels/figures/gdp_cfct.xlsx',
                       overwrite = TRUE)


data2 <-
  consumption_alt_long %>% 
  filter_index("2019 Q4" ~ "2023 Q2") %>% 
  as_tibble() %>%
  group_by(date, real_gdp, gdp, gdp_deflator) %>% 
  summarise(net = sum(net), .groups = "drop") %>% 
  mutate(gdp_deflator = gdp_deflator / 100) %>% 
  mutate(gdp_cfct = (gdp - net) / gdp_deflator) %>% 
  mutate(real_gdp_growth = real_gdp / lag(real_gdp),
         gdp_cfct_growth = gdp_cfct / lag(gdp_cfct),
         growth_diff = (real_gdp_growth - gdp_cfct_growth)) 

data2 %>% 
  mutate_if(is.character, forcats::as_factor) %>% 
  pivot_longer(c(gdp_cfct, real_gdp)) %>% 
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  scale_color_brookings(labels = c('Real GDP Counterfactual', 'Real GDP'),
                        reverse = FALSE)  +
  scale_y_continuous(labels = scales::label_dollar()) +
  labs(title = 'Real GDP and counterfactual GDP in which pre-pandemic fiscal policy grows with potential',
       x = '', 
       y = 'Billions') 

ggsave(
  here::here('explainers/levels/figures', 'pre-pandemic-fiscal.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  bg = '#FAFAFA',
  device = ragg::agg_png()
)



# Merge graphs ------------------------------------------------------------

data2 %>% 
  select(date, real_gdp, gdp_cfct2 = gdp_cfct) %>% 
  left_join(data %>% select(date, gdp_cfct1  = gdp_cfct)) %>% 
  filter(date > yearquarter("2019 Q4")) %>% 
  pivot_longer(c(real_gdp, gdp_cfct1, gdp_cfct2)) %>% 
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  scale_color_brookings(labels = c('Counterfactual derived from FIM', 'Counterfactual with fiscal policy growing at potential', 'Real GDP'),
                        reverse = FALSE) + 
  scale_y_continuous(labels = scales::label_dollar()) +
  scale_x_yearquarter(date_breaks = '6 months') 


data2 %>% 
  select(date, real_gdp, gdp_cfct2 = gdp_cfct) %>% 
  left_join(data %>% select(date, gdp_cfct1  = gdp_cfct)) %>% 
  filter(date > yearquarter("2019 Q4")) %>% 
  openxlsx::write.xlsx('explainers/levels/cfct_comparison.xlsx', overwrite = TRUE)
GDP cfct 2: Fiscal policy growing w/potential"