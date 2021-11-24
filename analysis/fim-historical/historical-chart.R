source('analysis/fim-historical/fiscal_impact_history.R')


# Subset data
headline <-
  contributions %>%
  select(date, fiscal_impact, fiscal_impact_moving_average) %>%
  pivot_longer(fiscal_impact) %>% 
  filter(date <= yearquarter("2023 Q2"))



expanded <- 
  contributions %>%
  summarise(
    date,
    federal_contribution,
    state_contribution,
    taxes_transfers_contribution = transfers_contribution + federal_corporate_taxes_contribution +
      federal_non_corporate_taxes_contribution + state_corporate_taxes_contribution + state_non_corporate_taxes_contribution,
    fiscal_impact_moving_average
  ) %>%
  pivot_longer(cols = ends_with('contribution'),
               names_to = 'variable') %>% 
  filter(date <= yearquarter("2023 Q2"))
# Legend formatting
guidez <- guides(
  fill = guide_legend(
    keywidth = unit(0.8, "cm"),
    keyheight = unit(0.4, "cm"),
    ncol = 1
  ),
  colour = guide_legend(
    keywidth = unit(0.8, "cm"),
    keyheight = unit(0.05, "cm"),
    ncol = 1
  )
)


# Recessions data
recessions <-
  history %>%
  as_tibble() %>%
  select(date, recession = recessq) %>%
  mutate(
    date,
    diff = recession - dplyr::lag(recession),
    business_cycle = case_when(
      diff == 2 ~ 'recession_start',
      diff == -2 ~ 'recession_end',
      recession == 1 ~ 'recession',
      recession == -1 ~ 'expansion'
    ),
    .keep = 'used'
  ) %>%
  filter(business_cycle == 'recession_start' |
           business_cycle == 'recession_end') %>%
  pivot_longer(business_cycle) %>%
  mutate(date2 = as_date(date)) %>%
  pivot_wider(names_from = value,
              values_from = date) %>%
  select(recession_start, recession_end) %>%
  mutate(
    across(any_of(c(
      'recession_start', 'recession_end'
    )),
    .fns = ~ coalesce(.x, dplyr::lead(.x))),
    recession_end = dplyr::lead(recession_end),
    .keep = 'used'
  ) %>%
  unique() %>%
  drop_na()


# Normal chart ------------------------------------------------------------

# Headline
headline %>% 
  ggplot() + 
  geom_hline(yintercept = 0) +
  geom_rect(data = recessions,
            aes(xmin = recession_start, 
                xmax = recession_end, 
                ymin = -Inf,
                ymax = +Inf),
            fill = 'grey',
            alpha = 0.5) +
  geom_col(aes(x = date, y = value, fill = name),
           width = 50) +
  geom_line(aes(x = date, 
                y = fiscal_impact_moving_average,
                colour = "4-quarter moving-average"),
            size = 0.5) +
  geom_point(aes(x = date,
                 y = fiscal_impact_moving_average,
                 colour = "4-quarter moving-average"),
             size = 0.5) +
  ggtext::geom_richtext(aes(x = yearquarter(today()) + 8,
                            y = 13),
                        label = "Projection",
                        family = 'roboto',
                        size = 3,
                        face = 'bold',
                        cex = 2,
                        fill = NA, 
                        label.color = NA) +
  fim::scale_fill_fim(palette = 'headline',
                      labels = " Quarterly fiscal impact")  +
  scale_color_manual(" ",
                     values = c("4-quarter moving-average" = "black")) +
  annotate("rect", xmin = yearquarter('2021 Q3'), xmax = yearquarter('2023 Q2'),
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'yellow') +
  guides(fill = guide_legend(keywidth = unit(0.8, "cm"),
                             keyheight = unit(0.4, "cm"),
                             ncol = 1),
         colour = guide_legend(keywidth = unit(0.8, "cm"),
                               keyheight = unit(0.05, "cm"),
                               ncol = 1)) +
  fim::fim_theme() +
  guides(fill = guide_legend(keywidth = unit(0.8, "cm"),
                             keyheight = unit(0.4, "cm"),
                             ncol = 1),
         colour = guide_legend(
           keywidth = unit(0.8, "cm"),
           keyheight = unit(0.05, "cm"),
           ncol = 1
         ))+
  labs(title = glue("**Hutchins Center Fiscal Impact Measure: Total**"),
       x = NULL,
       y = NULL,
       subtitle = "Fiscal Policy Contribution to Real GDP Growth, percentage points",
       caption = "**Source:** Hutchins Center calculations from Bureau of Economic Analysis 
        and Congressional Budget Office data; grey shaded areas indicate recessions 
        and yellow shaded areas indicate projection.") +
  scale_x_yearquarter(breaks = yearquarter("1960 Q1") + seq(0, 256, by = 4 * 5),
                      date_labels = "%Y",
                      expand = expansion(0.0375)) +
  scale_y_continuous(breaks = seq(-6, 18, by = 2),
                     labels = scales::label_percent(scale = 1, accuracy = 1)) +
  theme(panel.grid = ggplot2::element_line(
    colour = "#FFFFFF")) 

ggsave(
  here::here('analysis/fim-historical/figures', 'headline.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  bg = '#FFFFFF'
)


# Components



ggplot(data = expanded) +
  geom_hline(yintercept = 0) +
  geom_rect(data = recessions,
            aes(xmin = recession_start, 
                xmax = recession_end, 
                ymin=-Inf,
                ymax=+Inf),
            fill = 'grey',
            alpha = 0.5) +
  geom_col(aes(x = date, y = value, fill = variable),
           width = 50) +
  geom_line(aes(x = date,
                y = fiscal_impact_moving_average,
                colour = "4-quarter moving-average"),
            size = 0.5) +
  geom_point(aes(x = date,
                 y = fiscal_impact_moving_average,
                 colour = "4-quarter moving-average"),
             size = 0.5) +
  ggtext::geom_richtext(aes(x = yearquarter(today()) + 8,
                            y = 13),
                        label = "Projection",
                        family = 'roboto',
                        size = 3,
                        face = 'bold',
                        cex = 2,
                        fill = NA, 
                        label.color = NA) +
  
  
  scale_fill_fim(palette = 'expanded',
                 labels = c(" Federal Purchases",
                            " State & Local Purchases",
                            " Taxes, Transfers, & Subsidies")) +
  scale_color_manual(" ", 
                     values=c("4-quarter moving-average" ="black")) +
  annotate("rect", xmin = yearquarter('2021 Q3'), xmax = yearquarter('2023 Q2'),
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = 'yellow') +
  # scale_x_yearquarter(breaks = 0, date_breaks = "2 years", date_labels = '%Y Q%q',
  #              expand = c(0,0)) + 
  
  guides(
    fill = guide_legend(
      keywidth = unit(0.8, "cm"),
      keyheight = unit(0.4, "cm"),
      ncol = 1
    ),
    colour = guide_legend(
      keywidth = unit(0.8, "cm"),
      keyheight = unit(0.05, "cm"),
      ncol = 1
    )
  ) +
  fim::fim_theme() +
  labs(title = glue("**Hutchins Center Fiscal Impact Measure: Components**"),
       x = NULL,
       y = NULL,
       subtitle = "Fiscal Policy Contribution to Real GDP Growth, percentage points",
       caption = "**Source:** Hutchins Center calculations from Bureau of Economic Analysis 
        and Congressional Budget Office data; grey shaded areas indicate recessions 
        and yellow shaded areas indicate projection.") +
  scale_y_continuous(breaks = seq(-18, 28, by = 2),
                     labels = scales::label_percent(scale = 1, accuracy = 1)) +
  theme(panel.grid = ggplot2::element_line(
    colour = "#FFFFFF"))  +
  scale_x_yearquarter(breaks = yearquarter("1960 Q1") + seq(0, 256, by = 4 * 5),
                      date_labels = "%Y",
                      expand = expansion(0.0375)) 

ggsave(
  here::here('analysis/fim-historical/figures', 'components.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  
  bg = '#FFFFFF'
)

# Chart -------------------------------------------------------------------
librarian::shelf(ggbrookings)
theme_set(theme_brookings())
update_geom_defaults(geom = 'line', list(size = 1.5))
update_geom_defaults(geom = 'point', list(size = 2))


# Headline
headline %>% 
  ggplot() + 
  geom_hline(yintercept = 0) +
  geom_rect(data = recessions,
            aes(xmin = recession_start, 
                xmax = recession_end, 
                ymin = -Inf,
                ymax = +Inf),
            fill = 'grey',
            alpha = 0.5) +
  geom_line(aes(x = date, y = value, color = name),
            size = 0.75) +
  ggtext::geom_richtext(aes(x = yearquarter(today()) + 8,
                            y = 13),
                        label = "Projection",
                        family = 'roboto',
                        size = 3,
                        face = 'bold',
                        cex = 2,
                        fill = NA, 
                        label.color = NA) +
  scale_color_brookings(labels = " Quarterly fiscal impact") +
  annotate("rect", xmin = yearquarter('2021 Q3'), xmax = yearquarter('2023 Q2'),
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'yellow') +
  labs(title = glue("**Hutchins Center Fiscal Impact Measure: Total**"),
       x = NULL,
       y = NULL,
       subtitle = "Fiscal Policy Contribution to Real GDP Growth, percentage points",
       caption = "**Source:** Hutchins Center calculations from Bureau of Economic Analysis 
        and Congressional Budget Office data; grey shaded areas indicate recessions 
        and yellow shaded areas indicate projection.") +
  scale_x_yearquarter(breaks = yearquarter("1960 Q1") + seq(0, 256, by = 4 * 5),
                      date_labels = "%Y",
                      expand = expansion(0.0375)) +
  scale_y_continuous(breaks = seq(-6, 18, by = 2),
                     labels = scales::label_percent(scale = 1, accuracy = 1)) +
  theme(legend.position = 'none')

ggsave(
  here::here('analysis/fim-historical/figures', 'headline_alternative.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  
  bg = '#FFFFFF'
)

# Components alternative

ggplot(data = expanded) +
  geom_rect(data = recessions,
            aes(xmin = recession_start, 
                xmax = recession_end, 
                ymin=-Inf,
                ymax=+Inf),
            fill = 'grey',
            alpha = 0.5) +
  geom_area(aes(x = date, y = value, fill = variable),
           width = 50) +

  ggtext::geom_richtext(aes(x = yearquarter(today()) + 8,
                            y = 13),
                        label = "Projection",
                        family = 'roboto',
                        size = 3,
                        face = 'bold',
                        cex = 2,
                        fill = NA, 
                        label.color = NA) +
  
  
  scale_fill_fim(palette = 'expanded',
                 labels = c(" Federal Purchases",
                            " State & Local Purchases",
                            " Taxes, Transfers, & Subsidies")) +
  annotate("rect", xmin = yearquarter('2021 Q3'), xmax = yearquarter('2023 Q2'),
           ymin = -Inf, ymax = Inf, alpha = 0.2, fill = 'yellow') +
  guides(
    fill = guide_legend(
      keywidth = unit(0.8, "cm"),
      keyheight = unit(0.4, "cm"),
      ncol = 1
    ),
    colour = guide_legend(
      keywidth = unit(0.8, "cm"),
      keyheight = unit(0.05, "cm"),
      ncol = 1
    )
  ) +
  labs(title = glue("**Hutchins Center Fiscal Impact Measure: Components**"),
       x = NULL,
       y = NULL,
       subtitle = "Fiscal Policy Contribution to Real GDP Growth, percentage points",
       caption = "Source: Hutchins Center calculations from Bureau of Economic Analysis 
        and Congressional Budget Office data; grey shaded areas indicate recessions 
        and yellow shaded areas indicate projection.") +
  scale_y_continuous(breaks = seq(-18, 28, by = 2),
                     labels = scales::label_percent(scale = 1, accuracy = 1)) +
  scale_x_yearquarter(breaks = yearquarter("1960 Q1") + seq(0, 256, by = 4 * 5),
                      date_labels = "%Y",
                      expand = expansion(0.0375)) 

ggsave(
  here::here('analysis/fim-historical/figures', 'components-alternative.png'),
  width = 85 * (14 / 5),
  height = 53 * (14 / 5),
  units = 'mm',
  dpi = 300,
  type = 'cairo',
  
  bg = '#FFFFFF'
)