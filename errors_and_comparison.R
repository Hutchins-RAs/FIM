librarian::shelf(
  'tidyverse',
  'purrr',
  'tsibble',
  'lubridate',
  'glue',
  'ggiraph',
  'plotly'
)
comparison_plot <- function(.data, variable){
  
  
  plot <- .data %>% 
    filter(variable == {{ variable }}) %>% 
    ggplot(aes(x = date,  y =  value, fill = source)) +
    #geom_col(position=position_dodge2(reverse = TRUE)) +
    geom_col(position=position_dodge2(reverse = TRUE)) +
    labs(title = glue::glue("{snakecase::to_title_case(variable)}"),
         x = NULL,
         y = NULL) +
    ggthemes::theme_hc() +
    gghutchins::scale_fill_hutchins(
      name = "",
      labels = c('Updated', 'published'),
      pal = 'qual',
      rev = FALSE
    ) +
    scale_x_yearquarter(breaks = waiver(),
                        date_breaks = '3 months',
                        date_labels = "Q%q") +
    facet_grid( ~ year(date),
                space = "free_x",
                scales = "free_x",
                switch = "x")  +
    theme(legend.position = 'top') +
    guides(fill = guide_legend(reverse = TRUE)) 
  
  
  variable_name <- rlang::as_name(rlang::ensym(variable))
  
  if(str_ends(variable_name, 'contribution')){
    plot + 
      scale_y_continuous(name = '', 
                         labels = scales::label_percent(scale = 1))
  } else {
    plot +
      scale_y_continuous(name = '', 
                         labels = scales::label_comma())
  }
  
}


# Load published months results
published <- 
  readxl::read_xlsx('results/4-2021/fim-4-2021-published.xlsx') %>% 
  mutate(date = yearquarter(date)) %>% 
  drop_na(date) %>% 
  as_tsibble(index = date) %>% 
  filter_index("2020 Q2" ~ "2023 Q1") %>% 
  # rename variables so that they match new names
  rename_with(~paste0(.x, 'ribution'), ends_with("cont")) %>% 
  rename_with(~stringr::str_replace(.x, pattern = 'unemployment_insurance',
                                    replacement = 'ui'), 
              contains('unemployment_insurance')) %>% 
  rename_with(~stringr::str_replace(.x, pattern = 'nom',
                                    replacement = 'purchases'), 
              contains('nom')) %>% 
  rename_with(~stringr::str_replace(.x, pattern = 'pi',
                                    replacement = 'deflator_growth'), 
              contains('pi')) %>% 
  rename_with(~stringr::str_replace(.x, pattern = 'noncorp',
                                    replacement = 'non_corporate'), 
              contains('noncorp')) %>% 
  rename_with(~stringr::str_replace(.x, pattern = 'federal_cgrants',
                                    replacement = 'consumption_grants'), 
              contains('cgrants')) %>% 
  rename_with(~stringr::str_replace(.x, pattern = 'federal_igrants',
                                    replacement = 'investment_grants'), 
              contains('igrants')) %>% 
  rename_with(~stringr::str_replace(.x, pattern = 'state_local',
                                    replacement = 'state'), 
              contains('state_local')) %>% 
  
  rename(
    federal_aid_to_small_businesses_arp = aid_to_small_businesses,
    federal_non_health_grants_arp_contribution = non_health_grants_contribution
  ) %>% 
  select(-date.y)

# 
no_errors <- readxl::read_xlsx('results/4-2021/fim-4-2021-without-errors.xlsx') %>% 
  mutate(date = yearquarter(date)) %>% 
  drop_na(date) %>% 
  as_tsibble(index = date) %>% 
  filter_index("2020 Q2" ~ "2023 Q1")

published_long <- pivot_longer(published, cols = where(is.numeric), values_to = 'published')
no_errors_long <- pivot_longer(no_errors, cols = where(is.numeric), values_to = 'no_errors')

errors <- inner_join(published_long, 
                         no_errors_long,
                         by = c('date', 'name', 'id')) %>% 
  pivot_longer(c(published, no_errors),
               names_to = 'source') %>% 
  rename(variable = name)

errors_nested <-
  comparison %>% 
  group_by(variable) %>% 
  nest() %>% 
  mutate(plot = map2(.x = variable,
                     .y = data,
                     .f = ~comparison_plot(.data = .y, 
                                           variable = .x)))


error_plots <- rlang::set_names(errors_nested$plot, errors_nested$variable)


# Load previous months results
previous <- 
  readxl::read_xlsx('results/4-2021/fim-4-2021-without-errors.xlsx') %>% 
  mutate(date = yearquarter(date)) %>% 
  drop_na(date) %>% 
  as_tsibble(index = date) %>% 
  filter_index("2020 Q2" ~ "2023 Q1") 
 

# 
current <- readxl::read_xlsx('results/5-2021/fim-5-2021.xlsx') %>% 
  mutate(date = yearquarter(date)) %>% 
  drop_na(date) %>% 
  as_tsibble(index = date) %>% 
  filter_index("2020 Q2" ~ "2023 Q1")

previous_long <- pivot_longer(previous, cols = where(is.numeric), values_to = 'previous')
current_long <- pivot_longer(current, cols = where(is.numeric), values_to = 'current')

comparison <- inner_join(previous_long, 
                         current_long,
                         by = c('date', 'name', 'id')) %>% 
  pivot_longer(c(previous, current),
               names_to = 'source') %>% 
  rename(variable = name)

comparison_nested <-
  comparison %>% 
  group_by(variable) %>% 
  nest() %>% 
  mutate(plot = map2(.x = variable,
                     .y = data,
                     .f = ~comparison_plot(.data = .y, 
                                           variable = .x)))


comparison_plots <- rlang::set_names(comparison_nested$plot, comparison_nested$variable)

rmarkdown::render('presentation.Rmd',)
