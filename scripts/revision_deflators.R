# Deflators ---------------------------------------------------------------



deflators <- inner_join(previous_long,
                        current_long,
                        by = c('date', 'name' )) %>% 
  mutate(diff = current - previous) %>% 
  rename(variable = name) %>% 
  filter(str_detect(variable, 'deflator_growth')) %>% 
  mutate(across(where(is.numeric),
                ~ ((1 + .x)^4))-1)

diff_plot <-
  deflators %>% 
  mutate(variable = snakecase::to_title_case(variable)) %>% 
  ggplot(aes(x = date,  y =  diff, color = variable)) +
  #geom_col(position=position_dodge2(reverse = TRUE)) +
  geom_line(show.legend = FALSE) +
  geom_point(aes(shape=id), show.legend = FALSE) +
  facet_wrap(variable~.) +
  scale_y_continuous(labels = scales::label_percent()) +
  
  labs(title = 'Difference in deflator growth', 
       subtitle = 'Annualized',
       x = NULL,
       y = NULL)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

cur_plot <-
  deflators %>% 
  mutate(variable = snakecase::to_title_case(variable)) %>% 
  ggplot(aes(x = date,  y =  current, color = variable)) +
  #geom_col(position=position_dodge2(reverse = TRUE)) +
  geom_line(show.legend = FALSE) +
  geom_point(aes(shape=id),show.legend = FALSE) +
  facet_wrap(variable~.) +
  scale_y_continuous(labels = scales::label_percent()) +
  geom_hline(yintercept = 0) +
  labs(title = 'Current deflator growth', 
       subtitle = 'Annualized',
       x = NULL,
       y = NULL) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

prev_plot <-
  deflators %>% 
  mutate(variable = snakecase::to_title_case(variable)) %>% 
  ggplot(aes(x = date,  y =  previous, color = variable)) +
  #geom_col(position=position_dodge2(reverse = TRUE)) +
  geom_line(show.legend = FALSE) +
  geom_point(aes(shape=id), show.legend = FALSE) +
  facet_wrap(variable~.) +
  scale_y_continuous(labels = scales::label_percent()) +
  geom_hline(yintercept = 0) +
  labs(title = 'Previous deflator growth', 
       subtitle = 'Annualized',
       x = NULL,
       y = NULL)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))