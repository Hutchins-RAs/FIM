#Counterfactual example for Louise
#start 2020 Q1 onward, then just growth that datapoint by gdp growth
#growth rates: compare the growth rate of what actually happened versus the counterfactual of inflation + potential gdp growth
#neutral contribution of consumption. fim divided by share consumption (or other component) is the FI of consumption



### Moving counterfactual

# purchases_moving_counterfactual <- function(df) {
#   df %>%
#     mutate(
#       over(
#         c(
#           'federal_purchases',
#           'state_purchases',
#           'consumption_grants',
#           'investment_grants'
#         ),
#         
#         .fn = ~ (lag(.("{.x}")) * (
#           1 + .("{.x}_deflator_growth") + real_potential_gdp_growth
#         )) ,
#         
#         .names = "{x}_counterfactual"
#       ),
#       grants_counterfactual = consumption_grants_counterfactual + investment_grants_counterfactual,
#       federal_counterfactual = federal_purchases_counterfactual + grants_counterfactual,
#       state_counterfactual = state_purchases_counterfactual  - grants_counterfactual,
#       total_purchases_counterfactual = state_counterfactual + federal_counterfactual
#     )
#   
# }
# 
# purchases_noarp_moving_counterfactual <-
#   consumption %>% purchases_counterfactual() %>% filter_index("2019 Q1" ~ "2023 Q1")
# 
# #OR
# moving_counterfactual <-
#   purchases_noarp_moving_counterfactual %>% pivot_longer(cols = c(federal_purchases, federal_purchases_counterfactual)) %>% ggplot(aes(x = date, y = value, color = name)) + geom_line() + theme(legend.position = "top")
# 
# 

### Static counterfactul
consumption2 <- consumption %>%
          mutate(across(
            c(
              federal_purchases,
              state_purchases,
              consumption_grants,
              investment_grants
            ),
            .fns = ~ if_else(date == yearquarter("2020 Q1"), .x, NA_real_),
            .names = "{.col}_counterfactual"
          ))
        
     
        
        
   counterfactual <- consumption2%>%
          mutate(across(
            c(
              federal_purchases,
              state_purchases,
              consumption_grants,
              investment_grants
            ),
            .fns = ~ if_else(date > yearquarter("2020 Q1"),  NA_real_, .x),
            .names = "{.col}_counterfactual"
          )) %>% 
   mutate(across(ends_with('counterfactual'),
                 ~ real_potential_gdp_growth +  get(paste0(str_sub(cur_column(), end = -16), "_deflator_growth")),
                 .names = '{.col}_growth')) %>% 
     coalesce_growth(federal_purchases_counterfactual,
                     state_purchases_counterfactual,
                     consumption_grants_counterfactual,
                     investment_grants_counterfactual) %>% 
     dplyr::filter(dplyr::between(dplyr::row_number(), dplyr::last(which(date == yearquarter("2020 Q1"))), n())) %>% 
     dplyr::select(date, id, ends_with('counterfactual')) %>% 
     dplyr::mutate(dplyr::across(where(is.numeric),  ~ purrr::accumulate(.x, `*`))) %>% 
     coalesce_join(consumption, by = 'date') 
          
   




counterfactual <- counterfactual %>% 
  as_tsibble(index = date) %>% 
  filter_index("2020 Q1" ~ "2023 Q1") %>% 
  select(date, ends_with('purchases'), ends_with('counterfactual'), 
         consumption_grants, investment_grants, -starts_with('add'),
         -starts_with('real'))


data_long <- counterfactual %>% select(-ends_with('counterfactual'))  %>% 
  pivot_longer(-date,
               values_to = 'fim')
counterfactual_long <- counterfactual %>% select(date, ends_with('counterfactual')) %>% 
  pivot_longer(-date,
               values_to = 'counterfactual')%>% 
  mutate(name = str_remove(name, '_counterfactual'))

comparison <- left_join(data_long, counterfactual_long,
                        by = c('date', 'name')) %>% 
  pivot_longer(c(fim, counterfactual),
               names_to = 'source') %>% 
  rename(variable = name)


comparison_plot <- function(.data, variable){
  
  
  plot <- .data %>% 
    filter(variable == {{ variable }}) %>% 
    ggplot(aes(x = date,  y =  value, color = source)) +
    #geom_col(position=position_dodge2(reverse = TRUE)) +
    geom_line() +
    labs(title = glue::glue("{snakecase::to_title_case(variable)}"),
         x = NULL,
         y = NULL) +
    ggthemes::theme_hc() +
    gghutchins::scale_fill_hutchins(
      name = "",
      labels = c('Updated', 'Previous'),
      pal = 'qual',
      rev = FALSE
    ) +
    scale_x_yearquarter(breaks = waiver(),
                        date_breaks = '3 months',
                        date_labels = "%Y Q%q") +
    theme(legend.position = 'top',
          legend.title = element_blank()) +
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


  
comparison_nested <-
  comparison %>% 
  group_by(variable) %>% 
  nest() %>% 
  mutate(plot = map2(.x = variable,
                     .y = data,
                     .f = ~comparison_plot(.data = .y, 
                                           variable = .x)))
   
level_plots <- rlang::set_names(comparison_nested$plot, comparison_nested$variable)



growth <- comparison %>% 
  group_by(variable,source) %>% 
  mutate(growth = (value / lag(value) - 1)) %>% 
  select(-value) %>% 
  rename(value = growth) %>% 
  group_by(variable) %>% 
  nest() %>% 
  mutate(plot = map2(.x = variable,
                     .y = data,
                     .f = ~comparison_plot(.data = .y, 
                                           variable = .x)))
growth_plots <- rlang::set_names(growth$plot, growth$variable)


level_plots$federal_purchases
level_plots$state_purchases
level_plots$consumption_grants
level_plots$investment_grants

growth_plots$federal_purchases +
  scale_y_continuous(labels = scales::label_percent())
growth_plots$state_purchases +
  scale_y_continuous(labels = scales::label_percent())
growth_plots$consumption_grants +
  scale_y_continuous(labels = scales::label_percent())
growth_plots$investment_grants + 
  scale_y_continuous(labels = scales::label_percent())
