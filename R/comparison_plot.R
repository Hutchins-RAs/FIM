#' Title
#'
#' @param .data 
#' @param variable 
#'
#' @return
#' @export
#'
#' @examples
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
      labels = c('Updated', 'Previous'),
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
