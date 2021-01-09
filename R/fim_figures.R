#' Set theme options
#'
#' @return
#' @export
#'
#' @examples
fim_theme <- function() {
  theme_bw() +
    theme(legend.position = "bottom", 
          panel.grid.minor.x=element_blank(),
          panel.grid.major.x=element_blank(),
          plot.margin=unit(c(1.2,.5,.5,.5),"cm"),
          plot.title = element_markdown(size=12),
          plot.subtitle = element_markdown(size=10) , 
          plot.caption = 
            element_textbox_simple(size = 9,
                                   lineheight = 1,
                                   padding = margin(5.5, 5.5, 5.5, 5.5),
                                   margin = margin(0, 0, 5.5, 0)),
          legend.text=element_markdown(size=10), 
          legend.title=element_blank(),
          legend.spacing.y = unit(2, 'cm')
    ) # , legend.margin = unit(c(rep(-.8, 4)),"cm")
}

#' Set legend options
#'
#' @return
#' @export
#'
#' @examples
format_legends <- function(){
  guidez <- guides(
    fill = guide_legend(keywidth = unit(0.8, "cm"),
                        keyheight = unit(0.4, "cm"), 
                        ncol = 1),
    colour = guide_legend(keywidth = unit(0.8, "cm"),
                          keyheight = unit(0.05, "cm"), 
                          ncol = 1)
  )
  return(guidez)
}

#' Set colors for components
#'
#' @return
#' @export
#'
#' @examples
fim_colors <- function(){
  total_pink <- rgb(231, 97, 159, maxColorValue = 255)
  state_local_purple = rgb(174, 104, 169,  maxColorValue = 255)
  federal_blue = rgb(33, 152, 199,  maxColorValue = 255)
  taxes_transfers_green = rgb(27, 149, 83,  maxColorValue = 255)
}

#' Get recession start and end dates for shading 
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
get_recession_shade <- function(df){
  recessions <-
    df %>% 
    select(date, recession) %>%
    mutate(recession = if_else(is.na(recession),
                               0,
                               recession),
           recession_event = recession - lag(recession),
           start = if_else(recession_event == 1, 
                           date,
                           NA_Date_) ,
           end = if_else(recession_event == -1,
                         date,
                         NA_Date_) 
    ) %>%
    select(start, end) %>%
    pivot_longer(cols = c(start, end)) %>%
    na.omit() %>%
    group_by(name) %>%
    mutate(row = row_number()) %>%
    pivot_wider(names_from = name,
                values_from = value) %>%
    select(-row)
  
  recession_shade <-
    geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin=-Inf, ymax=+Inf),
              fill = 'grey', alpha = 0.3)
  
  return(recession_shade)
}

#' Create a fim plot
#'
#' @param df 
#' @param title 
#'
#' @return
#' @export
#'
#' @examples
fim_plot <-
  function(df, title){
    recession_shade <- get_recession_shade()
    df %>%
      ggplot() +
      geom_bar(aes(x = date, y = value, fill = variable),
               stat = 'identity', width = 50) +
      geom_line(
        aes(x = date,
            y = fiscal_impact_moving_average,
            colour = "4-quarter moving-average")
      ) +
      geom_point(
        aes(x = date,
            y = fiscal_impact_moving_average,
            colour = "4-quarter moving-average"), size = 1
      ) +
      labs(
        title = glue("**Hutchins Center Fiscal Impact Measure: {title}**"),
        x = '',
        y = '',
        subtitle = "Fiscal Policy Contribution to Real GDP Growth, percentage points",
        caption = "Source: Hutchins Center calculations from Bureau of Economic Analysis 
        and Congressional Budget Office data; grey shaded areas indicate recessions 
        and yellow shaded areas indicate projection.") +
      geom_richtext(aes(x = Sys.Date()+350,
                        y = max_y), 
                    label = "Projection",
                    cex = 2, 
                    fill = NA, label.color = NA, # remove background and outline
      ) +
      annotate("rect", xmin = last_hist_date + 40, xmax = end,
               ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'yellow') +
      scale_x_date(breaks = 0, date_breaks = "2 years", date_labels = '%Y',
                   expand = c(0,0)) + 
      scale_color_manual(" ", 
                         values=c("4-quarter moving-average" ="black",
                                  "4-quarter moving-average" ="black")) +
      format_legends() +
      fim_theme() 
  }
