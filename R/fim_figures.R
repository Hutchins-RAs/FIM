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
#' Create recessions data frame
#' Data frame with start and end dates of U.S. recessions from NBER. 
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
get_recessions <- function(df){

  df %>% 
  dplyr::select(date, recession) %>%
  dplyr::mutate(recession = dplyr::if_else(is.na(recession),
                                           0,
                                           recession),
                recession_event = recession - dplyr::lag(recession),
                start = dplyr::if_else(recession_event == 1, 
                                       date,
                                       lubridate::NA_Date_) ,
                end = dplyr::if_else(recession_event == -1,
                                     date,
                                     lubridate::NA_Date_) 
  ) %>%
  dplyr::select(start, end) %>%
  tidyr::pivot_longer(cols = c(start, end)) %>%
  na.omit() %>%
  group_by(name) %>%
  dplyr::mutate(row = row_number()) %>%
  pivot_wider(names_from = name,
              values_from = value) %>%
  dplyr::select(-row)
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
  recession_shade <-
    geom_rect(data = df, aes(xmin = start, xmax = end, ymin=-Inf, ymax=+Inf),
              fill = 'grey', alpha = 0.3)
  
  return(recession_shade)
}

#' Create a fim plot
#' Functions for consistency in our plots
#' @param df 
#' @param title 
#'
#' @return
#' @export
#'
#' @examples
fim_plot <-
  function(df, title, last_hist_date = last_hist_date){
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
                        y = 16), 
                    label = "Projection",
                    cex = 2, 
                    fill = NA, label.color = NA, # remove background and outline
      ) +
      annotate("rect", xmin = last_hist_date + 40, xmax = end,
               ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'yellow') +
      geom_rect(data = recessions,
                aes(x = NULL, y = NULL,
                    xmin = start, xmax = end, 
                    ymin=-Inf, ymax=+Inf), fill = 'grey', alpha = 0.3) +
      scale_x_date(breaks = 0, date_breaks = "2 years", date_labels = '%Y',
                   expand = c(0,0)) + 
      scale_color_manual(" ", 
                         values=c("4-quarter moving-average" ="black",
                                  "4-quarter moving-average" ="black")) +
      guidez +
      uni.theme() 
  }


#' Title
#'
#' @return
#' @export
#'
#' @examples
total_fiscal_impact_plot <- function() {
  contributions %>%
    select(date, fiscal_impact, fiscal_impact_moving_average) %>%
    pivot_longer(cols = -c(date, fiscal_impact_moving_average), names_to = 'variable') %>%
    fim_plot(title = " Quarterly fiscal impact") +
    scale_fill_manual(labels = " Quarterly fiscal impact",
                      values = total_pink) +
    ggplot2::annotate("rect", xmin = last_hist_date + 40, xmax = end, 
                      ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "yellow")
}       

#' Title
#'
#' @return
#' @export
#'
#' @examples
taxes_transfers <- function() {
  contributions %>%
    select(date,  state_local_cont, federal_cont, taxes_transfers_cont, fiscal_impact_moving_average) %>%
    pivot_longer(cols = -c(date, fiscal_impact_moving_average), names_to = 'variable') %>%
    fim_plot(title = 'Total') +
    scale_fill_manual(
      labels = c(" State & Local Purchases", " Federal Purchases", " Taxes, Transfers, & Subsidies"),
      values =  c(state_local_purple, federal_blue, taxes_transfers_green)
    ) +
    ggplot2::annotate("rect", xmin = last_hist_date + 40, xmax = end, 
                      ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "yellow")
}
#' Title
#'
#' @return
#' @export
#'
#' @examples
components_govt <- function() {
  contributions %>%
    select(date, fiscal_impact_moving_average, state_local_cont, state_taxes_transfers_cont, 
           federal_cont, federal_taxes_transfers_cont)  %>%
    pivot_longer(cols = -c(date, fiscal_impact_moving_average), names_to = 'variable') %>%
    fim_plot(title = 'Total') +
    ggplot2::annotate("rect", xmin = last_hist_date + 40, xmax = end, 
                      ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "yellow") +
    scale_fill_brewer(labels = c(" State & Local Purchases",
                                 " State & Local Taxes, Transfers, & Subsidies",
                                 " Federal Purchases",
                                 " Federal Taxes, Transfers, & Subsidies")
    )
}  
#' Title
#'
#' @return
#' @export
#'
#' @examples
taxes_transfers_govt <- function(){
  contributions %>%
    select(date, fiscal_impact_moving_average,
           health_outlays_cont, social_benefits_cont, 
           noncorp_taxes_cont, corporate_taxes_cont,
           purchases_cont, subsidies_cont) %>%
    pivot_longer(cols = -c(date, fiscal_impact_moving_average), names_to = 'variable') %>%
    fim_plot(title = "Taxes and Transfers Components") +
    ggplot2::annotate("rect", xmin = last_hist_date + 40, xmax = end, 
                      ymin = -Inf, ymax = Inf, alpha = 0.1, fill = "yellow") +
    scale_fill_brewer(labels = c(" Health Outlays", " Social Benefits",
                                 " Noncorporate Taxes", " Corporate Taxes", 
                                 " Purchases", " Subsidies")
    )
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
taxes <- function(){
  contributions %>%
    filter(date > lubridate::today() - lubridate::years(2)) %>%
    select(date, federal_corporate_taxes_cont, federal_noncorp_taxes_cont,
           state_corporate_taxes_cont, state_noncorp_taxes_cont) %>%
    pivot_longer(cols = -c(date), names_to = 'variable') %>%
    ggplot(aes(x = date,
               y = value,
               fill = variable)) + 
    geom_col(position = 'dodge') +
    scale_fill_brewer(labels = c(' Federal Corporate Taxes', ' Federal Non-Corporate Taxes', ' State Corporate Taxes', ' State Non-Corporate Taxes'), type = 'qual', 
                      palette = 'Dark2') +
    fim_theme()+
    labs(
      title = '**Impact of Taxes by Level of Government**'
    )
}
#' Title
#'
#' @return
#' @export
#'
#' @examples
social_benefits <- function(){  
  contributions %>%
    filter(date > lubridate::today() - lubridate::years(5)) %>%
    select(date, 
           federal_social_benefits_cont, state_social_benefits_cont) %>%
    pivot_longer(cols = -c(date), names_to = 'variable') %>%
    ggplot(aes(x = date,
               y = value,
               fill = variable)) +
    geom_col(position = 'dodge') +
    scale_fill_brewer(labels = c(" Federal", " State"), type = 'seq'
    ) +
    fim_theme() +
    labs(title = 'Impact of Social Benefits',
         x= '',
         y ='')
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
health_outlays <- function(){
  contributions %>%
    filter(date > lubridate::today() - lubridate::years(5)) %>%
    select(date, 
           federal_health_outlays_cont, state_health_outlays_cont) %>%
    pivot_longer(cols = -c(date), names_to = 'variable') %>%
    ggplot(aes(x = date,
               y = value,
               fill = variable)) +
    geom_col(position = 'dodge') +
    scale_fill_brewer(labels = c(" Federal", " State"), type = 'div', direction = -1
    ) +
    fim_theme() +
    labs(title = 'Impact of transfers',
         x= '',
         y ='')
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
legislation <- function(){
  contributions %>%
    filter(date > lubridate::today() - lubridate::years(1)) %>%
    select(date, 
           subsidies_cont, unemployment_insurance_cont, 
           rebate_checks_cont) %>%
    pivot_longer(cols = -c(date), names_to = 'variable') %>%
    ggplot(aes(x = date,
               y = value,
               fill = variable)) +
    geom_col(position = 'dodge') +
    scale_fill_brewer(labels = c(
      ' Subsidies', ' Unemployment Insurance',
      ' Rebate checks'), type = 'qual'
    ) +
    fim_theme() + 
    labs(title = 'Impact of legislation',
         x = '',
         y = '')
}
