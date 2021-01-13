#' Compare between versions
#' Helper functions to visualize differences between different versions of 
#' the FIM. By default this returns a comparison between the previous months total
#' fiscal impact and the current one.
#' @param df 
#' @param df_new 
#' @param variable 
#' @param title 
#'
#' @return
#' @export
#'
#' @examples
comparison_plot <- function(df = old, df_new = new, variable = fiscal_impact, title = ''){
  variable <- enquo(variable)
  df %>%
    bind_rows(df_new) %>%
    select(date, !!variable, key) %>%
    group_by(key) %>%
    pivot_longer(where(is.numeric)) %>%
    ggplot(aes(x = date,
               y = value,
               fill = key)) +
    geom_col(position = 'dodge') +
    geom_vline(xintercept = last_hist_date, linetype = 'dotted') +
    labs(x = '', y = '', title = title) +
    scale_fill_brewer(name = "", labels = c('Updated', 'Previous'),
                      type = 'qual', palette = 'Paired', direction = -1) + 
    theme_hc() +
    theme(plot.title.position = 'plot')
}