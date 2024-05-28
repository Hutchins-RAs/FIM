
#' Implicit price deflators
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
  implicit_price_deflators <- function(df){
    # Implicit price deflators
    df %>% 
      mutate(jgf =  gf/gfh,
             jgs = gs/gsh,
             jc = c/ch) 
  }
#' Forecast state taxes
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
   forecast_state_taxes <- function(df){
    df %>% 
      left_join(hist %>%
                  select(date, gsrpt ,gsrpri, gsrcp ,gsrs),
                all.x = F) %>%
      filter(date > '2016-12-31') %>%
      mutate(
        across(
          .cols = c("gsrpt" ,"gsrpri", "gsrcp" ,"gsrs"),
          .fns = ~ na.locf(. / gdp) * gdp
        )
      )
  }


#' Get growth rates of federal transfers
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
federal_transfers_growth_rates <- function(df){
  df %>%
    mutate(
      across(
        .cols = c("gfrpt",  "gfrpri",  "gfrcp",  "gfrs"),
        .fns = ~ q_g(.x),
        .names = "{.col}_g"
      )
    ) 
  }

#' Get health growth rates
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
health_outlays_growth_rates <- function(df){
  df %>%
    mutate(
      across(
        .cols = c("yptmr",  "yptmd" ),
        .fns = ~ q_g(.x),
        .names = "{.col}_growth"
      )
    ) 
}

smooth_budget_series <- function(df) {
  federal_taxes <- c('gfrpt', 'gfrpri', 'gfrcp', 'gfrs')
  health_outlays <- c('yptmd', 'yptmr')
  unemployment_insurance <- 'yptu'
  df %>%
    mutate(across(all_of(c(federal_taxes, health_outlays, unemployment_insurance)),
                  ~ zoo::rollapply(.x, width = 4, mean, fill = NA,min_obs = 1, align = 'right')))
}
