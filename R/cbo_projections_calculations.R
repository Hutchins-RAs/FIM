
cola_adjustment <- function(df){
  
  get_cola_rate <- function(df){
    df %>%
      mutate(cpiu_g = q_a(cpiu) / 100,
             cola_rate = if_else(quarter(date) == 1,
                                 lag(cpiu_g, 2), 
                                 NULL)) %>%
      fill(cola_rate)
  }
  smooth_transfers_net_health_ui <- function(df){
    df %>%
      mutate(gftfp_unadj = gftfp,
             health_ui = TTR::SMA(yptmd + yptmr + yptu, n = 4),
             smooth_gftfp_minus_health_ui = TTR::SMA((gftfp - health_ui) * (1 - cola_rate), n =4),
             gftfp = smooth_gftfp_minus_health_ui * (1 + cola_rate) + health_ui)
  }
  df %>%
    get_cola_rate() %>%
    smooth_transfers_net_health_ui()
  
}

#' Alternative tax scenario
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
  alternative_tax_scenario <- function(df){
    # Construct alternative scenario for personal current taxes, under which the TCJA provisions for income taxes don't
    # expire in 2025
    expdate <- "2025-12-30"
    predate <- "2025-09-30"
    
    df %>%
      mutate(gfrptCurrentLaw = gfrpt,
             gfrptCurrentLaw_growth = gfrpt_growth,
             gfrpt_growth =
               if_else(date >= expdate,
                       lag(gfrpt_growth),
                       gfrpt_growth,
                       missing = NULL
               ),
             gfrpt  = if_else(date >= predate,
                              lag(gfrpt) * (1 + gfrpt_growth / 400),
                              gfrpt))
  }
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
#' Calculate growth rates
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
  growth_rates <- function(df){
    df %>%
      mutate(
        across(
          .cols = where(is.numeric) & !ends_with('_growth'),
          .fns = ~ q_g(.),
          .names = "{.col}_growth"
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
                  ~ rollapply(.x, width = 4, mean, fill = NA, align = 'right')))
}
