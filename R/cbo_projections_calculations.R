
#' Cola adjustment 
#'
#' @return
#' @export
#'
#' @examples
  cola_adjustment <- function() {
    cbo_projections %>%
      mutate(cpiu_g = q_a(cpiu) / 100,
             cola_rate = if_else(month(date) == 3,
                                 lag(cpiu_g, 2),
                                 NULL) 
      ) %>%
      fill(cola_rate) %>%
      mutate(
        gftfp_before_cola = gftfp,
        health_ui = SMA(yptmd + yptmr + yptu, n = 4),
        gftfp_noCOLA = SMA((gftfp - health_ui)*(1-cola_rate), n = 4),
        gftfp =  gftfp_noCOLA * (1 + cola_rate)  + health_ui,
      ) 
  } 
#' Smooth budget series
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
  smooth_budget_series <- function(df){
    # smooth all budget series except total social transfers, which we did above
    df %>%
      mutate(
        across(.cols = c("gfrpt",  "gfrpri",  "gfrcp",  "gfrs", "yptmr",  "yptmd"),
               .fns = ~ rollapply(.x, width = 4, mean, fill = NA, align =  'right')
        ) 
      )
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
             gfrptCurrentLaw_g = gfrpt_g,
             gfrpt_g =
               if_else(date >= expdate,
                       lag(gfrpt_g),
                       gfrpt_g,
                       missing = NULL
               ),
             gfrpt  = if_else(date >= predate,
                              lag(gfrpt) * (1 + gfrpt_g / 400),
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
          .cols = where(is.numeric),
          .fns = ~ q_g(.),
          .names = "{.col}_g"
        ) 
      )
  }
 
