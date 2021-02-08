
cola_adjustment <- function(df){
  
  get_cola_rate <- function(df){
    df %>%
      mutate(cpiu_g = q_a(cpiu) / 100,
             cola_rate = if_else(month(date) == 3,
                                 lag(cpiu_g, 2), 
                                 NULL)) %>%
      fill(cola_rate)
  }
  smooth_transfers_net_health_ui <- function(df){
    df %>%
      mutate(health_ui = SMA(yptmd + yptmr + yptu, n = 4),
             gftfp_unadjusted = SMA((gftfp - health_ui) * (1 - cola_rate), n = 4),
             gftfp = if_else(id == 'forecast', 
                             gftfp_unadjusted * (1 + cola_rate) + health_ui,
                             gftfp)) %>%
      select(-c(health_ui, gftfp_unadjusted, cola_rate))
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
        .names = "{.col}_g"
      )
    ) 
}
