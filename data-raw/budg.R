# Budget ----------------------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman") 
pacman::p_load(readxl, magrittr, dplyr, usethis, here, data.table)
budg <-
  read_xlsx(here::here("inst",'extdata', 'cbo_budget_nipas_proj_annual.xlsx'))



budg %<>% bind_rows(budg, budg, budg) %>% 
  arrange(fy) %>% 
  mutate(fiscal_quarter = rep(seq(1, 4, 1), 14)) %>%
  mutate(fiscal_quarter = case_when(
    fiscal_quarter == 1 ~ '03-31',
    fiscal_quarter == 2 ~ '06-30',
    fiscal_quarter == 3 ~ '09-30',
    fiscal_quarter == 4 ~ '12-31'
  )) %>%
  mutate(calendar_quarter = lag(fiscal_quarter)) %>%
  unite('fiscal_date', c(fy, fiscal_quarter), sep = '-') %>%
  mutate(date = lubridate::as_date(lag(fiscal_date))) %>%
  select(-c(fiscal_date, calendar_quarter)) %>%
  relocate(date) 

### 2.1.1 COLA Adjustments --------------------------------------------------------------------------------------

# Adjust federal transfers to feature their january COLA-related bump; reattribute that growth to calendar quarter 1
# before smoothing out the rest of the non-COLA related growth. SSA uses CPI-W to create COLAs; We use CBO's projection
# of CPI-U. This slightly affects the timing of total transfers, but not their levels
# 
budg %<>%
  mutate(cpiu = lag(econ$cpiu),
         cpiu_g = q_a(cpiu) / 100,
         pcw = haver$pcw[match(budg$date, haver$date)],
         pcw_g = q_a(pcw) / 100,
         #        Applicable cola rate is CPIW from Q3 of previous year
         cola_rate =
           if_else(
             month(date) == 3,
             lag(cpiu_g, 2),
             NULL
           )
  ) %>% 
  # forecastPeriod filling so each q has correct cola rate, 
  fill(cola_rate)

budg$pcw_g[is.na(budg$pcw_g)] = budg$cpiu_g[is.na(budg$pcw_g)]

budg %<>% 
  mutate(health_ui = TTR::SMA(yptmd + yptmr + yptu, n = 4),
                  # temporarily take out medicaid, medicare, ui, and COLA 
                  # smooth with 4 quarter  moving average
                  gftfp_noCOLA = TTR::SMA((gftfp - health_ui)*(1-cola_rate), n = 4),
                  # Store old gftfp as unadjusted
                  gftfp_unadj = gftfp,
                  # Add COLA and smoothed health back in for new adjusted gftfp
                  gftfp = gftfp_noCOLA * (1 + cola_rate)  + health_ui,
                  gftfp_g = q_g(gftfp)
  ) %>%
  # smooth all budget series except total social transfers, which we did above
  mutate( 
    across(.cols = c("gfrpt",  "gfrpri",  "gfrcp",  "gfrs", "yptmr",  "yptmd" ),
           .fns = ~ zoo::rollapply(.x, width = 4, mean, fill = NA, align =  'right')
    ) %>%
      # take "q-o-q" growth rate
      mutate(
        across(
          .cols = c("gfrpt",  "gfrpri",  "gfrcp",  "gfrs", "yptmr",  "yptmd" ),
          .fns  = ~  q_g(.x),
          .names = "{.col}_g"
        )
      )
  ) 
### 2.1.2 Alternate tax scenario --------------------------------------------------------------------------------

# Construct alternative scenario for personal current taxes, under which the TCJA provisions for income taxes don't
# expire in 2025

expdate = "2025-12-30"
predate = "2025-09-30"
budg %<>% 
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
                        gfrpt)
)

usethis::use_data(budg, overwrite = TRUE)

# Filter so that we only get the budget growth rates
budgetGrowthRates <-
  budg %>% 
    select(date, ends_with('_g'), gfrptCurrentLaw) 

usethis::use_data(budgetGrowthRates, overwrite = TRUE)

