## code to prepare `rra` dataset goes here
librarian::shelf('tidyverse', 'zoo', 'TTR', 'tsibble', 'targets', 'tarchetypes', 'lubridate', "moodymudskipper/safejoin",
                 'alistaire47/pipecleaner', 'glue', 'validate', 'fim', 'dplyover', 'tsibble', 'gt', 'readxl')

rra_raw <- read_xlsx('inst/extdata/pandemic_legislation.xlsx',
                     sheet = 'RRA')
devtools::load_all()


rra <-
  rra_raw %>% 
  group_by(component) %>% 
  nest(data = c(category, name, total)) %>% 
  mutate(total = purrr::map_dbl(data, ~sum(.x$total)))

dates <- tibble(date = yearquarter('2021 Q1') + 0:12)
(rra <- 
    rra %>% 
    select(-data) %>% 
    pivot_wider(names_from = c(component, legislation), values_from = total) %>% 
    mutate(date = yearquarter('2021 Q1'), .before = everything()) %>% 
    as_tsibble(index = date) %>% 
     full_join(dates, by ='date') )


# MPCs --------------------------------------------------------------------

social_benefits_rra_timing <- mpc(timing = rep(1/4, 4))
snap_rra_timing <- mpc(timing = c(0.5, 0.5))

subsidies_rra_timing <- mpc(timing = rep(1/8, 8))
ppp_rra_timing <- mpc(timing = c(0.5, 0.5))
aviation_rra_timing <- mpc(timing = rep(1/3, 3))
employee_retention_credit_rra_timing <- mpc(timing = rep(1/4, 4))

federal_health_outlays_rra_timing <- mpc(timing = rep(1/12, 12))

consumption_grants_rra_timing <- mpc(timing = rep(1/12, 12))
federal_purchases_rra_timing <- mpc(timing = c(0.35, 0.35, 0.15, 0.15))


mpc_rra <- function(.data, var){
  var <- rlang::as_name(var)
  mpc_fun <- eval(sym(glue::glue('mpc_{var}')))
  .data %>% 
    mutate(across(!!var, ~ mpc_fun(.x)))
    

}

rra <-
  rra %>% 
  mutate(consumption_grants_rra = consumption_grants_rra_timing(consumption_grants_rra),
         ppp_rra = ppp_rra_timing(ppp_rra),
         aviation_rra = aviation_rra_timing(aviation_rra),
         employee_retention_credit_rra = employee_retention_credit_rra_timing(employee_retention_credit_rra),
         subsidies_rra = subsidies_rra_timing(subsidies_rra),
         snap_rra = snap_rra_timing(snap_rra),
         social_benefits_rra = social_benefits_rra_timing(social_benefits_rra)) %>% 
  mutate(federal_purchases_rra = federal_purchases_rra_timing(federal_purchases_rra),
         federal_health_outlays_rra = federal_health_outlays_rra_timing(federal_health_outlays_rra)) %>% 
  mutate(across(where(is.numeric),
                ~ .x * 4),
         across(where(is.numeric),
                ~ coalesce(.x, 0)))


 
  
rra <-
  rra %>% 
  pivot_longer(-date) %>% 
  mutate(component = recode(name, 
                            ppp_rra = 'subsidies_rra',
                            aviation_rra = 'subsidies_rra',
                            employee_retention_credit_rra = 'subsidies_rra',
                            subsidies_rra = 'subsidies_rra',
                            
                            snap_rra = 'federal_social_benefits_rra',
                            social_benefits_rra = 'federal_social_benefits_rra',
                            rebate_checks_rra = 'rebate_checks_rra',
                            federal_ui_rra = 'federal_ui_rra',
                            consumption_grants_rra = 'consumption_grants_rra',
                            federal_purchases_rra = 'federal_purchases_rra',
                            federal_health_outlays_rra = 'federal_health_outlays_rra')) %>% 
  group_by(component) %>% 
  nest(data = c(name, value)) %>% 
  mutate(total = purrr::map_dbl(data, ~sum(.x$value))) %>% 
  select(-data) %>% 
  pivot_wider(date, names_from = component, values_from = total)


usethis::use_data(rra, overwrite = TRUE)


rra$data

