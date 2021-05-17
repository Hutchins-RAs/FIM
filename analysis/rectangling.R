# Setup -------------------------------------------------------------------
librarian::shelf(
  'tidyverse',
  'zoo',
  'TTR',
  'tsibble',
  'targets',
  'tarchetypes',
  'lubridate',
  'alistaire47/pipecleaner',
  'glue',
  'validate',
  'fim',
  'dplyover',
  'tsibble',
  'magrittr',
  'feasts',
  'fable'
)

# Baseline projections ----------------------------------------------------

dictionary <-  tribble(~haver_name, ~fim_name,
                       "gf", "federal_purchases_nominal",
                       "gfh", "federal_purchases_real",
                       "gs", "state_purchases_nominal",
                       "gsh", "state_purchases_real",
                       "gftfp", "federal_social_benefits",
                       "gfrpt", "federal_personal_taxes",
                       "gfrpri", "federal_payroll_taxes",
                       "gfrcp", "federal_corporate_taxes",
                       "yptu", "total_ui",
                       "federal_ui", "federal_ui",
                       "state_ui", "state_ui",
                       "yptmr", "medicare", 
                       "yptmd", "medicaid",
                       "gdppothq", "potential_gdp_real",
                       "gdppotq", "potential_gdp_nominal",
                       "c", "consumption_nominal",
                       "ch",  "consumption_real",
                       "gdp", "gdp_nominal",
                       "gdph", "gdp_real"
                       )


cbo <- fim::projections %>% 
  filter_index('2020 Q4'  ~.) %>% 
  smooth_budget_series()%>% 
  select(-fy) %>% 
  
  pivot_longer(-c(date, id)) %>% 
  
  left_join(dictionary,by = c("name" = "haver_name")) %>% 
  
  as_tibble() %>% 
  select(-name) %>% 
  drop_na(fim_name) %>% 
  pivot_wider(names_from = fim_name,
              values_from = value) %>% 
  pivot_longer(ends_with("nominal") |ends_with('real'),
               names_pattern = "(.*)_(nominal|real)$",
               names_to = c("variable", ".value")) %>% 
  mutate(deflator_growth = q_g(nominal / real),
         growth = q_g(nominal)) 
  # pivot_wider(names_from = 'variable', values_from = c(deflator_growth, growth),
  #             names_glue = "{variable}_{.value}") 
  

growth_rates <-
  cbo %>% select(date,variable,ends_with('growth'))
  

baseline_projections_wide <-
  read_data() %>%
  define_variables() %>%
  as_tsibble(key = id, index = date) %>% 
  create_override(
    var = state_purchases_growth,
    start = yearquarter('2020 Q4'),
    end = yearquarter('2022 Q1'),
    values = c(rep(0.0025, 3), 0.005, 0.0075, 0.01)
  )  %>% 
  growth_assumptions() %>%
  reallocate_legislation() %>% 
  mutate(
    across(c(ppp, aviation, paid_sick_leave, employee_retention),
           ~ coalesce(.x, 0)),
    federal_subsidies = federal_subsidies - ppp - aviation - paid_sick_leave - employee_retention,
    subsidies = federal_subsidies + state_subsidies
  ) %>% 
  
  ungroup() %>% 
  forecast2(gdp, federal_purchases, state_purchases) %>% 
  mutate(# Health outlays reattribution
    health_outlays = medicare + medicaid,
    federal_health_outlays = medicare + medicaid_grants,
    state_health_outlays = medicaid - medicaid_grants,
    # Aggregate taxes
    corporate_taxes = federal_corporate_taxes + state_corporate_taxes,
    payroll_taxes = federal_payroll_taxes + state_payroll_taxes,
    production_taxes = federal_production_taxes + state_production_taxes,
    personal_taxes = federal_personal_taxes + state_personal_taxes,
    # Coalesce NA's to 0
    across(where(is.numeric),
           ~ coalesce(.x, 0))) %>% 
  get_non_corporate_taxes() %>% 
  as_tsibble(key = id, index = date)
# Rectangle data ----------------------------------------------------------
baseline_projections_long <- 
    baseline_projections_wide %>%
    select(
      date,
      federal_social_benefits,
      state_social_benefits,
      federal_health_outlays,
      state_health_outlays,
      federal_subsidies,
      state_subsidies,
      federal_ui,
      state_ui,
      federal_corporate_taxes,
      state_corporate_taxes,
      gdp,
      real_potential_gdp_growth,
      medicaid_growth,
      federal_purchases,
      state_purchases,
      federal_consumption_grants = consumption_grants,
      federal_investment_grants = investment_grants
    ) %>%
    pivot_longer(
      starts_with(c('federal', 'state')),
      names_to = c('government', 'variable'),
      names_pattern = '(federal|state)_(.*)',
      values_to = 'values'
    ) %>%
    mutate(
      component = case_when(
        variable %in% c('social_benefits', 'subsidies', 'health_outlays', 'ui') ~ 'transfers',
        variable %in% c('corporate_taxes') ~ 'taxes',
        variable %in% c('purchases', 'consumption_grants', 'investment_grants') ~ 'government'
      )
    )

# Contributions -----------------------------------------------------------
myplot <- function(.data, variable){
 plot <- .data %>% 
    filter(variable == {{ variable }}) %>% 
    ggplot(aes(x = date,  y =  values, color = variable)) +
    geom_line()+
    labs(title = paste0("Variable: ", variable))
}
#  Plot everything
baseline_projections_plots  <-
  baseline_projections_long %>% 
  group_by(variable) %>% 
  nest() %>% 
  mutate(plot = map2(.x = variable,
                     .y = data,
                     .f = ~ myplot(.data = .y, variable = .x)))
# Prepare mpc functions
fun <- tribble(
  ~ variable, ~mpc,
  'social_benefits' , 'mpc_social_benefits', 
  'health_outlays' , 'mpc_health_outlays',
  'subisidies' , 'mpc_subsidies',
  'ui' , 'mpc_ui',
  'corporate_taxes' , 'mpc_corporate_taxes',
  'purchases', 'identity',
  'consumption_grants', 'identity'
)



deflators <- 
  baseline_projections_wide %>% 
  select(date, ends_with('deflator_growth')) %>% 
  pivot_longer(-c(date, id),
               names_to = 'deflator',
               values_to = 'deflator_value')

deflators %>% 
  filter_index("2017 Q1" ~ "2022 Q4") %>% 
  filter(id == "historical") %>% 
  autoplot()

fim_long <-
  baseline_projections_long %>% 
  left_join(fun, by = 'variable') %>% 
  filter(id == 'historical') %>% 
  as_tsibble(key = c(government, variable), index = date) %>% 
  drop_na() %>%
  mutate(consumption = invoke_map(mpc, values)) %>% 
  group_by(variable) %>% 
  mutate(growth = values / lag(values) - 1) %>% 
  mutate(consumption = as.numeric(consumption)) %>% 
   mutate(deflator = if_else(component != 'government', 'consumption_deflator_growth', 'federal_purchases_deflator_growth')) %>% 
  left_join(deflators, by = c('date', 'id', 'deflator')) %>% 
  mutate(counterfactual = lag(values) * (1 + deflator_value + real_potential_gdp_growth),
         counterfactual_consumption = invoke_map(mpc, counterfactual),
         counterfactual_consumption = as.numeric(counterfactual_consumption)) %>% 
  mutate(contribution =  (consumption - counterfactual_consumption) / lag(gdp)) %>% 
    select(date, id, government, component, variable, values, 
           growth, consumption, counterfactual_consumption, real_potential_gdp_growth, deflator, deflator_value, contribution) %>% 
    filter_index("2010 Q1" ~ .)

theme_set(gghutchins::theme_hutchins())
fim_long %>% 
  rename_with(.fn = ~snakecase::to_title_case(.),
              .cols = everything()) %>% 
  openxlsx::write.xlsx("fim_long.xlsx")

fim_long %>% 
  filter(component == "transfers") %>% 
  autoplot() 
  scale_x_yearquarter(breaks = "Two year")

fim_long %>% 
  filter(variable == "social_benefits",
         government == "federal") %>%
  feasts::gg_tsdisplay()

fim_long %>% 
  filter(variable == "purchases") %>%
  feasts::gg_season()

fim_long %>% 
  filter(variable == "purchases",
         government == "state") %>%
  feasts::gg_tsdisplay()

fim_long %>% 
  filter(variable == "ui",
         government == "state") %>% 
  feasts::gg_subseries()

fim_long %>% 
  filter(component == "transfers") %>% 
  autoplot()

fim_long %>% 
  aggregate_key(component/variable, values)

