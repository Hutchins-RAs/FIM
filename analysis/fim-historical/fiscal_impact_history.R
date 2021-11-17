# Setup -------------------------------------------------------------------
Sys.setenv(TZ = 'UTC')
librarian::shelf(
  "tidyverse",
  "zoo",
  "TTR",
  "tsibble",
  "lubridate",
  "glue",
  "fim",
  "dplyover",
  'gt',
  'ggtext'
)
options(digits = 4)
options(scipen = 20)
devtools::load_all()

# Wrangle data ------------------------------------------------------------

# Since BEA put all CARES act grants to S&L in Q2 2020 we need to
# override the historical data and spread it out based on our best guess
# for when the money was spent.
overrides <- readxl::read_xlsx('data/forecast.xlsx',
                               sheet = 'historical overrides') %>% 
  select(-name) %>% 
  pivot_longer(-variable) %>% 
  pivot_wider(names_from = 'variable',
              values_from = 'value') %>% 
  rename(date = name) %>% 
  mutate(date = yearquarter(date))


# Load national accounts data from BEA
projections <- get_cbo_projections()




history_raw <- readxl::read_xlsx('analysis/fim-historical/haver_pivoted_historical.xlsx')

history <- 
  history_raw %>% 
  rename(variable = name) %>% 
  pivot_longer(cols = -variable) %>% 
  pivot_wider(names_from = variable,
              values_from = value) %>% 
  rename(date = name) %>% 
  mutate(date = tsibble::yearquarter(date)) %>% 
  mutate(id = 'historical') %>%
  format_tsibble()

fmap <- read_csv('analysis/fim-historical/nhe_fmap.csv') %>% 
  mutate(fshare = if_else(is.na(fshare), gf_medicaid / (gf_medicaid + gs_medicaid), fshare)) %>% 
  mutate(year = as.character(year)) %>% 
  left_join(history %>% select(date) %>% 
              separate(date, into = c('year', 'quarter')), by = 'year') %>% 
  unite(col = date,
        c('year', 'quarter'),
        sep = '') %>% 
  mutate(date = yearquarter(date))


usna <-
  history %>%
  coalesce_join(projections, by = 'date') %>% 
  left_join(fmap, by = 'date') %>% 
  as_tsibble(key = id, index = date) %>% 
  mutate(gfeghdx = if_else(date < yearquarter("1999 Q1"), fshare * yptmd, gfeghdx)) %>% 
  # Rename Haver codes for clarity
  define_variables() %>%
  # Specify time series structure:
  # Key is historical or forecast period
  # Indexed by date
  as_tsibble(key = id, index = date) %>%
  # Calculate GDP growth for data but take CBO for projection
  mutate_where(id == 'historical',
               real_potential_gdp_growth = q_g(real_potential_gdp)) %>% 
  # Net out unemployment insurance, rebate checks, and Medicare to apply different MPC's
  mutate(
    federal_social_benefits = if_else(is.na(medicare), federal_social_benefits - ui, federal_social_benefits - ui - rebate_checks - medicare),
    state_social_benefits = if_else(is.na(medicaid), state_social_benefits, state_social_benefits - medicaid),
    social_benefits = federal_social_benefits + state_social_benefits,
    consumption_grants = if_else(is.na(medicaid_grants), gross_consumption_grants, gross_consumption_grants - medicaid_grants) 
  ) %>% 
  mutate(rebate_checks_arp = if_else(date == yearquarter("2021 Q1"),
                                     1348.1,
                                     0)) %>%
  mutate_where(id == 'projection',
               rebate_checks_arp = NA,
               federal_ui = NA,
               state_ui = NA) %>% 
  mutate_where(date == yearquarter('2021 Q1'),
               rebate_checks = rebate_checks - rebate_checks_arp,
               federal_social_benefits = federal_social_benefits + 203
  ) %>% 
  mutate(
         
         # Aggregate taxes
         corporate_taxes = federal_corporate_taxes + state_corporate_taxes,
         non_corporate_taxes = federal_non_corporate_taxes + state_non_corporate_taxes) %>% 
  mutate_where(id == 'projection',
               consumption_grants_deflator_growth = state_purchases_deflator_growth,
               investment_grants_deflator_growth = state_purchases_deflator_growth) %>% 
  mutate_where(date >= yearquarter('2020 Q2') & date <= yearquarter('2021 Q3'),
               consumption_grants = overrides$consumption_grants_override) 

# Forecast ----------------------------------------------------------------
forecast <- 
  readxl::read_xlsx('data/forecast.xlsx',
                    sheet = 'forecast') %>% 
  select(-15:-17, -name) %>% 
  pivot_longer(-variable) %>% 
  pivot_wider(names_from = 'variable',
              values_from = 'value') %>% 
  rename(date = name) %>% 
  mutate(date = yearquarter(date))


projections <- coalesce_join(usna, forecast, by = 'date') %>%
  
  mutate(# Coalesce NA's to 0
    across(where(is.numeric),
           ~ coalesce(.x, 0))) %>%
  mutate(
    health_outlays = medicare + medicaid,
    federal_health_outlays = medicare + medicaid_grants,
    state_health_outlays = medicaid - medicaid_grants
  )

# Consumption -------------------------------------------------------------

consumption <-
  projections %>%
  taxes_transfers_minus_neutral() %>%
  calculate_mpc("social_benefits") %>%
  mutate(rebate_checks_post_mpc = mpc_rebate_checks(rebate_checks_minus_neutral)) %>%
  calculate_mpc("subsidies") %>%
  calculate_mpc("health_outlays") %>%
  calculate_mpc("corporate_taxes") %>%
  calculate_mpc("non_corporate_taxes") %>% 
  mutate(across(c(federal_ui_minus_neutral, state_ui_minus_neutral),
                .fns = ~ if_else(date < yearquarter("2021 Q2"),
                                 mpc_ui(.x),
                                 mpc_ui_arp(.x)),
                .names = '{.col}_post_mpc')) %>% 
  
  mutate(across(
    .cols = all_of(
      c(
        "rebate_checks_arp",
        "federal_other_direct_aid_arp",
        "federal_other_vulnerable_arp",
        # "federal_ui_arp",
        #"state_ui_arp",
        "federal_aid_to_small_businesses_arp"
      )
    ),
    .fns = ~ .x - dplyr::lag(.x, default = 0) * (1 + real_potential_gdp_growth + consumption_deflator_growth),
    .names = "{.col}_minus_neutral"
  )) %>% 
  mutate(
    across(
      .cols = any_of(
        c("federal_ui_arp", "state_ui_arp", "federal_other_vulnerable_arp") %>% paste0("_minus_neutral")
      ),
      .fns = ~ mpc_vulnerable_arp(.x),
      .names = "{.col}_post_mpc"
    ),
    across(
      .cols = all_of(
        c("rebate_checks_arp", "federal_other_direct_aid_arp") %>% paste0("_minus_neutral")
      ),
      .fns = ~ mpc_direct_aid_arp(.),
      .names = "{.col}_post_mpc"
    ),
    
    federal_aid_to_small_businesses_arp_minus_neutral_post_mpc = mpc_small_businesses_arp((federal_aid_to_small_businesses_arp_minus_neutral))
  )

# Contribution ------------------------------------------------------------

contributions <-
  consumption %>%
  purchases_contributions() %>% 
  mutate(across(ends_with("post_mpc"),
                ~ 400 * .x / lag(gdp),
                .names = "{.col}_contribution"
  )) %>%
  rename_with(~ str_replace(.x, "_minus_neutral_post_mpc_contribution", "_contribution")) %>% 
  rename_with(~ str_replace(.x, "minus_neutral_post_mpc", "post_mpc")) %>% 
  rename_with(~ str_replace(.x, "post_mpc_contribution", "contribution")) %>% 
  sum_transfers_contributions() %>% 
  
  mutate(
    grants_contribution = consumption_grants_contribution + investment_grants_contribution,
    federal_contribution = federal_purchases_contribution + grants_contribution,
    state_contribution = state_purchases_contribution - grants_contribution
  ) %>%
  mutate(social_benefits_contribution = federal_social_benefits_contribution + state_social_benefits_contribution) %>%
  mutate(non_corporate_taxes_contribution = federal_non_corporate_taxes_contribution + state_non_corporate_taxes_contribution) %>%
  mutate(taxes_contribution = non_corporate_taxes_contribution + corporate_taxes_contribution) %>%
  mutate(
    transfers_contribution = federal_social_benefits_contribution + state_social_benefits_contribution +
      rebate_checks_contribution + rebate_checks_arp_contribution + federal_ui_contribution + state_ui_contribution +
      federal_subsidies_contribution + federal_aid_to_small_businesses_arp_contribution +  state_subsidies_contribution + federal_health_outlays_contribution +
      state_health_outlays_contribution + federal_other_direct_aid_arp_contribution + federal_other_vulnerable_arp_contribution,
    taxes_contribution = federal_non_corporate_taxes_contribution + state_non_corporate_taxes_contribution +
      federal_corporate_taxes_contribution + state_corporate_taxes_contribution
  ) %>%
  #sum_taxes_contributions() %>%
  get_fiscal_impact()





# Web materials  -------------------------------------------------------------

openxlsx::write.xlsx(contributions, file = 'analysis/fim-historical/contributions.xlsx', overwrite = TRUE)

# Interactive data
interactive <- 
  contributions %>% 
  mutate(consumption = transfers_contribution + taxes_contribution,
         recession = recode(recession, `-1` = 0),
         recession = replace_na(recession, 0),
         id = recode(id, 
                     historical = 0,
                     projection = 1)) %>% 
  select(date, 
         impact = fiscal_impact_moving_average,
         recession,
         total = fiscal_impact,
         federal = federal_contribution,
         state_local = state_contribution,
         consumption,
         projection = id) %>% 
  separate(date, c('year', 'quarter'))

readr::write_csv(interactive,  file = glue('analysis/fim-historical/interactive-1960-2021.csv'))




# Chart -------------------------------------------------------------------

# Subset data
headline <-
  contributions %>%
  select(date, fiscal_impact, fiscal_impact_moving_average) %>%
  pivot_longer(fiscal_impact) %>% 
  filter(date <= yearquarter("2023 Q2"))

# Legend formatting
guidez <- guides(
  fill = guide_legend(
    keywidth = unit(0.8, "cm"),
    keyheight = unit(0.4, "cm"),
    ncol = 1
  ),
  colour = guide_legend(
    keywidth = unit(0.8, "cm"),
    keyheight = unit(0.05, "cm"),
    ncol = 1
  )
)


# Recessions data

recessions <-
  history %>%
  as_tibble() %>%
  select(date, recession = recessq) %>%
  mutate(
    date,
    diff = recession - dplyr::lag(recession),
    business_cycle = case_when(
      diff == 2 ~ 'recession_start',
      diff == -2 ~ 'recession_end',
      recession == 1 ~ 'recession',
      recession == -1 ~ 'expansion'
    ),
    .keep = 'used'
  ) %>%
  filter(business_cycle == 'recession_start' |
           business_cycle == 'recession_end') %>%
  pivot_longer(business_cycle) %>%
  mutate(date2 = as_date(date)) %>%
  pivot_wider(names_from = value,
              values_from = date) %>%
  select(recession_start, recession_end) %>%
  mutate(
    across(any_of(c(
      'recession_start', 'recession_end'
    )),
    .fns = ~ coalesce(.x, dplyr::lead(.x))),
    recession_end = dplyr::lead(recession_end),
    .keep = 'used'
  ) %>%
  unique() %>%
  drop_na()


  ggplot(data = headline) +
  geom_rect(data = recessions,
            aes(xmin = recession_start, 
                xmax = recession_end, 
                ymin=-Inf,
                ymax=+Inf),
            fill = 'grey',
            alpha = 0.5) +
  geom_col(aes(x = date, y = value, fill = name),
           width = 50) +
  geom_line(aes(x = date, 
                y = fiscal_impact_moving_average,
                colour = "4-quarter moving-average")) +
  geom_point(aes(x = date,
                 y = fiscal_impact_moving_average,
                 colour = "4-quarter moving-average"),
             size = 1) +
  ggtext::geom_richtext(aes(x = yearquarter(today()) + 4,
                            y = 16),
                        label = "Projection",
                        cex = 2,
                        fill = NA, 
                        label.color = NA) +
  fim::scale_fill_fim(palette = 'headline',
                      labels = " Quarterly fiscal impact")  +
  scale_color_manual(" ",
                     values = c("4-quarter moving-average" = "black")) +
  annotate("rect", xmin = yearquarter('2021 Q3'), xmax = yearquarter('2023 Q2'),
           ymin = -Inf, ymax = Inf, alpha = 0.1, fill = 'yellow') +
  guides(fill = guide_legend(keywidth = unit(0.8, "cm"),
                             keyheight = unit(0.4, "cm"),
                             ncol = 1),
         colour = guide_legend(keywidth = unit(0.8, "cm"),
                               keyheight = unit(0.05, "cm"),
                               ncol = 1)) +
  fim::fim_theme() +
  guides(fill = guide_legend(keywidth = unit(0.8, "cm"),
                             keyheight = unit(0.4, "cm"),
                             ncol = 1),
         colour = guide_legend(
           keywidth = unit(0.8, "cm"),
           keyheight = unit(0.05, "cm"),
           ncol = 1
         ))+
  labs(title = glue("**Hutchins Center Fiscal Impact Measure: Total**"),
       x = NULL,
       y = NULL,
       subtitle = "Fiscal Policy Contribution to Real GDP Growth, percentage points",
       caption = "Source: Hutchins Center calculations from Bureau of Economic Analysis 
        and Congressional Budget Office data; grey shaded areas indicate recessions 
        and yellow shaded areas indicate projection.") 

