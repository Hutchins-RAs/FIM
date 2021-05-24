# Setup -------------------------------------------------------------------
librarian::shelf(
  "tidyverse",
  "zoo",
  "TTR",
  "tsibble",
  "targets",
  "tarchetypes",
  "lubridate",
  "alistaire47/pipecleaner",
  "glue",
  "validate",
  "fim",
  "dplyover",
  "tsibble",
  "magrittr"
)

# Wrangle data ------------------------------------------------------------
usna <-
  read_data() %>%
  # Don't sneak in ui reallocation here.
  define_variables() %>%
  as_tsibble(key = id, index = date) %>%
  create_override(
    var = state_purchases_growth,
    start = yearquarter("2021 Q2"),
    end = yearquarter("2023 Q2"),
    values = c(
      0.024113689,
      0.019426547,
      0.015868285,
      0.015868285,
      0.015868285,
      0.014673846,
      0.013475174,
      0.012272234,
      0.012272234
    )
  ) %>%
  growth_assumptions() %>%
  ungroup() %>%
  mutate(real_potential_gdp_growth = q_g(real_potential_gdp)) %>%
  mutate(
    federal_social_benefits = federal_social_benefits - ui - rebate_checks,
    social_benefits = federal_social_benefits + state_social_benefits
  )


## Remove ARP from USNA
arp <- readxl::read_xlsx("data/arp_summary.xlsx") %>%
  mutate(date = yearquarter(date))

usna_processed <-
  usna %>%
  left_join(arp, by = "date") %>%
  mutate(across(
    .cols = contains("arp"),
    .fns = ~ coalesce(.x, 0)
  )) %>%
  rename(rebate_checks_arp = federal_rebate_checks_arp) %>%
  mutate_where(
    id == "historical",
    federal_subsidies = federal_subsidies - federal_aid_to_small_businesses_arp,
    rebate_checks = rebate_checks - rebate_checks_arp,
    federal_ui_arp = 0,
    state_ui_arp = 0,
  ) %>%
  mutate(social_benefits = federal_social_benefits + state_social_benefits)



# Forecast ----------------------------------------------------------------
baseline_projections <-
  usna_processed %>%
  mutate(consumption_grants = gross_consumption_grants - medicaid_grants) %>%
  forecast() %>%
  ungroup() %>%
  mutate(
    grants = consumption_grants + investment_grants,
    state_social_benefits = state_social_benefits - medicaid,
    federal_social_benefits = federal_social_benefits - medicare,
    social_benefits = federal_social_benefits + state_social_benefits
  ) %>%
  mutate( # Health outlays reattribution
    health_outlays = medicare + medicaid,
    federal_health_outlays = medicare + medicaid_grants,
    state_health_outlays = medicaid - medicaid_grants,


    # Aggregate taxes
    corporate_taxes = federal_corporate_taxes + state_corporate_taxes,
    payroll_taxes = federal_payroll_taxes + state_payroll_taxes,
    production_taxes = federal_production_taxes + state_production_taxes,
    personal_taxes = federal_personal_taxes + state_personal_taxes,
    federal_non_corporate_taxes = federal_payroll_taxes + federal_personal_taxes + federal_production_taxes,
    state_non_corporate_taxes = state_payroll_taxes + state_personal_taxes + state_production_taxes,
    non_corporate_taxes = federal_non_corporate_taxes + state_non_corporate_taxes,

    # Coalesce NA's to 0
    across(
      where(is.numeric),
      ~ coalesce(.x, 0)
    )
  )


baseline_projections %>%
  filter_index("2020 Q1" ~ .) %>%
  as_tibble() %>%
  select(
    date,
    state_health_outlays,
    state_social_benefits,
    state_non_corporate_taxes,
    state_corporate_taxes,
    federal_health_outlays,
    federal_social_benefits,
    federal_subsidies,
    consumption_grants
  ) %>%
  pivot_longer(-date) %>%
  pivot_wider(
    names_from = date,
    values_from = value
  ) %>%
  openxlsx::write.xlsx("data/baseline_projections.xlsx")

# Add factors -------------------------------------------------------------
add_factors <-
  readxl::read_xlsx("data/add_factors.xlsx",
    sheet = "FIM Add Factors"
  ) %>%
  mutate(date = tsibble::yearquarter(date))
projections <-
  baseline_projections %>%
  left_join(add_factors, by = "date") %>%
  mutate(across(
    .cols = starts_with("add"),
    .fns = ~ if_else(id == "historical",
      0,
      .x
    )
  )) %>%
  mutate(across(
    c(
      federal_purchases,
      state_purchases,
      federal_social_benefits,
      state_social_benefits,
      federal_subsidies,
      federal_health_outlays,
      state_health_outlays
    ),
    ~ .x + get(paste0("add_", cur_column()))
  )) %>%
  mutate(consumption_grants = if_else(date >= yearquarter("2020 Q2"),
    consumption_grants_override,
    consumption_grants
  )) %>%
  mutate_where(id == "projection",
    federal_ui = federal_ui_override,
    state_ui = state_ui_override,
    ui = ui_override
  ) %>%
  mutate_where(date == yearquarter("2021 Q1"),
    ppp = 639.2,
    federal_subsidies = 753,
    subsidies = federal_subsidies + state_subsidies
  )

# American Rescue Plan ----------------------------------------------------
arp_contribution <-
  readxl::read_xlsx("data/arp_summary.xlsx") %>%
  mutate(date = yearquarter(date)) %>%
  as_tsibble(index = date) %>%
  append_row(-1) %>%
  mutate(across(where(is.numeric), ~ coalesce(.x, 0))) %>%
  full_join(baseline_projections %>% filter_index("2020 Q4" ~ .) %>% select(date, real_potential_gdp_growth, consumption_deflator_growth, consumption_grants_deflator_growth, gdp),
    by = "date"
  ) %>%
  rename(rebate_checks_arp = federal_rebate_checks_arp) %>%
  mutate_where(date == yearquarter("2021 Q1"),
    federal_ui_arp = 0,
    state_ui_arp = 0
  ) %>%
  mutate(across(
    .cols = all_of(
      c(
        "rebate_checks_arp",
        "federal_other_direct_aid_arp",
        "federal_health_grants_arp",
        "federal_non_health_grants_arp",
        "federal_other_vulnerable_arp",
        "federal_ui_arp",
        "state_ui_arp",
        "federal_aid_to_small_businesses_arp"
      )
    ),
    .fns = ~ .x - dplyr::lag(.x, default = 0) * (1 + real_potential_gdp_growth + consumption_deflator_growth),
    .names = "{.col}_minus_neutral"
  )) %>%
  mutate(federal_non_health_grants_arp_post_mpc = mpc_non_health_grants_arp(federal_non_health_grants_arp)) %>%
  mutate(federal_non_health_grants_arp_contribution = 400 * (
    federal_non_health_grants_arp_post_mpc - lag(federal_non_health_grants_arp_post_mpc) * (1 + real_potential_gdp_growth + consumption_grants_deflator_growth)
  ) / lag(gdp, default = 1)) %>%
  mutate(
    across(
      .cols = all_of(
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
    federal_health_grants_arp_minus_neutral_post_mpc = mpc_health_outlays(federal_health_grants_arp_minus_neutral),
    federal_aid_to_small_businesses_arp_minus_neutral_post_mpc = mpc_small_businesses_arp((federal_aid_to_small_businesses_arp_minus_neutral))
  ) %>%
  mutate(across(ends_with("post_mpc"),
    ~ 400 * .x / lag(gdp),
    .names = "{.col}_contribution"
  )) %>%
  rename_with(~ str_replace(.x, "_minus_neutral_post_mpc_contribution", "_contribution")) %>%
  mutate(date,
    consumption_grants_arp_contribution = federal_non_health_grants_arp_contribution,
    federal_health_outlays_arp_contribution = federal_health_grants_arp_contribution,
    federal_social_benefits_arp_contribution = federal_other_direct_aid_arp_contribution + federal_other_vulnerable_arp_contribution,
    federal_subsidies_arp_contribution = federal_aid_to_small_businesses_arp_contribution,
    rebate_checks_arp_contribution,
    federal_ui_arp_contribution,
    state_ui_arp_contribution
  ) %>%
  mutate(
    date,
    consumption_grants_arp_contribution,
    federal_transfers_arp_contribution =
      federal_social_benefits_arp_contribution +
        federal_subsidies_arp_contribution +
        federal_health_outlays_arp_contribution +
        federal_ui_arp_contribution +
        rebate_checks_arp_contribution,
    state_transfers_arp_contribution = state_ui_arp_contribution
  )





# Consumption -------------------------------------------------------------

baseline_consumption <-
  baseline_projections %>%
  taxes_transfers_minus_neutral() %>%
  calculate_mpc("social_benefits") %>%
  calculate_mpc("ui") %>%
  mutate(rebate_checks_post_mpc = mpc_rebate_checks(rebate_checks_minus_neutral)) %>%
  calculate_mpc("subsidies") %>%
  calculate_mpc("health_outlays") %>%
  calculate_mpc("corporate_taxes") %>%
  calculate_mpc("non_corporate_taxes")


consumption <-
  projections %>%
  taxes_transfers_minus_neutral() %>%
  calculate_mpc("social_benefits") %>%
  calculate_mpc("ui") %>%
  mutate(rebate_checks_post_mpc = mpc_rebate_checks(rebate_checks_minus_neutral)) %>%
  calculate_mpc("subsidies") %>%
  calculate_mpc("health_outlays") %>%
  calculate_mpc("corporate_taxes") %>%
  calculate_mpc("non_corporate_taxes")

# Contribution ------------------------------------------------------------

# Without add factors or ARP ---------
baseline_contribution <-
  baseline_consumption %>%
  purchases_contributions() %>%
  taxes_contributions() %>%
  sum_taxes_contributions() %>%
  transfers_contributions() %>%
  sum_transfers_contributions() %>%
  sum_taxes_transfers() %>%
  get_fiscal_impact()
# ------
contribution_no_arp <-
  consumption %>%
  purchases_contributions() %>%
  taxes_contributions() %>%
  sum_taxes_contributions() %>%
  transfers_contributions() %>%
  sum_transfers_contributions() %>%
  get_fiscal_impact()

contribution <-
  contribution_no_arp %>%
  coalesce_join(arp_contribution, by = c("date", "id")) %>%
  mutate(across(
    .cols = contains("arp"),
    .fns = ~ coalesce(.x, 0)
  )) %>%
  mutate(
    grants_contribution = consumption_grants_contribution + investment_grants_contribution,
    federal_contribution = federal_purchases_contribution + grants_contribution + consumption_grants_arp_contribution,
    state_contribution = state_purchases_contribution - grants_contribution
  ) %>%
  mutate(
    federal_transfers_contribution = federal_transfers_arp_contribution + federal_transfers_contribution,
    state_transfers_contribution = state_transfers_arp_contribution + state_transfers_contribution,
    transfers_contribution = federal_transfers_contribution + state_transfers_contribution
  ) %>%
  sum_taxes_transfers() %>%
  get_fiscal_impact()




contribution %>% 
  prepare_interactive() %>% View()
