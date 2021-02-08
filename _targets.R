

librarian::shelf('tidyverse', 'zoo', 'TTR', 'tsibble', 'targets', 'tarchetypes', 'lubridate',
                 'alistaire47/pipecleaner')

devtools::load_all()
# Build workflow plan data frame.

options(tidyverse.quiet = TRUE)
options(crayon.enabled = FALSE)


# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.

# Set target-specific options such as packages.
tar_option_set(error = "workspace")

create_projections <- function(df){
  components <- get_components_names()
  df %>%  
    make_cumulative_growth_rates() %>%
    fill(components) %>%
    make_forecasts() %>% 
    sum_projections(gtfp, gftfp, gstfp) %>%
    sum_projections(yptx, gfrpt, gsrpt) %>%
    sum_projections(ytpi, gfrpri, gsrpri) %>%
    sum_projections(grcsi, gfrs, gsrs) %>%
    sum_projections(grcsi, gfrs, gsrs) %>%
    sum_projections(yctlg, gfrcp, gsrcp) %>%
    sum_projections(gsub, gfsub, gssub)
}

components_growth_rates <- function(df){
  df %>%
    purchases_growth() %>%
    transfers_growth() %>%
    health_growth() %>%
    subsidies_growth() %>%
    grants_growth() %>%
    deflators_growth() 
}


# End this file with a list of target objects.
last_hist_date <- '2020-12-31'
tar_plan(
  projections = 
    read_data() %>%
    cola_adjustment() %>%
    growth_rates() %>%
    project_state_taxes() %>%
    fmap_share_old() %>%
    components_growth_rates() %>%
    group_by(id) %>%
    mutate(forecast_period = if_else(date <= last_hist_date, 0, 1)) %>%
    mutate(historical = if_else(date > last_hist_date, 0, 1)) %>%
    create_projections() %>%
    medicaid_reallocation()
)


read_data() %>%
  rename(gdp = gdp,
         real_gdp = gdph,
         gdp_deflator = jgdp,
         consumption = c,
         real_consumption = ch,
         federal_purchases = gf,
         real_federal_purchases = gfh,
         state_purchases = gs,
         real_state_purchases = gsh,
         
         # GRANTS
         consumption_grants = gfeg,
         investment_grants = gfeigx,
         health_grants = gfeghhx,
         medicaid_grants = gfeghdx,
         coronavirus_relief_fund = gfegc,
         education_stabilization_fund = gfege,
         provider_relief_fund = gfegv,
         
         # SUBSIDIES
         subsidies = gsub,
         federal_subsidies = gfsub,
         state_subsidies = gssub,
         ppp = gfsubp,
         coronavirus_food_assistance = gfsubf,
         employee_retention = gfsube,
         paid_sick_leave = gfsubk,
         aviation = gfsubg,
         subsidies_provider_relief_fund,
         transit = gfsubs,
         
         
         
         
         # Transfers
         federal_social_benefits = gftfp,
         state_social_benefits = gstfp,
         medicare = yptmr,
         medicaid = yptmd,
         ui = yptu,
         ui_expansion = gftfpu, 
         peuc = yptue,
         pua = yptup,
         puc = yptuc,
         wages_lost_assistance = yptolm,
         rebate_checks = gftfpe,
         nonprofit_ppp = gftfpp,
         nonprofit_provider_relief_fund = gftfpv,
         
         
         
         # Taxes
         personal_taxes = yptx,
         production_taxes = ypti,
         payroll_taxes = grcsi,
         corporate_taxes = yctlg,
         
         federal_personal_taxes =  gfrpt,
         federal_production_taxes = gfrpri,
         federal_payroll_taxes = gfrs,
         federal_corporate_taxes = gfrcp,
         
         state_personal_taxes =  gsrpt,
         state_production_taxes = gsrpri,
         state_payroll_taxes = gsrs,
         state_corporate_taxes = gsrcp,
         
         
         )

