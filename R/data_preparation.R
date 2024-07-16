
define_variables <- function(df){
  df %>%
    transmute(date,
              id,
              gdp = gdp,
              gdp_growth,
              real_gdp = gdph,
              real_gdp_growth = q_g(real_gdp),
              
              gdp_deflator = jgdp,
              gdp_deflator_growth = jgdp_growth,
              real_potential_gdp = gdppothq,
              real_potential_gdp_growth  = gdppothq_growth,
              consumption = c,
              real_consumption = ch,
              consumption_deflator = jc,
              consumption_deflator_growth  = jc_growth,
              cpiu,
              recession = recessq,
              unemployment_rate,
              
              # PURCHASES
              purchases = g, 
              federal_purchases = gf,
              real_federal_purchases = gfh,
              federal_purchases_deflator = jgf,
              federal_purchases_deflator_growth = jgf_growth,
              state_purchases = gs,
              state_purchases_deflator = jgs,
              consumption_grants_deflator = jgse,
              consumption_grants_deflator_growth = q_g(consumption_grants_deflator),
              investment_grants_deflator = jgsi,
              investment_grants_deflator_growth = q_g(jgsi),
              real_state_purchases = gsh,
              state_purchases_deflator = jgs,
              state_purchases_deflator_growth = jgs_growth,
              
              # GRANTS
              health_grants = gfeghhx,
              medicaid_grants = gfeghdx,
              gross_consumption_grants = gfeg,
              investment_grants = gfeigx,
              coronavirus_relief_fund = gfegc, 
              education_stabilization_fund = gfege,
              # Half of  hospitals are non-profits
              provider_relief_fund = gfegv,
              
              
              # SUBSIDIES
              subsidies = gsub,
              federal_subsidies = gfsub,
              state_subsidies = gssub,
              ppp =gfsubp, 
              coronavirus_food_assistance = gfsubf,
              employee_retention = gfsube,
              paid_sick_leave = gfsubk,
              aviation = gfsubg,
              provider_relief_fund_subsidies = gfsubv,
              transit = gfsubs,
              
              # Transfers
              social_benefits = gtfp,
              federal_social_benefits = gftfp,
              state_social_benefits = gstfp,
              medicare = yptmr,
              medicaid = yptmd,
              ui = yptu,
              ui_expansion = gftfpu, 
              ui_extended_benefits = yptub,
              peuc = yptue,
              pua = yptup,
              puc = yptuc, 
              wages_lost_assistance = coalesce(yptol, 0),
              federal_ui = coalesce(ui_expansion, 0) +  wages_lost_assistance,
              state_ui = ui - federal_ui,
              rebate_checks = gftfpe,
              nonprofit_ppp = gftfpp,
              nonprofit_provider_relief_fund =gftfpv, 
              medicare_reimbursement_increase = gftfpr,
              
              
              across(c('peuc', 'pua', 'puc',  'wages_lost_assistance', 'rebate_checks', 'nonprofit_ppp',
                       'nonprofit_provider_relief_fund','coronavirus_relief_fund', 'education_stabilization_fund',
                       'provider_relief_fund'),
                     ~ coalesce(.x, 0)),
              
              # Taxes
              personal_taxes = yptx,
              production_taxes = ytpi,
              payroll_taxes = grcsi,
              non_corporate_taxes = personal_taxes + production_taxes + payroll_taxes,
              corporate_taxes = yctlg,
              
              federal_personal_taxes =  gfrpt,
              federal_production_taxes = gfrpri,
              federal_payroll_taxes = gfrs,
              federal_non_corporate_taxes = federal_personal_taxes + federal_production_taxes + federal_payroll_taxes,
              federal_corporate_taxes = gfrcp,
              
              state_personal_taxes =  gsrpt,
              state_production_taxes = gsrpri,
              state_payroll_taxes = gsrs,
              state_non_corporate_taxes = state_personal_taxes + state_production_taxes + state_payroll_taxes,
              state_corporate_taxes = gsrcp#,
              
              # GROWTH
              # consumption_growth = c_growth,
              # real_consumption_growth = ch_growth,
              #consumption_deflator_growth =  dc_growth,
              
              # federal_social_benefits_growth = gftfp_growth,
              # federal_personal_taxes_growth = gfrpt_growth,
              # federal_production_taxes_growth = gfrpri_growth,
              # federal_payroll_taxes_growth = gfrs_growth,
              # federal_corporate_taxes_growth = gfrcp_growth,
              # medicare_growth = yptmr_growth,
              # medicaid_growth = yptmd_growth,
              # ui_growth  =  yptu_growth,
              # purchases_growth = g_growth, 
              # real_purchases_growth =  gh_growth,
              # federal_purchases_growth = gf_growth,
              # real_federal_purchases_growth = gfh_growth,
              # state_purchases_growth = gs_growth,
              # real_state_purchases_growth = gsh_growth,
    ) 
  
  
}



reallocations <- function(df) {
  df %>%
    mutate(
      # Reallocate Consumption Grants
      consumption_grants = gross_consumption_grants - medicaid_grants - coronavirus_relief_fund - education_stabilization_fund - provider_relief_fund,
      grants = consumption_grants + investment_grants,
      
      health_outlays = medicare + medicaid,
      federal_health_outlays = medicare + medicaid_grants,
      state_health_outlays = medicaid - medicaid_grants,
      
      # Reallocate Subsidies
      federal_subsidies = federal_subsidies - ppp - aviation - paid_sick_leave - employee_retention,
      subsidies = federal_subsidies + state_subsidies,
      # Social Benefits
      social_benefits_gross = social_benefits,
      federal_social_benefits_gross = federal_social_benefits,
      state_social_benefits_gross = state_social_benefits,
      
      federal_social_benefits = federal_social_benefits - federal_ui - medicare - rebate_checks - nonprofit_provider_relief_fund - nonprofit_ppp ,
      state_social_benefits = state_social_benefits - medicaid
    )
}


reallocate_legislation <- function(.data){
  .data %>% 
    mutate(
      # Reallocate Consumption Grants
      consumption_grants = gross_consumption_grants - medicaid_grants - coronavirus_relief_fund - education_stabilization_fund - provider_relief_fund,
      grants = consumption_grants + investment_grants,
      # Reallocate Subsidies
      # federal_subsidies = federal_subsidies - ppp - aviation - paid_sick_leave - employee_retention,
      # subsidies = federal_subsidies + state_subsidies,
      # Reallocate social benefits
      federal_social_benefits = federal_social_benefits -  medicare - rebate_checks - nonprofit_provider_relief_fund - nonprofit_ppp,
      state_social_benefits = state_social_benefits - medicaid ,
      social_benefits = federal_social_benefits + state_social_benefits
    ) 
}


#' Generate a Cumulative Series Based on a Seed and Growth Rates
#'
#' This function calculates a cumulative series starting from a seed value and
#' applying a sequence of growth rates. 
#'
#' @param seed The initial starting value for the series.
#' @param growth_rates A vector of growth rates to apply to the seed. Values are
#' represented as a proportion, such as 1.02 (2% growth) or 0.98 (2% decline). The 
#' first element represents the growth rate between the seed period and the subsequent
#' period; the second element represents the growth rate between the second and 
#' third period; and so on.
#'
#' @return A numeric vector representing the cumulative series. The result is the same
#' length as the growth_rates vector and excludes the seed value.
#' 
#' @export
#'
#' @examples
#' seed <- 100
#' growth_rates <- c(1.05, 1.03, -0.98, 1.04)
#' generate_series(seed, growth_rates)
#' TODO: add an option to append or not append seed value to beginning of result
#' vector. Also, add data input checks.
cumulative_series <- function(seed, growth_rates) {
  result <- c(seed*cumprod(growth_rates))
  return(result)
}


#' 
#' 
#' 
#' GROW CURRENT GDP WITH CBO GROWTH RATE
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
gdp_cbo_growth_rate <- function(df){
  hist_length = length(which(df$id == "historical"))+2
  df_length = length(df$id)
  
  seed_gdp = df$gdp[df$date == current_quarter]
  seed_growth = df$gdp_growth[df$date == current_quarter+1]
  
  df$gdp[df$date==current_quarter+1]<- seed_gdp*(1+seed_growth)
  
  for (i in hist_length:df_length) {
    df$gdp[i] <- (1 + df$gdp_growth[i]) * df$gdp[i - 1]
  }
  
  seed_gdph = df$gdph[df$date == current_quarter]
  seed_growth = df$gdph_growth[df$date == current_quarter+1]
  
  df$gdph[df$date==current_quarter+1]<- seed_gdph*(1+seed_growth)
  
  for (i in hist_length:df_length) {
    df$gdph[i] <- (1 + df$gdph_growth[i]) * df$gdph[i - 1]
  }
  usna<-df
}
