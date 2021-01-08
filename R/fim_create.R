#' Create fim data frame
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
fim_create <- function(df){
  df %>%
    transmute(date = date,
              # REFERENCE VARIABLES
              gdp = gdp, # nominal gdp
              gdppot = q_g(gdppothq) + q_g(jgdp) , #  nominal potential output growth
              gdppoth = q_g(gdppothq), # real potential output growth
              pi_gdp = q_g(jgdp), # gdp "deflator"
              pce = c, # nominal consumption
              pi_pce = q_g(jc),
              recession = if_else(recessq == -1,
                                  0,
                                  recessq),   
              # GRANTS & NOMINAL SPENDING
              ## Federal
              federal_nom = gf, # SPENDING
              ## GRANTS
              federal_cgrants_gross  = gfeg,
              federal_health_grants = gfeghhx,
              federal_medicaid_grants = yfptmd, 
              federal_cgrants = federal_cgrants_gross - federal_medicaid_grants, # Federal (non-health) grants in aid to states
              federal_igrants = gfeigx, # Federal capital grants in aid to states, nominal
              pi_federal = q_g(jgf),
              ## State
              state_local_nom = gs ,
              pi_state_local = q_g(jgs) ,
              pi_state_local_c = q_g(jgse) ,
              pi_state_local_i = q_g(jgsi),
              federal_nom_pi = pi_federal,
              state_local_nom_pi = pi_state_local,
              federal_cgrants_pi = pi_state_local_c,
              federal_igrants_pi = pi_state_local_i,
              # TAXES AND TRANSFERS
              ## Total
              medicare = yptmr,
              medicaid = yptmd,
              health_outlays = medicare + medicaid, # Medicare + Medicaid
              unemployment_insurance = yptu,
              gtfp,
              social_benefits_gross = gtfp,
              social_benefits = social_benefits_gross - health_outlays, # Social benefits net health outlays
              personal_taxes = yptx,
              payroll_taxes = grcsi,
              production_taxes = ytpi,
              noncorp_taxes = personal_taxes + production_taxes + payroll_taxes, # alternative
              corporate_taxes = yctlg,
              subsidies  = gsub,
              rebate_checks = if_else(is.na(gftfpe), 0, gftfpe),
              ## Federal
              federal_medicaid = yfptmd,
              federal_health_outlays = medicare + federal_medicaid,
              federal_unemployment_insurance = federal_unemployment_insurance_override,
              federal_rebate_checks = rebate_checks,
              federal_social_benefits = gftfpnet - federal_health_outlays,
              federal_personal_taxes = gfrpt,
              federal_payroll_taxes = gfrs,
              federal_production_taxes  = gfrpri,
              federal_noncorp_taxes = federal_personal_taxes + federal_payroll_taxes + federal_production_taxes,
              federal_corporate_taxes = gfrcp,
              federal_subsidies = gfsub,
              ## State
              state_medicaid = medicaid - federal_medicaid,
              state_health_outlays = state_medicaid,
              state_social_benefits_gross = gstfp,
              state_unemployment_insurance = unemployment_insurance - federal_unemployment_insurance,
              state_rebate_checks = 0,
              state_social_benefits = state_social_benefits_gross - state_health_outlays - federal_medicaid_grants, # no medicare at state and local level
              state_personal_taxes = gsrpt,
              state_payroll_taxes = gsrs,
              state_production_taxes  = gsrpri,
              state_noncorp_taxes = state_personal_taxes + state_payroll_taxes + state_production_taxes,
              state_corporate_taxes = gsrcp,
              state_subsidies = gssub
    )
}