temp <-
  projections %>% 
            mpc_apply(federal_corporate_taxes, 
                      federal_non_corporate_taxes,
                      federal_social_benefits,
                      federal_health_outlays,
                      federal_ui,
                      rebate_checks,
                      rebate_checks_arp,
                      federal_subsidies,
                      federal_aid_to_small_businesses_arp,
                      federal_other_direct_aid_arp,
                      federal_other_vulnerable_arp,
                      
                      state_corporate_taxes, 
                      state_non_corporate_taxes,
                      state_social_benefits,
                      state_health_outlays,
                      state_ui,
                      state_subsidies,
            ) 
