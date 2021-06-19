library('readxl')

arp_annual <- read_xlsx('data/american_rescue_plan.xlsx',
          range = 'A3:AV14') %>% 
  janitor::clean_names()

arp_timing <- read_xlsx('data/american_rescue_plan.xlsx', sheet = 'Timing Assumptions',
                        range = 'A2:I22') %>% 
  mutate(date = yearquarter(date, fiscal_start = 1)) %>% 
  as_tsibble(index = date)

fy_annual_to_quarter  <- function(df){
  year <-
    df %>%
    tsibble::index_var()
  min <-
    df %>%
    select(rlang::enexpr(year)) %>%
    min() 
  
  max <- 
    df %>%
    select(rlang::enexpr(year)) %>%
    max()
  start <- tsibble::yearquarter(glue::glue('{min} Q1'), fiscal_start = 1)
  end <- tsibble::yearquarter(glue::glue('{max} Q4'), fiscal_start = 1)
  x <- seq(start,  end, by = 1)
  
  df %>%
    as_tibble() %>%
    slice(rep(1:n(), each=4)) %>%
    
    mutate(date = tsibble::yearquarter(x)) %>%
    relocate(date, .before =  everything()) %>%
    tsibble::as_tsibble(index = date)
}


arp_annual %>% 
  as_tsibble(index = date) %>% 
  fy_annual_to_quarter() %>% 
  mutate(across(-date,
                ~ if_else(date > yearquarter('2021 Q3'),
                          lead(.x), .x)))
  
  start <- tsibble::yearquarter(glue::glue('2021 Q2'), fiscal_start = 10)
  end <- tsibble::yearquarter(glue::glue('2031 Q3'))
  x <- seq(start,  end, by = 1)
arp_quarterly <-
  arp_annual %>% 
  as_tsibble(index = date) %>% 
  fy_annual_to_quarter() %>% 
  mutate(across(-date,
                ~ if_else(date > yearquarter('2021 Q3'),
                          lead(.x), .x))) %>% 
  summarize(rebate_checks,
            other_direct_aid = child_tax_credit + eitc + child_care_for_workers + dependent_care_for_families,
            health_grants = medicaid + medicare + chip,
  
            non_health_grants = coronavirus_relief_fund +
              human_services_and_community_supports_minus_childcare_policies + provider_relief + title_9_other + education + covid_containment_vaccination + other_state_and_local + grants_to_tribal_governments + commerce_and_science_federal_spending +  other_transportation + child_care_and_development_block_grant_program + va +  mental_health + medical_supplies + homeland_security_grants + environment_grants + foreign_aid + miscellaneous_from_t9,
            consumption_grants_arp = coronavirus_relief_fund + provider_relief + education + grants_to_tribal_governments + other_state_and_local,
            federal_purchases_arp = non_health_grants - consumption_grants_arp,
            other_vulnerable = housing_assistance + food + emergency_assistance + cobra + premium_tax_credits + ratepayer_protection + assistance_for_older_americans,
            state_ui,
            federal_ui = pua + puc + peuc + ui_tax_suspension + other_ui,
            aid_to_small_businesses = ppp + child_care_stabilization + grants_to_small_businesses + small_business_credit_initiative + paid_sick_leave + employee_retention + pensions + transit_and_aviation_support,
            ppp,
            child_care_stabilization,  
            grants_to_small_businesses,
            small_business_credit_initiative,
            paid_sick_leave,
            employee_retention,
            pensions,
            coronavirus_relief_fund, 
            provider_relief,
            education,
            small_business_credit_initiative,
            other_state_and_local, 
            transit_and_aviation_support,
            grants_to_tribal_governments,
            .keep = '.used')  %>% 
  left_join(arp_timing, by = 'date') %>% 
  summarize(rebate_checks_arp = 4 * rebate_checks * rebate_checks_timing,
            other_direct_aid = 4 *other_direct_aid * other_direct_aid_timing,
            health_grants = 4 * health_grants * health_grants_timing,
            consumption_grants_arp = 4 * consumption_grants_arp * grants_timing,

            across(c(coronavirus_relief_fund, provider_relief, education,
                     other_state_and_local, grants_to_tribal_governments),
.fns = ~ 4 * .x * grants_timing                 ),
across(c(ppp, child_care_stabilization, grants_to_small_businesses, small_business_credit_initiative, paid_sick_leave, employee_retention, pensions, transit_and_aviation_support),
       ~ 4 * .x * aid_to_small_businesses_timing),
            non_health_grants = 4 * non_health_grants * grants_timing, 
            other_vulnerable = 4 * other_vulnerable * other_vulnerable_timing, 
            federal_ui_arp = 4 * federal_ui * ui_timing,
            state_ui_arp = 4 * state_ui * ui_timing,
            aid_to_small_businesses = 4 * aid_to_small_businesses * aid_to_small_businesses_timing) %>% 
  rename(
         federal_health_grants_arp = health_grants,
         federal_non_health_grants_arp = non_health_grants,
         federal_other_vulnerable_arp = other_vulnerable,
         federal_aid_to_small_businesses_arp = aid_to_small_businesses,
         federal_other_direct_aid_arp = other_direct_aid
         
         ) %>% 
  mutate_where(date == yearquarter('2021 Q3'),
               federal_ui_arp = 350)

openxlsx::write.xlsx(arp_quarterly, 'data/arp_summary.xlsx')
arp <- arp_quarterly
usethis::use_data(arp, overwrite = TRUE)

# arp_quarterly %>% 
#   mpc_arp_non_health_grants() %>% 
#   mutate(across(.cols = all_of(c('federal_ui_arp', 'state_ui_arp', 'other_vulnerable')),
#                 .fns = ~ mpc_vulnerable_arp(.x),
#                 .names = '{.col}_post_mpc'),
#          across(.cols = all_of(c('rebate_checks_arp', 'other_direct_aid')),
#                 .fns = ~ mpc_direct_aid_arp(.),
#                 .names = '{.col}_post_mpc'),
#          aid_to_small_businesses_post_mpc = mpc_small_businesses_arp((aid_to_small_businesses))
#          ) %>% 
#   mutate(arp = rebate_checks_arp + other_direct_aid + health_grants + non_health_grants + other_vulnerable + federal_ui_arp + state_ui_arp + aid_to_small_businesses,
#          arp_post_mpc = rebate_checks_arp_post_mpc + other_direct_aid_post_mpc + federal_ui_arp_post_mpc + state_ui_arp_post_mpc + other_vulnerable_post_mpc + non_health_grants_post_mpc + aid_to_small_businesses_post_mpc )





arp %>%
  summarise(date,
         total = coronavirus_relief_fund + education + provider_relief +
           grants_to_tribal_governments + other_state_and_local,
         coronavirus_relief_fund,
         education,
         provider_relief,
         grants_to_tribal_governments,
         other_state_and_local) %>%
  mutate(across(-date,
                ~ mpc_non_health_grants_arp(.x),
                .names = '{.col}_spending')) %>%
  select(date, ends_with('spending')) %>%
  pivot_longer(-date) %>%
  as_tibble() %>% 
  pivot_wider(names_from = date,
               values_from = value) 
#   openxlsx::write.xlsx('grants_spending_arp.xlsx')
# 
# 

arp %>% 
  select(date, ppp, child_care_stabilization,grants_to_small_businesses,
         small_business_credit_initiative,
         paid_sick_leave,
         employee_retention,
         pensions,
         transit_and_aviation_support) %>% 
  as_tibble() %>% 
  pivot_longer(-date) %>% 
  pivot_wider(names_from = date,
              values_from = value) %>% 
  openxlsx::write.xlsx('subsidies_arp.xlsx')




