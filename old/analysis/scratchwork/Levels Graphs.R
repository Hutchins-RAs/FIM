###########################################################################
#Getting the levels variables-- applying the MPC but not minus neutral
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
  "dplyover"
)

options(digits = 4)
options(scipen = 20)
devtools::load_all()


# Wrangle data ------------------------------------------------------------

overrides <- readxl::read_xlsx('data/forecast_06_2021.xlsx',
                               sheet = 'historical overrides') %>% 
  select(-name) %>% 
  pivot_longer(-variable) %>% 
  pivot_wider(names_from = 'variable',
              values_from = 'value') %>% 
  rename(date = name) %>% 
  mutate(date = yearquarter(date))
# Load national accounts data from BEA

usna <-
  read_data() %>%
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
    federal_social_benefits = federal_social_benefits - ui - rebate_checks - medicare,
    state_social_benefits = state_social_benefits - medicaid,
    social_benefits = federal_social_benefits + state_social_benefits,
    consumption_grants = gross_consumption_grants - medicaid_grants,
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
  mutate(consumption_grants = gross_consumption_grants - medicaid_grants,
         
         # Aggregate taxes
         corporate_taxes = federal_corporate_taxes + state_corporate_taxes,
         non_corporate_taxes = federal_non_corporate_taxes + state_non_corporate_taxes,
         
         
  ) %>% 
  mutate_where(id == 'projection',
               consumption_grants_deflator_growth = state_purchases_deflator_growth,
               investment_grants_deflator_growth = state_purchases_deflator_growth) %>% 
  mutate_where(date >= yearquarter('2020 Q2') & date <= yearquarter('2021 Q1'),
               consumption_grants = overrides$consumption_grants_override) 







# Forecast ----------------------------------------------------------------
forecast <- readxl::read_xlsx('data/forecast_06_2021.xlsx',
                              sheet = 'forecast') %>% 
  select(-name) %>% 
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
  mutate(social_benefits_minus_neutral= social_benefits, federal_social_benefits_minus_neutral = federal_social_benefits, state_social_benefits_minus_neutral = state_social_benefits, health_outlays_minus_neutral = health_outlays, federal_health_outlays_minus_neutral = federal_health_outlays, state_health_outlays_minus_neutral = state_health_outlays, subsidies_minus_neutral = subsidies, federal_subsidies_minus_neutral = federal_subsidies, state_subsidies_minus_neutral = state_subsidies, federal_ui_minus_neutral = federal_ui, state_ui_minus_neutral = state_ui, rebate_checks_minus_neutral = rebate_checks, corporate_taxes_minus_neutral = corporate_taxes,federal_corporate_taxes_minus_neutral = federal_corporate_taxes, state_corporate_taxes_minus_neutral = state_corporate_taxes,  non_corporate_taxes_minus_neutral = non_corporate_taxes, federal_non_corporate_taxes_minus_neutral = federal_non_corporate_taxes, state_non_corporate_taxes_minus_neutral = state_non_corporate_taxes)  %>% 
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
    .fns = ~ .x,
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

##############################################################################
levels <- consumption %>% select(date, consumption_grants, investment_grants, federal_purchases, state_purchases,ends_with('post_mpc'))%>% filter_index("2019 Q4" ~ "2024 Q1") %>% mutate(grants = consumption_grants + investment_grants) %>% mutate(state_purchases_minus_grants = state_purchases - grants) 

#Purchases
federal_purchases <-ggplot(data=levels, aes(x=date, y=federal_purchases)) + geom_bar(stat="identity") + ggtitle("Federal Purchases (NIPA Consistent, no Grants)")+ theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_line(colour = "grey"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank())
federal_purchases 

state_purchases <-ggplot(data=levels, aes(x=date, y=state_purchases_minus_grants)) + geom_bar(stat="identity")+ ggtitle("State Purchases (FIM Consistent, no Grants)") + theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_line(colour = "grey"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank())
state_purchases

#Grants
consumption_grants <-ggplot(data=levels, aes(x=date, y=consumption_grants)) +
  geom_bar(stat="identity") + ggtitle("Consumption Grants")+ theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.line = element_line(colour = "grey"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank())
consumption_grants 

investment_grants <-ggplot(data=levels, aes(x=date, y=investment_grants)) +
   geom_bar(stat="identity") + ggtitle("Investment Grants") + theme(
     axis.title.x = element_blank(),
     axis.title.y = element_blank(),
     axis.line = element_line(colour = "grey"),
     panel.border = element_blank(),
     panel.grid.major = element_blank(),
     panel.background = element_blank(),
     panel.grid.minor = element_blank())

investment_grants

#Subsidies
federal_subsidies <-ggplot(data=levels, aes(x=date, y=federal_subsidies_post_mpc)) +geom_bar(stat="identity") + ggtitle("Non-ARP Subsidies plus ARP Provider Relief and PPP")+ theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_line(colour = "grey"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank())
federal_subsidies 

federal_aid_to_small_businesses_arp <- ggplot(data=levels, aes(x=date, y=federal_aid_to_small_businesses_arp_minus_neutral_post_mpc)) +geom_bar(stat="identity") + ggtitle("ARP Subsidies less Provider Relief and PPP")+ theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_line(colour = "grey"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank())
federal_aid_to_small_businesses_arp 


#Taxes
federal_non_corporate_taxes <-ggplot(data=levels, aes(x=date, y=federal_non_corporate_taxes_post_mpc)) +geom_bar(stat="identity")+ ggtitle("Federal Non-Corporate Taxes")+ theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_line(colour = "grey"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank())
federal_non_corporate_taxes 

state_non_corporate_taxes <-ggplot(data=levels, aes(x=date, y=state_non_corporate_taxes_post_mpc)) +geom_bar(stat="identity") + ggtitle("State Non-Corporate Taxes") + theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_line(colour = "grey"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank())
state_non_corporate_taxes 

federal_corporate_taxes <-ggplot(data=levels, aes(x=date, y=federal_corporate_taxes_post_mpc)) +geom_bar(stat="identity") + ggtitle("Federal Corporate Taxes")+ theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_line(colour = "grey"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank())
federal_corporate_taxes 

state_corporate_taxes <-ggplot(data=levels, aes(x=date, y=state_corporate_taxes_post_mpc)) +geom_bar(stat="identity") + ggtitle("State Corporate Taxes")+ theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_line(colour = "grey"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank())
state_corporate_taxes 

#Rebate Checks
rebate_checks_non_arp<-ggplot(data=levels, aes(x=date, y=rebate_checks_post_mpc)) + geom_bar(stat="identity")  + ggtitle("Non-ARP Rebate Checks")+ theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_line(colour = "grey"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank())
rebate_checks_non_arp 

rebate_checks_arp <-ggplot(data=levels, aes(x=date, y=rebate_checks_arp_minus_neutral_post_mpc)) + geom_bar(stat="identity") + ggtitle("ARP Rebate Checks")+ theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_line(colour = "grey"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank())
rebate_checks_arp 

#UI
federal_ui <-ggplot(data=levels, aes(x=date, y=federal_ui_minus_neutral_post_mpc)) +geom_bar(stat="identity")  + ggtitle("Federal Unemployment Insurance")+ theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_line(colour = "grey"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank())
federal_ui 

state_ui <-ggplot(data=levels, aes(x=date, y=state_ui_minus_neutral_post_mpc)) +geom_bar(stat="identity")  + ggtitle("State Unemployment Insurance")+ theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_line(colour = "grey"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank())
state_ui 
 
#Health Outlays
federal_health_outlays <-ggplot(data=levels, aes(x=date, y=federal_health_outlays_post_mpc)) +geom_bar(stat="identity")  + ggtitle("Federal Health Outlays")+ theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_line(colour = "grey"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank())
federal_health_outlays

state_health_outlays <-ggplot(data=levels, aes(x=date, y=state_health_outlays_post_mpc)) +geom_bar(stat="identity")  + ggtitle("State Health Outlays") + theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_line(colour = "grey"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank())
state_health_outlays 

#Other Social Benefits
federal_social_benefits <-ggplot(data=levels, aes(x=date, y=federal_social_benefits_post_mpc)) +geom_bar(stat="identity") + ggtitle("Federal Social Benefits")+ theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_line(colour = "grey"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank())
federal_social_benefits 

state_social_benefits <-ggplot(data=levels, aes(x=date, y=state_social_benefits_post_mpc)) +geom_bar(stat="identity") +ggtitle("State Social Benefits") + theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_line(colour = "grey"),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.background = element_blank(),
  panel.grid.minor = element_blank())
state_social_benefits

#ARP Other Vulnerable and Other Direct Aid
federal_other_vulnerable_arp <- ggplot(data=levels, aes(x=date, y=federal_other_vulnerable_arp_minus_neutral_post_mpc)) +geom_bar(stat="identity") + ggtitle("Other Vulnerable (ARP)")+ theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  axis.line = element_line(colour = "grey"),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_blank(),
    panel.grid.minor = element_blank()) 
federal_other_vulnerable_arp 

federal_other_direct_aid_arp <- ggplot(data=levels, aes(x=date, y=federal_other_direct_aid_arp_minus_neutral_post_mpc)) +geom_bar(stat="identity") + ggtitle("Other Direct Aid (ARP)") + theme(
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
axis.line = element_line(colour = "grey"),
panel.border = element_blank(),
panel.grid.major = element_blank(),
panel.background = element_blank(),
panel.grid.minor = element_blank())
federal_other_direct_aid_arp 


federal_purchases
state_purchases
consumption_grants
investment_grants
federal_subsidies 
federal_aid_to_small_businesses_arp
federal_non_corporate_taxes
state_non_corporate_taxes
federal_corporate_taxes
state_corporate_taxes
rebate_checks_non_arp 
rebate_checks_arp 
federal_ui
state_ui 
federal_health_outlays
state_health_outlays 
federal_social_benefits 
state_social_benefits
federal_other_vulnerable_arp 
federal_other_direct_aid_arp


#Ask how to save as PDF (having trouble with the methods I'm finding online)




















