#######################
# Define Server Logic #
#######################

Sys.setenv(TZ = "UTC")

# Load all required packages
packages <- c("shiny", "tidyr", "dplyr", "lubridate", "tsibble", "zoo", "glue", 
              "readxl", "writexl", "shinyjs", "plotly", "shinycssloaders", "TTR")
librarian::shelf(packages)

# Source shiny_functions.R, a helper script re-writing some of the functions contained in the FIM package rather than loading the package. 
source('src/shiny_functions.R')

#------- Load the FIM Data ---------# 
# Read in the forecast sheet data 
data <- readxl::read_xlsx('cache/forecast.xlsx')

# Read in the Hutchins Center FIM output (we use this to create the final chart, which compares the user's results with ours)
load('cache/hutchins_fim.rda') 

# Read in the National Accounts data and historical overrides 
load('cache/usna.rda')
load('cache/historical_overrides.rda')

# Read in MPCs 
mpcs <- readxl::read_xlsx('cache/mpcs.xlsx')

# Set the Current Quarter 
current_quarter <- yearquarter(Sys.Date()) - 1

# Source the contributions R script, which defines the functions that are used to calculate the FIM contributions. 
source("src/shiny_contributions.R")

##########################
# DEFINE SERVER FUNCTION #
##########################

server <- function(input, output, session) {
  
  # Create download handler for the Excel file containing forecast data and MPCs
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("fim_data_download", ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(list(
        "FIM Data" = data,  # Sheet 1: Forecasts pulled from the forecast sheet
        "MPCs" = mpcs), file) # Sheet 2: MPC dataset created by shiny/src/get_mpcs.R
    }
  )
  
  # Reactive expression to retrieve the user-uploaded FIM data 
  forecast_user <- reactive({
    req(input$file)
    
    tryCatch({
      data <- read_xlsx(input$file$datapath, sheet = 1) %>%
        select(-name) %>%  # Remove the 'name' column from the data
        pivot_longer(-variable, names_to = 'date') %>%  # Reshape the data
        pivot_wider(names_from = 'variable', values_from = 'value')
      
      # Check if all values (excluding the date column) are numeric
      numeric_check <- data %>%
        select(-date) %>%  # Exclude the 'date' column
        summarise(across(everything(), ~ all(is.numeric(.x)))) %>%
        unlist() %>% 
        all()
      
      # If non-numeric values are found, display an error notification
      if (!numeric_check) {
        showNotification("Uh oh! The file you uploaded contains non-numeric values where numeric data is expected. Please check your file and try again.",
                         type = "error", duration = 30)
        return(NULL)
      }
      
      # Continue processing if all values are numeric
      data %>%
        mutate(date = yearquarter(date)) %>%  # Convert date to year-quarter format
        tsibble::as_tsibble(index = date)
    }, error = function(e) {
      
      # Show an error notification if the file is incorrect 
      showNotification("Uh oh! It seems the file you uploaded is not what 
                     we were expecting. Please make sure the file you upload matches the structure 
                     of our data download.",
                       type = "error", duration = 30)
      NULL 
    })
    
  })
  
  # Reactive expression to retrieve the user-uploaded MPCs
  mpcs_user <- reactive({
    req(input$file)  
    read_xlsx(input$file$datapath, sheet = 2) %>% 
      # Reshape the uploaded MPC data set 
      pivot_longer(cols = -Variable,  
                   names_to = "Quarter",  
                   values_to = "Value") %>%
      pivot_wider(names_from = Variable, 
                  values_from = Value) 
      
  })
  
  # Create projections dataset that joins national accounts, forecasts, and historical overrides
  projections <- reactive({
    req(forecast_user())
    ui_forecast <- data.frame(forecast_user())
    
    # Join the NIPAs (contained in the cache folder) with the user forecast
    base_data <- coalesce_join(usna, ui_forecast, by = 'date') %>% 
      mutate(across(where(is.numeric), ~ coalesce(.x, 0))) %>%
      mutate(
        federal_health_outlays = medicare + medicaid_grants,
        state_health_outlays   = medicaid - medicaid_grants
      )
    
    # Apply overrides
    historical_overrides_filtered <- historical_overrides %>%
      filter(date >= yearquarter('2020 Q2') & date <= current_quarter) %>%
      select(date, contains("override"))
    
    result <- base_data %>%
      left_join(historical_overrides_filtered, by = "date") %>%
      mutate(
        federal_other_direct_aid_arp = case_when(
          date >= yearquarter('2020 Q2') & date <= current_quarter & 
            !is.na(federal_other_direct_aid_arp_override) ~ federal_other_direct_aid_arp_override,
          TRUE ~ federal_other_direct_aid_arp
        ),
        federal_other_vulnerable_arp = case_when(
          date >= yearquarter('2020 Q2') & date <= current_quarter & 
            !is.na(federal_other_vulnerable_arp_override) ~ federal_other_vulnerable_arp_override,
          TRUE ~ federal_other_vulnerable_arp
        ),
        federal_social_benefits = case_when(
          date >= yearquarter('2020 Q2') & date <= current_quarter & 
            !is.na(federal_social_benefits_override) ~ federal_social_benefits_override,
          TRUE ~ federal_social_benefits
        ),
        federal_aid_to_small_businesses_arp = case_when(
          date >= yearquarter('2020 Q2') & date <= current_quarter & 
            !is.na(federal_aid_to_small_businesses_arp_override) ~ federal_aid_to_small_businesses_arp_override,
          TRUE ~ federal_aid_to_small_businesses_arp
        ),
        federal_corporate_taxes = case_when(
          date == current_quarter & is.na(federal_corporate_taxes) & is.na(state_corporate_taxes) & 
            !is.na(federal_corporate_taxes_override) ~ federal_corporate_taxes_override,
          TRUE ~ federal_corporate_taxes
        ),
        state_corporate_taxes = case_when(
          date == current_quarter & is.na(federal_corporate_taxes) & is.na(state_corporate_taxes) & 
            !is.na(state_corporate_taxes_override) ~ state_corporate_taxes_override,
          TRUE ~ state_corporate_taxes
        ),
        supply_side_ira = case_when(
          date >= yearquarter('2020 Q2') & date <= current_quarter & 
            !is.na(supply_side_ira_override) ~ supply_side_ira_override,
          TRUE ~ supply_side_ira
        ),
        federal_student_loans = case_when(
          date >= yearquarter('2020 Q2') & date <= current_quarter & 
            !is.na(federal_student_loans_override) ~ federal_student_loans_override,
          TRUE ~ federal_student_loans
        ),
        federal_social_benefits = case_when(
          date == yearquarter("2021 Q1") ~ federal_social_benefits + 203,
          TRUE ~ federal_social_benefits
        ),
        federal_ui = case_when(
          date == yearquarter('2021 Q4') ~ 11,
          TRUE ~ federal_ui
        ),
        state_ui = case_when(
          date == yearquarter('2021 Q4') ~ ui - 11,
          TRUE ~ state_ui
        )
      ) %>%
      select(-contains("override")) 
    
    # --- Pad with future quarters until 259 rows ---
    all_dates <- tibble(
      date = seq(yearquarter("2020 Q2"), by = 1, length.out = 259)
    )
    
    result_padded <- all_dates %>%
      left_join(result, by = "date") %>%
      mutate(across(where(is.numeric), ~ coalesce(.x, 0)))
    
    return(result_padded)
  })
  
  
  #########################
  # GENERATE MPC MATRICES #
  #########################
  # FIX ME: We need to make this more efficient, we should not be re-writing all the matrices 
  
  # Federal Non-Corporate Taxes MPC 
  federal_non_corporate_taxes_mpc <- reactive({
    req(mpcs_user())
    
    mpc_vector <- as.vector(mpcs_user()$federal_non_corporate_taxes_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
      
  })
  
  # State Non-Corporate Taxes MPC 
  state_non_corporate_taxes_mpc <- reactive({
    req(mpcs_user())
    
    mpc_vector <- as.vector(mpcs_user()$state_non_corporate_taxes_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
    
  })
  
  # Federal Corporate Taxes
  federal_corporate_taxes_mpc <- reactive({
    req(mpcs_user())
    
    mpc_vector <- as.vector(mpcs_user()$federal_corporate_taxes_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
    
  })
  
  # State Corporate Taxes
  state_corporate_taxes_mpc <- reactive({
    req(mpcs_user())
    
    mpc_vector <- as.vector(mpcs_user()$state_corporate_taxes_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
  })
  
  # Federal Social Benefits MPC 
  federal_social_benefits_mpc <- reactive({
    req(mpcs_user())
    
    mpc_vector <- as.vector(mpcs_user()$federal_social_benefits_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
    
  })
  
  # State Social Benefits MPC 
  state_social_benefits_mpc <- reactive({
    req(mpcs_user())
    
    mpc_vector <- as.vector(mpcs_user()$state_social_benefits_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
  })
  
  # Rebate Checks MPC 
  rebate_checks_mpc <- reactive({
    req(mpcs_user()) 
    
    mpc_vector <- as.vector(mpcs_user()$rebate_checks_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
  })
  
  # Rebate Checks ARP MPC
  rebate_checks_arp_mpc <- reactive({
    req(mpcs_user())
    
    mpc_vector <- as.vector(mpcs_user()$rebate_checks_arp_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
  })
  
  # Federal UI MPC
  federal_ui_mpc <- reactive({
    req(mpcs_user())
    
    mpc_vector <- as.vector(mpcs_user()$federal_ui_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
  })
  
  # State UI MPC 
  state_ui_mpc <- reactive({
    req(mpcs_user())
    
    mpc_vector <- as.vector(mpcs_user()$state_ui_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
  })
  
  # Federal Subsidies MPC 
  federal_subsidies_mpc <- reactive({
    req(mpcs_user())
    
    mpc_vector <- as.vector(mpcs_user()$federal_subsidies_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
  })
  
  # Federal Aid to Small Businesses ARP MPC 
  federal_aid_to_small_businesses_arp_mpc <- reactive({
    req(mpcs_user())
    
    mpc_vector <- as.vector(mpcs_user()$federal_aid_to_small_businesses_arp_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
  })
  
  # Federal Other Direct Aid ARP MPC
  federal_other_direct_aid_arp_mpc <- reactive({
    req(mpcs_user())
    
    mpc_vector <- as.vector(mpcs_user()$federal_other_direct_aid_arp_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
  })
  
  # Federal Other Vulnerable ARP MPC 
  federal_other_vulnerable_arp_mpc <- reactive({
    req(mpcs_user())
    
    mpc_vector <- as.vector(mpcs_user()$federal_other_vulnerable_arp_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
  })
  
  # Federal Student Loans MPC 
  federal_student_loans_mpc <- reactive({
    req(mpcs_user())
    
    mpc_vector <- as.vector(mpcs_user()$federal_student_loans_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
  })
  
  # State Subsidies MPC 
  state_subsidies_mpc <- reactive({
    req(mpcs_user())
    
    mpc_vector <- as.vector(mpcs_user()$state_subsidies_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
  })
  
  # Federal Health Outlays 
  federal_health_outlays_mpc <- reactive({
    req(mpcs_user())
    
    mpc_vector <- as.vector(mpcs_user()$federal_health_outlays_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
  })
  
  # State Health Outlays MPC 
  state_health_outlays_mpc <- reactive({
    req(mpcs_user())
    
    mpc_vector <- as.vector(mpcs_user()$state_health_outlays_mpc)
    mpc_matrix(mpc_vector = mpc_vector, dim = 259)
  })
  
  ##############
  # APPLY MPCS #
  ##############
  
  post_mpc_federal_non_corporate_taxes <- reactive({
    req(projections(), 
        federal_non_corporate_taxes_mpc())
    data <- projections()
    
    mpc(
      x = data$federal_non_corporate_taxes, 
      mpc_matrix = federal_non_corporate_taxes_mpc()
    )
    
  })
  
  post_mpc_state_non_corporate_taxes <- reactive({
    req(projections(),
        state_non_corporate_taxes_mpc())
    data <- projections()
    
    mpc(
      x = data$state_non_corporate_taxes, 
      mpc_matrix = state_non_corporate_taxes_mpc()
    )
    
  })
  
  post_mpc_federal_corporate_taxes <- reactive({
    req(projections(),
        federal_corporate_taxes_mpc())
    data <- projections()
    
    mpc(
      x = data$federal_corporate_taxes, 
      mpc_matrix = federal_corporate_taxes_mpc()
    )
    
  })
  
  post_mpc_state_corporate_taxes <- reactive({
    req(projections(),
        state_corporate_taxes_mpc())
    data <- projections()
    
    mpc(
      x = data$state_corporate_taxes, 
      mpc_matrix = state_corporate_taxes_mpc()
    )
    
  })
  
  post_mpc_federal_social_benefits <- reactive({
    req(projections(), 
        federal_social_benefits_mpc())
    data <- projections()
    
    mpc(
      x = data$federal_social_benefits, 
      mpc_matrix = federal_social_benefits_mpc()
    )
  
  })
  
  post_mpc_state_social_benefits <- reactive({
    req(projections(), 
        state_social_benefits_mpc())
    data <- projections()
    
    mpc(
      x = data$state_social_benefits, 
      mpc_matrix = state_social_benefits_mpc()
    )
    
  })
  
  post_mpc_rebate_checks <- reactive({
    req(projections(), 
        rebate_checks_mpc())
    data <- projections()
    
    mpc(
      x = data$rebate_checks, 
      mpc_matrix = rebate_checks_mpc()
    )
    
  })
  
  post_mpc_rebate_checks_arp <- reactive({
    req(projections(), 
        rebate_checks_arp_mpc())
    data <- projections()
    
    mpc(
      x = data$rebate_checks_arp, 
      mpc_matrix = rebate_checks_arp_mpc()
    )
    
  })
  
  post_mpc_federal_ui <- reactive({
    req(projections(), 
        federal_ui_mpc())
    data <- projections()
    
    mpc(
      x = data$federal_ui, 
      mpc_matrix = federal_ui_mpc()
    )
    
  })
  
  post_mpc_state_ui <- reactive({
    req(projections(),
        state_ui_mpc())
    data <- projections()
    
    mpc(
      x = data$state_ui, 
      mpc_matrix = state_ui_mpc()
    )
    
  })
  
  post_mpc_federal_subsidies <- reactive({
    req(projections(),
        federal_subsidies_mpc())
    data <- projections()
    
    mpc(
      x = data$federal_subsidies, 
      mpc_matrix = federal_subsidies_mpc()
    )
    
  })
  
  post_mpc_federal_aid_to_small_businesses_arp <- reactive({
    req(projections(),
        federal_aid_to_small_businesses_arp_mpc())
    data <- projections()
    
    mpc(
      x = data$federal_aid_to_small_businesses_arp, 
      mpc_matrix = federal_aid_to_small_businesses_arp_mpc()
    )
    
  })
  
  post_mpc_federal_other_direct_aid_arp <- reactive({
    req(projections(),
        federal_other_direct_aid_arp_mpc())
    data <- projections()
    
    mpc(
      x = data$federal_other_direct_aid_arp, 
      mpc_matrix = federal_other_direct_aid_arp_mpc()
    )
    
  })
  
  post_mpc_federal_other_vulnerable_arp <- reactive({
    req(projections(),
        federal_other_vulnerable_arp_mpc())
    data <- projections()
    
    mpc(
      x = data$federal_other_vulnerable_arp, 
      mpc_matrix = federal_other_vulnerable_arp_mpc()
    )
    
  })
  
  post_mpc_federal_student_loans <- reactive({
    req(projections(),
        federal_student_loans_mpc())
    data <- projections()
    
    mpc(
      x = data$federal_student_loans, 
      mpc_matrix = federal_student_loans_mpc()
    )
    
  })
  
  post_mpc_state_subsidies <- reactive({
    req(projections(),
        state_subsidies_mpc())
    data <- projections()
    
    mpc(
      x = data$state_subsidies, 
      mpc_matrix = state_subsidies_mpc()
    )
    
  })
  
  post_mpc_federal_health_outlays <- reactive({
    req(projections(),
        federal_health_outlays_mpc())
    data <- projections()
    
    mpc(
      x = data$federal_health_outlays, 
      mpc_matrix = federal_health_outlays_mpc()
    )
    
  })
  
  post_mpc_state_health_outlays <- reactive({
    req(projections(),
        state_health_outlays_mpc())
    data <- projections()
    
    mpc(
      x = data$state_health_outlays, 
      mpc_matrix = state_health_outlays_mpc()
    )
    
  })
  
  
  ################################
  # CREATE AUXILLIARY CATEGORIES #
  ################################  
  
  taxes <- reactive({
    req(projections(), 
        post_mpc_federal_non_corporate_taxes(), 
        post_mpc_state_non_corporate_taxes(), 
        post_mpc_federal_corporate_taxes(), 
        post_mpc_state_corporate_taxes())
    data <- projections()
    
    post_mpc_federal_non_corporate_taxes() + post_mpc_state_non_corporate_taxes() + 
      post_mpc_federal_corporate_taxes() + post_mpc_state_corporate_taxes() + 
      data$supply_side_ira
    
  })
  
  transfers <- reactive({
    req(post_mpc_federal_social_benefits(), 
        post_mpc_state_social_benefits(),
        post_mpc_rebate_checks(), 
        post_mpc_rebate_checks_arp(),
        post_mpc_federal_ui(), 
        post_mpc_state_ui(), 
        post_mpc_federal_subsidies(), 
        post_mpc_federal_aid_to_small_businesses_arp(), 
        post_mpc_federal_other_direct_aid_arp(), 
        post_mpc_federal_other_vulnerable_arp(), 
        post_mpc_federal_student_loans(), 
        post_mpc_state_subsidies(), 
        post_mpc_federal_health_outlays(), 
        post_mpc_state_health_outlays())
    
    post_mpc_federal_social_benefits() + post_mpc_state_social_benefits() +
      post_mpc_rebate_checks() + post_mpc_rebate_checks_arp() + 
      post_mpc_federal_ui() + post_mpc_state_ui() + 
      post_mpc_federal_subsidies() + post_mpc_federal_aid_to_small_businesses_arp() + 
      post_mpc_federal_other_direct_aid_arp() + post_mpc_federal_other_vulnerable_arp()  + 
      post_mpc_federal_student_loans() + post_mpc_state_subsidies() +
      post_mpc_federal_health_outlays() + post_mpc_state_health_outlays()
    
  })
    
  taxes_transfers <- reactive({
    req(taxes(), transfers())
    
    taxes() + transfers()
    
  })
  
  fim_state_purchases <- reactive({
    req(projections())
    data <- projections()
    
    data$state_purchases + 
      data$consumption_grants + 
      data$investment_grants 
  })
  
  ###############################
  # CALCULATE FIM CONTRIBUTIONS #
  ###############################  
  
  # Federal Purchases Contribution (NIPA Consistent)
  nipa_federal_purchases_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution_purchases(
      x = data$federal_purchases,
      dg = data$federal_purchases_deflator_growth,
      rpgg = data$real_potential_gdp_growth,
      gdp = data$gdp
    )
    
  })
  
  # State Purchases Contribution (NIPA Consistent)
  nipa_state_purchases_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution_purchases(
      x = data$state_purchases, 
      dg = data$state_purchases_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      gdp = data$gdp 
    )
  })
  
  # Total NIPA Purchases Contribution (NIPA Consistent)
  nipa_total_purchases_contribution <- reactive({
    req(nipa_state_purchases_contribution(), 
        nipa_federal_purchases_contribution())
    
    nipa_state_purchases_contribution() + 
      nipa_federal_purchases_contribution()
    
  })
  
  # State Purchases Contribution (FIM Consistent)
  fim_state_purchases_contribution <- reactive({
    req(projections(),
        fim_state_purchases())
    data <- projections()
    
    contribution_purchases(
      x = fim_state_purchases(),
      dg = data$state_purchases_deflator_growth,
      rpgg = data$real_potential_gdp_growth,
      gdp = data$gdp
    )
    
  })
  
  # Federal Purchases Contribution (FIM Consistent)
  fim_federal_purchases_contribution <- reactive({
    req(nipa_total_purchases_contribution(),
        fim_state_purchases_contribution())
    
    nipa_total_purchases_contribution() - fim_state_purchases_contribution()
    
  })
  
  # Consumption Contribution (include uncertainty factor)
  consumption_contribution <- reactive({
    req(projections(),
        taxes_transfers())
    data <- projections()
    
    contribution_transfers(
      x = taxes_transfers(),
      dg = data$consumption_deflator_growth, 
      rpgg = data$real_potential_gdp_growth,
      c = data$consumption,
      gdp = data$gdp 
    ) + 
      data$uncertainty
    
  })
  
  taxes_contribution <- reactive({
    req(projections(),
        taxes())
    data <- projections()
    
    contribution_transfers(
      x = taxes(),
      dg = data$consumption_deflator_growth, 
      rpgg = data$real_potential_gdp_growth,
      c = data$consumption,
      gdp = data$gdp 
    )
  })
  
  transfers_contribution <- reactive({
    req(projections(),
        transfers())
    data <- projections()
    
    contribution_transfers(
      x = transfers(),
      dg = data$consumption_deflator_growth, 
      rpgg = data$real_potential_gdp_growth,
      c = data$consumption,
      gdp = data$gdp 
    )
  })
  
  #Federal Non-Corporate Taxes 
  federal_non_corporate_taxes_contribution <- reactive({
    req(projections(),
        post_mpc_federal_non_corporate_taxes())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_federal_non_corporate_taxes(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  # State Non-Corporate Taxes Contribution
  state_non_corporate_taxes_contribution <- reactive ({
    req(projections(),
        post_mpc_state_non_corporate_taxes())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_state_non_corporate_taxes(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  # Federal Corporate Taxes Contribution
  federal_corporate_taxes_contribution <- reactive ({
    req(projections(),
        post_mpc_federal_corporate_taxes())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_federal_corporate_taxes(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  # Supply Side IRA Contribution 
  supply_side_ira_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$supply_side_ira, 
      dg = data$consumption_deflator_growth, 
      rpgg = data$real_potential_gdp_growth,
      c = data$consumption,
      gdp = data$gdp 
    )
  })
  
  # State Corporate Taxes Contribution 
  state_corporate_taxes_contribution <- reactive ({
    req(projections(),
        post_mpc_state_corporate_taxes())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_state_corporate_taxes(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  # Federal Social Benefits
  federal_social_benefits_contribution <- reactive ({
    req(projections(),
        post_mpc_federal_social_benefits())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_federal_social_benefits(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  # State Social Benefits
  state_social_benefits_contribution <- reactive ({
    req(projections(),
        post_mpc_state_social_benefits())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_state_social_benefits(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  # Rebate Checks 
  rebate_checks_contribution <- reactive ({
    req(projections(),
        post_mpc_rebate_checks())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_rebate_checks(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  # Rebate Checks ARP 
  rebate_checks_arp_contribution <- reactive ({
    req(projections(),
        post_mpc_rebate_checks_arp())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_rebate_checks_arp(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  # Federal UI
  federal_ui_contribution <- reactive ({
    req(projections(),
        post_mpc_federal_ui())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_federal_ui(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  # State UI Contribution 
  state_ui_contribution <- reactive ({
    req(projections(),
        post_mpc_state_ui())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_state_ui(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  # Federal Subsidies Contribution 
  federal_subsidies_contribution <- reactive ({
    req(projections(),
        post_mpc_federal_subsidies())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_federal_subsidies(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  # Federal Aid to Small Businesses ARP Contribution 
  federal_aid_to_small_businesses_arp_contribution <- reactive ({
    req(projections(),
        post_mpc_federal_aid_to_small_businesses_arp())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_federal_aid_to_small_businesses_arp(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  # Federal Other Direct Aid ARP Contribution
  federal_other_direct_aid_arp_contribution <- reactive ({
    req(projections(),
        post_mpc_federal_other_direct_aid_arp())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_federal_other_direct_aid_arp(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  # Federal Other Vulnerable ARP 
  federal_other_vulnerable_arp_contribution <- reactive ({
    req(projections(),
        post_mpc_federal_other_vulnerable_arp())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_federal_other_vulnerable_arp(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  # Federal Student Loans 
  federal_student_loans_contribution <- reactive ({
    req(projections(),
        post_mpc_federal_student_loans())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_federal_student_loans(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  # State Subsidies Contribution 
  state_subsidies_contribution <- reactive ({
    req(projections(),
        post_mpc_state_subsidies())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_state_subsidies(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  # Federal Health Outlays Contribution 
  federal_health_outlays_contribution <- reactive ({
    req(projections(),
        post_mpc_federal_health_outlays())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_federal_health_outlays(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  # State Health Outlays 
  state_health_outlays_contribution <- reactive ({
    req(projections(),
        post_mpc_state_health_outlays())
    data <- projections()
    
    contribution_transfers(
      x = post_mpc_state_health_outlays(),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      c = data$consumption,
      gdp = data$gdp
    )
  })
  
  #########################
  # PRODUCE FINAL RESULTS #
  #########################
  
  # Calculate Federal Contribution 
  federal_contribution <- reactive({
    req(fim_federal_purchases_contribution())
    
    fim_federal_purchases_contribution()
  }) 
  
  # Calculate State Contribution 
  state_contribution <- reactive({
    req(fim_state_purchases_contribution())
    
    fim_state_purchases_contribution() 
  })
  
  # Calculate FIM
  fim <- reactive({ 
    req(federal_contribution(), state_contribution(), consumption_contribution())
    
    federal_contribution() + state_contribution() + consumption_contribution()
    
  })
  
  # Get 4q Moving Average
  fiscal_impact_4q_ma <- reactive({
    req(fim())
    
    fim() %>%
      SMA(zoo::na.locf(., na.rm = F), n=4)
  })
  
  # Create Contributions Data Frame (interactive users are able to download this data frame as an Excel file)
  contributions <- reactive({
    req(
      projections(), 
      # Individual Contributions:
      federal_purchases_contribution(), consumption_grants_contribution(), 
      state_purchases_contribution(), 
      federal_non_corporate_taxes_contribution(), state_non_corporate_taxes_contribution(), 
      federal_corporate_taxes_contribution(), supply_side_ira_contribution(), 
      state_corporate_taxes_contribution(), federal_social_benefits_contribution(), 
      state_social_benefits_contribution(), rebate_checks_contribution(), 
      rebate_checks_arp_contribution(), federal_ui_contribution(), 
      state_ui_contribution(), federal_subsidies_contribution(), 
      federal_aid_to_small_businesses_arp_contribution(), 
      federal_other_direct_aid_arp_contribution(), 
      federal_other_vulnerable_arp_contribution(), federal_student_loans_contribution(), 
      state_subsidies_contribution(), federal_health_outlays_contribution(), 
      state_health_outlays_contribution(),
      # Aggregate Contributions:
      federal_contribution(), 
      state_contribution(), 
      taxes_contribution(), 
      transfers_contribution(), 
      consumption_contribution(), 
      # Total: 
      fim()
    )
    data.frame(
      # Individual Contributions: 
      date = as.character(projections()$date), 
      federal_purchases_contribution = federal_purchases_contribution(),
      consumption_grants_contribution = consumption_grants_contribution(), 
      state_purchases_contribution = state_purchases_contribution(), 
      federal_non_corporate_taxes_contribution = federal_non_corporate_taxes_contribution(), 
      state_non_corporate_taxes_contribution = state_non_corporate_taxes_contribution(), 
      federal_corporate_taxes_contribution = federal_corporate_taxes_contribution(), 
      supply_side_ira_contribution = supply_side_ira_contribution(), 
      state_corporate_taxes_contribution = state_corporate_taxes_contribution(), 
      federal_social_benefits_contribution = federal_social_benefits_contribution(), 
      state_social_benefits_contribution = state_social_benefits_contribution(), 
      rebate_checks_contribution = rebate_checks_contribution(), 
      rebate_checks_arp_contribution = rebate_checks_arp_contribution(), 
      federal_ui_contribution = federal_ui_contribution(), 
      state_ui_contribution = state_ui_contribution(), 
      federal_subsidies_contribution = federal_subsidies_contribution(), 
      federal_aid_to_small_businesses_arp_contribution = federal_aid_to_small_businesses_arp_contribution(), 
      federal_other_direct_aid_arp_contribution = federal_other_direct_aid_arp_contribution(), 
      federal_other_vulnerable_arp_contribution = federal_other_vulnerable_arp_contribution(), 
      federal_student_loans_contribution = federal_student_loans_contribution(), 
      state_subsidies_contribution = state_subsidies_contribution(), 
      federal_health_outlays_contribution = federal_health_outlays_contribution(), 
      state_health_outlays_contribution = state_health_outlays_contribution(),
      # Aggregate Contributions: 
      federal_contribution = federal_contribution(), 
      state_contribution = state_contribution(), 
      taxes_contribution = taxes_contribution(), 
      transfers_contribution = transfers_contribution(), 
      consumption_contribution(), 
      # Total: 
      fim()
    ) %>% 
      filter(date > "1999 Q4") %>% # keep only more recent quarters 
      filter(date <= as.character(current_quarter + 8)) %>% # eliminate quarters after the projection period 
      rename(fim = fim..) # rename the FIM variable 
  })
  
  # Get Date
  date <- reactive({
    projections()$date
  })
  
  # Create Plot Data with proper alignment
  fiscal_impact_measure_output <- reactive({
    req(date(), fim(), projections())
    
    # Create user data frame
    user_data <- data.frame(
      date = projections()$date, 
      user_fim = fim()
    )
    
    # Create hutchins data frame (assuming hutchins_fim has a date column)
    hutchins_data <- data.frame(
      date = hutchins_fim$date,  # Make sure this column exists
      hutchins_fim = hutchins_fim$fiscal_impact_measure
    )
    
    # Merge on matching dates only
    merged_data <- merge(user_data, hutchins_data, by = "date", all = FALSE)
     
    return(merged_data)
  })
  
  # Define results loaded reactive function
  # We want to display our results and the corresponding help text only if the user has uploaded data, so we define a reactive 
  # function indicating if a spreadsheet has been provided by the user. 
  
  resultsLoaded <- reactiveVal(FALSE)
  observeEvent(input$file, {
    resultsLoaded(TRUE)
  })
  # Set resultsLoaded to FALSE when the file is not uploaded
  observe({
    if (is.null(input$file)) {
      resultsLoaded(FALSE)
    }
  })
  
  # Create FIM Plot in the Main Panel 
  output$fimPlot <- renderPlotly({
    
    # Check if the user data has not been uploaded 
    if(!resultsLoaded()){
      
      # Define FIM Plot to display initially 
      data <- hutchins_fim %>% 
        filter(date > yearquarter("1999 Q4")) %>%
        filter(date < (current_quarter + 9)) %>% 
        mutate(date = as.character(date)) %>%
        mutate(fiscal_impact_4q_ma = SMA(zoo::na.locf(fiscal_impact_measure, na.rm = F), n=4))
      
      # Create the plot object
      plot1 <- plot_ly(data) %>% 
        add_trace(x = ~date, y = ~fiscal_impact_measure, type = "bar",
                  name = "Hutchins Center FIM", marker = list(color = "#e4649c"),
                  hovertemplate = 'Fiscal Impact: %{y:.2f}%<extra></extra>') %>% 
        add_trace(x = ~date, y = ~fiscal_impact_4q_ma, type = "scatter",
                  mode = 'lines+markers',
                  name = "4 Quarter Moving Average", marker = list(color = "black"), line = list(color = "black"),
                  hovertemplate = 'Four Quarter Moving Average: %{y:.2f}%<extra></extra>') %>% 
        layout(
          # X Axis 
          xaxis = list(
            title = "",
            showspikes = TRUE, 
            spikemode = "across", 
            spikecolor = "black",
            spikethickness = 1,
            spikedash = "solid", 
            tickmode = 'linear',
            tick0 = '2000 Q1',
            dtick = 4
          ),
          
          # Y Axis 
          yaxis = list(
            title = "",
            ticksuffix = "%"
          ), 
          
          # Format Hover Line 
          hovermode = "x unified",
          
          # Format Data Label 
          hoverlabel = list(
            bordercolor = 'transparent',
            font = list(size = 12)
          ),
          
          # Format Legend 
          legend = list(
            x=1,
            y=1,
            xanchor='left',
            yanchor='middle'
          )
        ) %>% 
        config(displayModeBar = FALSE)
      
      # Return the plot object
      plot1
      
    } else {
      
      # Get the data for user results
      data <- fiscal_impact_measure_output()
      
      # Check if data is NULL or empty
      if(is.null(data) || nrow(data) == 0) {
        # Return an empty plot or a message plot
        return(plot_ly() %>% 
                 layout(title = "Loading data..."))
      }
      
      # Create results plot with user defined inputs once a spreadsheet has been uploaded 
      data <- data %>% 
        filter(date < (current_quarter + 9)) %>% 
        filter(date >= yearquarter("2015 Q1")) %>% 
        mutate(date = as.character(date))
      
      # Create the plot object
      plot2 <- plot_ly(data) %>% 
        add_trace(x = ~date, y = ~user_fim, type = "bar", 
                  name = "Your FIM", marker = list(color = "#003A70"),
                  hovertemplate = 'Your FIM: %{y:.2f}%<extra></extra>') %>% 
        add_trace(x = ~date, y = ~hutchins_fim, type = "bar", 
                  name = "Hutchins FIM", marker = list(color = "#FF9E1B"),
                  hovertemplate = 'Hutchins FIM: %{y:.2f}%<extra></extra>') %>% 
        layout(
          # X Axis 
          xaxis = list(
            title = "",
            showspikes = TRUE, 
            spikemode = "across", 
            spikecolor = "black",
            spikethickness = 1,
            spikedash = "solid", 
            tickmode = 'linear',
            tick0 = '2000 Q1',
            dtick = 4
          ), 
          
          # Y Axis 
          yaxis = list(
            title = "",
            ticksuffix = "%"
          ), 
          
          # Format Hover Line 
          hovermode = "x unified",
          
          # Format Data Label 
          hoverlabel = list(
            bordercolor = 'transparent'
          )
        ) %>% 
        config(displayModeBar = FALSE)
      
      # Return the plot object
      plot2
    }
  })
  
  # Also fix the table_data reactive function:
  table_data <- reactive({
    # Only execute if results are loaded
    if(!resultsLoaded()) {
      return(NULL)
    }
    
    req(date(), fiscal_impact_measure_output(), consumption_contribution(),
        federal_contribution(), state_contribution())
    
    tryCatch({
      data.frame(
        date = date(),
        user_fim = fim(),
        federal_contribution = federal_contribution(), 
        state_contribution = state_contribution(),
        consumption_contribution = consumption_contribution()
      ) %>% 
        # FIX: Ensure current_quarter is properly formatted for comparison
        filter(date <= (current_quarter + 8)) %>% 
        filter(date >= current_quarter) %>% 
        mutate(date = as.character(date))
    }, error = function(e) {
      cat("Error in table_data:", e$message, "\n")
      return(NULL)
    })
  })
  
  # Fixed renderTable function
  output$dataTable <- renderTable({
    if(!resultsLoaded()) {
      return(NULL)
    }
    
    data <- table_data()
    if(is.null(data)) {
      return(NULL)
    }
    
    colnames(data) <- c("Date", "Your FIM", "Federal Purchases Contribution",
                        "State Purchases Contribution", 
                        "Consumption Contribution")
    data
  })
  
  # Define plot title 
  output$results_plotTitle <- renderUI({
    if (resultsLoaded() == TRUE) {
      tags$h3(style = "font-weight: bold; font-size: 24px;", "Your Fiscal Impact Measure")
    } else if (resultsLoaded() == FALSE) {
      tags$h3(style = "font-weight: bold; font-size: 24px;", "Hutchins Center Fiscal Impact Measure")
    } else {
      print("")
    }
  })
  
  # Define initial FIM plot help text
  output$chart_helpText <- renderUI({
    if(!resultsLoaded()) {
      print("The chart below displays the Hutchins Center FIM. Use the panel on the
            left to input your own data and this graph will be regenerated 
            based on your inputs. Please allow a few moments for your results to load.")
    } else (
      print("")
    )
  })
  
  # Table Title
  # Defines a table  title that displays only when the results have loaded 
  output$results_Title <- renderUI ({
    if (resultsLoaded()) {
      tags$h3(style = "font-weight: bold; font-size: 24px;", "Results Summary")
    } else {
      NULL
    }
  })
  
  # Reactive Allowing User to Download Contributions 
  output$downloadContributions <- downloadHandler(
    filename = function() {
      paste("fim_contributions_download", ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(contributions(), file)  
    }
  )
  
  # Disable the contributions download button initially
  shinyjs::disable("downloadContributions")
  
  # Observe file upload and enable the button if a file is uploaded
  observeEvent(input$file, {
    shinyjs::enable("downloadContributions")
  })
  
}
