# Define server logic 
server <- function(input, output) {
  
  # Download handler for the Excel file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("fim_data_download", ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(data, file)  # Assuming 'data' is predefined
    }
  )
  
  # Reactive expression to read the uploaded file
  uploaded_data <- reactive({
    req(input$file)  # Ensure the file is uploaded
    read_xlsx(input$file$datapath)
  })
  
  # Reactive expression to clean the uploaded data
  forecast_user <- reactive({
    req(uploaded_data())
    
    uploaded_data() %>%
      select(-name) %>%  # Remove the 'name' column from the data
      pivot_longer(-variable, names_to = 'date') %>%  # Reshape the data
      pivot_wider(names_from = 'variable', values_from = 'value') %>%
      mutate(date = yearquarter(date)) %>%  # Convert date to year-quarter format
      tsibble::as_tsibble(index = date)
  })
  
  # random stuff
  type <- reactive({
    req(forecast_user())
    
    print(as.character(forecast_user()$date))
  })
  
  # Create projections dataset 
  projections <- reactive({
    req(forecast_user())
    
    # Joining datasets and performing calculations
    forecast_user() %>%
      coalesce_join(usna, by = 'date') %>%  # Ensure 'usna' is defined
      mutate(across(where(is.numeric), ~ coalesce(.x, 0))) %>%  # Coalesce NA's to 0 for numeric values
      
      # Define FIM variables
      mutate(
        federal_health_outlays = medicare + medicaid_grants,
        state_health_outlays = medicaid - medicaid_grants
      ) %>%
      
      # Ensure 'historical_overrides' is defined
      mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter, 
                   federal_other_direct_aid_arp = historical_overrides$federal_other_direct_aid_arp_override,
                   federal_other_vulnerable_arp = historical_overrides$federal_other_vulnerable_arp_override,
                   federal_social_benefits = historical_overrides$federal_social_benefits_override,
                   federal_aid_to_small_businesses_arp = historical_overrides$federal_aid_to_small_businesses_arp_override) %>%
      
      # Additional transformations
      mutate_where(date == current_quarter & is.na(federal_corporate_taxes) & is.na(state_corporate_taxes),
                   federal_corporate_taxes = tail(historical_overrides$federal_corporate_taxes_override, n = 1),
                   state_corporate_taxes = tail(historical_overrides$state_corporate_taxes_override, n = 1)) %>%
      
      # More transformations
      mutate_where(date == yearquarter("2021 Q1"), federal_social_benefits = federal_social_benefits + 203) %>%
      mutate_where(date == yearquarter('2021 Q4'), federal_ui = 11, state_ui = ui - federal_ui) %>%
      mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter, supply_side_ira = historical_overrides$supply_side_ira_override) %>%
      mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter, federal_student_loans = historical_overrides$federal_student_loans_override)
  })
  
  
  #####################################
  # CALCULATE THE FIM USING USER DATA #
  #####################################
  
  # Federal Purchases Contribution
  federal_purchases_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_purchases,
      mpc_matrix = NULL, 
      dg = data$federal_purchases_deflator_growth,
      rpgg = data$real_potential_gdp_growth,
      gdp = data$gdp
    )
    
  })
  
  # Consumption Grants Contribution 
  consumption_grants_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$consumption_grants,
      mpc_matrix = NULL, 
      dg = data$consumption_grants_deflator_growth, 
      rpgg = data$real_potential_gdp_growth,
      gdp = data$gdp 
    )
  })
  
  # Investment Grants Contribution 
  investment_grants_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$investment_grants, 
      mpc_matrix = NULL, 
      dg = data$investment_grants_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      gdp = data$gdp
    )
  })
  
  # State Purchases Contribution 
  state_purchases_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$state_purchases, 
      mpc_matrix = NULL, 
      dg = data$state_purchases_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      gdp = data$gdp 
    )
  })
  
  #Federal Non-Corporate Taxes 
  federal_non_corporate_taxes_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_non_corporate_taxes,
      mpc_matrix = readRDS("cache/mpc_matrices/federal_non_corporate_taxes.rds"), 
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      gdp = data$gdp
    )
  })
  
  # State Non-Corporate Taxes Contribution
  state_non_corporate_taxes_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$state_non_corporate_taxes,
      mpc_matrix =  readRDS("cache/mpc_matrices/state_non_corporate_taxes.rds"),
      dg = data$consumption_deflator_growth,
      rpgg = data$real_potential_gdp_growth, 
      gdp = data$gdp
    )
  })
  
  # Federal Corporate Taxes Contribution
  federal_corporate_taxes_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_corporate_taxes,
      mpc_matrix = readRDS("cache/mpc_matrices/federal_corporate_taxes.rds"),
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # Supply Side IRA Contribution 
  supply_side_ira_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$supply_side_ira, 
      mpc_matrix = NULL, 
      dg = data$consumption_deflator_growth, 
      rpgg = data$real_potential_gdp_growth, 
      gdp = data$gdp 
    )
  })
  
  # State Corporate Taxes Contribution 
  state_corporate_taxes_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$state_corporate_taxes,
      mpc_matrix = readRDS("cache/mpc_matrices/state_corporate_taxes.rds"),
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp 
    )
  })
  
  # Federal Social Benefits
  federal_social_benefits_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_social_benefits,
      mpc_matrix = readRDS("cache/mpc_matrices/federal_social_benefits.rds"),
      rpgg = data$real_potential_gdp_growth, 
      dg = data$consumption_deflator_growth, 
      gdp = data$gdp) 
  })
  
  # State social benefits 
  state_social_benefits_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$state_social_benefits, 
      mpc_matrix = readRDS("cache/mpc_matrices/state_social_benefits.rds"),
      rpgg = data$real_potential_gdp_growth, 
      dg = data$consumption_deflator_growth, 
      gdp = data$gdp
    )
  }) 
  
  # Rebate Checks 
  rebate_checks_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$rebate_checks,
      mpc_matrix = readRDS("cache/mpc_matrices/rebate_checks.rds"),
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # Rebate Checks ARP 
  rebate_checks_arp_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$rebate_checks_arp,
      mpc_matrix = readRDS("cache/mpc_matrices/rebate_checks_arp.rds"),
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # Federal UI
  federal_ui_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_ui,
      mpc_matrix = readRDS("cache/mpc_matrices/federal_ui.rds"),
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # State UI Contribution 
  state_ui_contribution <- reactive ({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$state_ui,
      mpc_matrix = readRDS("cache/mpc_matrices/state_ui.rds"), 
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # Federal Subsidies Contribution 
  federal_subsidies_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_subsidies, 
      mpc_matrix = readRDS("cache/mpc_matrices/federal_subsidies.rds"), 
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
    
  })
  
  # Federal Aid to Small Businesses ARP Contribution 
  federal_aid_to_small_businesses_arp_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_aid_to_small_businesses_arp,
      mpc_matrix = readRDS("cache/mpc_matrices/federal_aid_to_small_businesses_arp.rds"),
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # Federal Other Direct Aid ARP Contribution
  federal_other_direct_aid_arp_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_other_direct_aid_arp, 
      mpc_matrix = readRDS("cache/mpc_matrices/federal_other_direct_aid_arp.rds"), 
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
    
  })
  
  # Federal Other Vulnerable ARP 
  federal_other_vulnerable_arp_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_other_vulnerable_arp,
      mpc_matrix = readRDS("cache/mpc_matrices/federal_other_vulnerable_arp.rds"), 
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # Federal Student Loans 
  federal_student_loans_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_student_loans,
      mpc_matrix =  readRDS("cache/mpc_matrices/federal_student_loans.rds"), 
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # State Subsidies Contribution 
  state_subsidies_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$state_subsidies,
      mpc_matrix =  readRDS("cache/mpc_matrices/state_subsidies.rds"), 
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # Federal Health Outlays Contribution 
  federal_health_outlays_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$federal_health_outlays,
      mpc_matrix =  readRDS("cache/mpc_matrices/federal_health_outlays.rds"), 
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
    
  })
  
  # State Health Outlays 
  state_health_outlays_contribution <- reactive({
    req(projections())
    data <- projections()
    
    contribution(
      x = data$state_health_outlays, 
      mpc_matrix = readRDS("cache/mpc_matrices/state_health_outlays.rds"), 
      rpgg = data$real_potential_gdp_growth,
      dg = data$consumption_deflator_growth,
      gdp = data$gdp
    )
  })
  
  # Calculate Federal Contribution 
  federal_contribution <- reactive({
    req(federal_purchases_contribution(), 
        consumption_grants_contribution(), 
        investment_grants_contribution())
    
    sum <- federal_purchases_contribution() + consumption_grants_contribution() + investment_grants_contribution()
  }) 
  
  # Calculate State Contribution 
  state_contribution <- reactive({
    req(state_purchases_contribution(), 
        consumption_grants_contribution(), 
        investment_grants_contribution())
    
    sum <- state_purchases_contribution() - consumption_grants_contribution() - investment_grants_contribution()
  })
  
  # Calculate Taxes Contribution
  taxes_contribution <- reactive({
    req(
      federal_non_corporate_taxes_contribution(), 
      state_non_corporate_taxes_contribution(), 
      federal_corporate_taxes_contribution(), 
      supply_side_ira_contribution(), 
      state_corporate_taxes_contribution)
    
    sum <- federal_non_corporate_taxes_contribution() + 
      state_non_corporate_taxes_contribution() +
      federal_corporate_taxes_contribution() + 
      supply_side_ira_contribution() + 
      state_corporate_taxes_contribution()
  })
  
  # Calculate Transfers Contribution 
  transfers_contribution <- reactive({
    req(federal_social_benefits_contribution(), state_social_benefits_contribution(), rebate_checks_contribution(), 
        rebate_checks_arp_contribution(), federal_ui_contribution(), state_ui_contribution(), federal_subsidies_contribution(), 
        federal_aid_to_small_businesses_arp_contribution(), federal_other_vulnerable_arp_contribution(), federal_student_loans_contribution(), 
        state_subsidies_contribution(), federal_health_outlays_contribution(), state_health_outlays_contribution()) 
    
    sum <-   federal_social_benefits_contribution() + 
      state_social_benefits_contribution() + 
      rebate_checks_contribution() + 
      rebate_checks_arp_contribution() + 
      federal_ui_contribution() + 
      state_ui_contribution() + 
      federal_subsidies_contribution() + 
      federal_aid_to_small_businesses_arp_contribution() + 
      federal_other_direct_aid_arp_contribution() + 
      federal_other_vulnerable_arp_contribution() + 
      federal_student_loans_contribution() + 
      state_subsidies_contribution() + 
      federal_health_outlays_contribution() + 
      state_health_outlays_contribution()
  })
  
  # Calculate fim 
  fim <- reactive({ 
    req(transfers_contribution(), taxes_contribution(), federal_contribution(), state_contribution())
    
    sum <- transfers_contribution() + taxes_contribution() + federal_contribution() + state_contribution()
    
  })
  
  # Create Data Frame 
  contributions <- reactive({
    req(
      federal_purchases_contribution(), consumption_grants_contribution(), 
      investment_grants_contribution(), state_purchases_contribution(), 
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
      fim()
    )
    
    data.frame(
      federal_purchases_contribution = federal_purchases_contribution(),
      consumption_grants_contribution = consumption_grants_contribution(), 
      investment_grants_contribution = investment_grants_contribution(), 
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
      fim()
    )
  })
  # Get Date
  date <- reactive({
    as.character(projections()$date)
  })
  
  # Create FIM-Date Data frame
  fiscal_impact_measure <- reactive({
    req(fim(), date())
    
    data <- data.frame(
      date(), 
      fim()
    ) 
    
    start <- which(data$date == "1999 Q4")
    end <- which(data$date == "2026 Q2")
    data %>% slice(start:end)
  })
  
  # Create Plot
  output$barPlot <- renderPlot({
    req(fiscal_impact_measure())
    
    ggplot(fiscal_impact_measure(), aes(x = fiscal_impact_measure()$fim, y = fiscal_impact_measure()$date)) +
      geom_bar(stat= "identity", fill = "violetred1") +
      labs(title = "Your Hutchins Center FIM", x = NULL, y = NULL) + 
      scale_x_continuous(labels = function(x) paste0(x, "%")) +
      scale_y_discrete(breaks = unique(fiscal_impact_measure()$date)[seq(1,length(unique(fiscal_impact_measure()$date)),by=8)]) +
      coord_flip() + 
      theme(text = element_text(family = "Roboto"),
            plot.title = element_text(size = 24), 
            axis.text.y = element_text(color = "black", size = 16),
            axis.text.x = element_text(color ="black", size = 16), 
            panel.background = element_rect(fill = "white"))
    
  }) 
  
  
  output$dataHead <- renderTable({
    print((fiscal_impact_measure()), digits = 6)  # Display the first few rows of the cleaned data
  })
}
