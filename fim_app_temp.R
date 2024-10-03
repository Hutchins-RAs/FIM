library(shiny)
library(readxl)
library(writexl)
library(lubridate)
library(zoo)

data <- readxl::read_xlsx('shiny/data_temp.xlsx')
load('shiny/cache/usna.rda')
load('shiny/cache/historical_overrides.rda')
current_quarter <- yearquarter(Sys.Date()) %>% yearquarter()
current_quarter <- current_quarter - 2
source("src/contributions.R")

# Define Server Logic 
server <- function(input, output) {
  
  # Download handler for the Excel file
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("fim_data_download", ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(data, file)  
    }
  )
  
  # Reactive expression to read the uploaded file
  uploaded_data <- reactive({
    req(input$file)  
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
  
  # Create projections dataset
  projections <- reactive({
    req(forecast_user())
    ui_forecast <- data.frame(forecast_user())
    
    # Join the NIPAs (contained in the cache folder) with the user in
    coalesce_join(usna, ui_forecast, by = 'date') %>%  # Ensure 'usna' is defined
      mutate(across(where(is.numeric), ~ coalesce(.x, 0)))%>% # Coalesce NA's to 0 for numeric values
      
      # Replace missing values with 0 
      mutate( # Coalesce NA's to 0 for all numeric values 
        across(where(is.numeric),
               ~ coalesce(.x, 0))) %>%
      
      #Define FIM health variables 
      mutate(
        federal_health_outlays = medicare + medicaid_grants,
        state_health_outlays = medicaid - medicaid_grants
      ) %>% 
      
      # apply historical_overrides for ARP 
      mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
                   federal_other_direct_aid_arp = historical_overrides$federal_other_direct_aid_arp_override,
                   federal_other_vulnerable_arp = historical_overrides$federal_other_vulnerable_arp_override,
                   federal_social_benefits = historical_overrides$federal_social_benefits_override,
                   federal_aid_to_small_businesses_arp = historical_overrides$federal_aid_to_small_businesses_arp_override) %>% 
      mutate_where(date == current_quarter & is.na(federal_corporate_taxes) & is.na(state_corporate_taxes),
                   federal_corporate_taxes = tail(historical_overrides$federal_corporate_taxes_override, n = 1),
                   state_corporate_taxes = tail(historical_overrides$state_corporate_taxes_override, n = 1)) %>% 
      mutate_where(date == yearquarter("2021 Q1"),
                   federal_social_benefits = federal_social_benefits + 203) %>% 
      mutate_where(date == yearquarter('2021 Q4'),
                   federal_ui = 11, 
                   state_ui = ui - federal_ui) %>%
      #apply historical_overrides for Supply Side IRA
      mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
                   supply_side_ira = historical_overrides$supply_side_ira_override) %>%
      #apply historical_overrides for Federal Student Loans
      mutate_where(date >= yearquarter('2020 Q2') & date <= current_quarter,
                   federal_student_loans = historical_overrides$federal_student_loans_override)
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
  
  # Create Table 
  output$dataTable <- renderTable({
    req(federal_social_benefits_contribution())
  })
  
}

# Define UI 
ui <- fluidPage(
  
  # App title
  titlePanel("Interactive"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      helpText("Click here to download the data"),
      
      # Button to download the data
      downloadButton("downloadData", "Download"),
      
      # Display text directing users to re-upload their forecasts 
      helpText("Re-upload the Excel file with your own forecasts."),
      
      # Generate file input for uploading excel data
      fileInput("file", label = NULL, accept = c(".xlsx")),  # Ensure .xlsx is specified
      
      # Set width
      width = 4
    ),
    
    # Main panel with spinner and plot output
    mainPanel(
      tableOutput("dataTable")
    )
  )
)


# Run the application
shinyApp(ui = ui, server = server)