library(shiny)

# Define the UI
ui <- fluidPage(
  numericInput("var1", "Variable 1", value = 10),
  numericInput("var2", "Variable 2", value = 20),
  numericInput("var3", "Variable 3", value = 30),
  verbatimTextOutput("result1"),
  verbatimTextOutput("result2"),
  verbatimTextOutput("result3")
)

# Define the server logic
server <- function(input, output, session) {
  # Reactive values to hold the observed variables
  observed_vars <- reactiveValues(var1 = NULL, var2 = NULL, var3 = NULL)
  
  # Custom functions to operate on each variable
  custom_function1 <- function(x) { x * 2 }
  custom_function2 <- function(x) { x + 10 }
  custom_function3 <- function(x) { x ^ 2 }
  
  # Observer for var1
  observeEvent(input$var1, {
    observed_vars$var1 <- custom_function1(input$var1)
  })
  
  # Observer for var2
  observeEvent(input$var2, {
    observed_vars$var2 <- custom_function2(input$var2)
  })
  
  # Observer for var3
  observeEvent(input$var3, {
    observed_vars$var3 <- custom_function3(input$var3)
  })
  
  # Output the results
  output$result1 <- renderPrint({
    observed_vars$var1
  })
  
  output$result2 <- renderPrint({
    observed_vars$var2
  })
  
  output$result3 <- renderPrint({
    observed_vars$var3
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

