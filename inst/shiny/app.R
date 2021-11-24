library(shiny)

monthApp <- function(filter = filter, ...) {
  
  ui <- navbarPage(
    "Sample app",
    tabPanel(
             datasetInput("dataset", filter = filter),
             tableOutput('data')
    )
  )
  server <- function(input, output, session) {
    data <- datasetServer("dataset")
    output$data <- renderTable(head(data()))
  }
  shinyApp(ui, server, ...)
}