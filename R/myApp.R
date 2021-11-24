library(shiny)
library(tidyverse)

myApp <- function(filter = NULL, ...) {
  
  ui <- fluidPage(
    sidebarLayout(
    sidebarPanel(
      datasetInput("data", filter = is.data.frame),
      selectVarInput("var"),
    ),
    mainPanel(
      myplotOutput('plot'))
   )
  )
  server <- function(input, output, session) {
    data <- datasetServer("data")
    x <- selectVarServer("var", data)
    myplotServer('plot', data,x)
  }
  shinyApp(ui, server, ...)
}