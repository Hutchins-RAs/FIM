myplotOutput <- function(id) {
  tagList(
    plotOutput(NS(id, "plot"))
  )
}



myplotServer <- function(id, data, x) {
  stopifnot(is.reactive(data))
  stopifnot(is.reactive(x))
  moduleServer(id, function(input, output, session) {
    
    output$plot <- renderPlot({
      ggplot(data(), aes(.data[['date']], .data[[input$x]])) +
        geom_point()
    }, res = 96)
    
  })
}