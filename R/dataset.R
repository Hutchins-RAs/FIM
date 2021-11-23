datasetInput <- function(id, filter = NULL) {
  names <- ls("package:fim")
  if (!is.null(filter)) {
    data <- lapply(names, get, "package:fim")
    names <- names[vapply(data, filter, logical(1))]
  }
  
  selectInput(NS(id, "dataset"), "Pick a dataset", choices = names)
}

datasetServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(get(input$dataset, "package:fim"))
  })
}