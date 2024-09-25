ui <- fluidPage(
  
  # App title
  titlePanel("Hutchins Center FIM-teractive"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      helpText("Click the button below to download our forecasts for each of the FIM components. You can edit this spreadsheet and replace our forecasts with your own. It is important that you edit only the values. Do not adjust the formatting or variable names."),
      
      # Button to download the data
      downloadButton("downloadData", "Download"),
      
      helpText("Re-upload the Excel file with your own forecasts."),
      
      # File input for uploading Excel file
      fileInput("file", label = NULL, accept = c(".xlsx")),  # Ensure .xlsx is specified
      # Set width
      width = 3),  # This comma was removed
    
    # Main panel to display the plot
    mainPanel(
      plotOutput("barPlot", width = 1100, height = 800)
    )
  )
)