# Load the rmarkdown package
library(rmarkdown)

# Set the input and output file paths
input_file <- "fim_guide.Rmd"
output_file <- "fim_guide.html"

# Render the R Markdown document and save the output to the specified path
rmarkdown::render(input_file, output_file = output_file)

