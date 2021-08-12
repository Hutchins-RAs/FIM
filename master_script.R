#FIM Master Script 
#Set working directory
setwd("C:/Users/scampbell/Documents/GitHub/fim")
#Loading the FIM package
devtools::load_all(".")
#Running the FIM calculations
source('fiscal_impact.R')
#Creating the graphs
rmarkdown::render('update-comparison.Rmd')


