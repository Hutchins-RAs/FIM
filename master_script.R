#FIM Master Script 
#Run this after you've opened the "fim" R Project in the top right corner of the RStudio window
#Set working directory
setwd("C:/Users/scampbell/Documents/GitHub/fim")
#Pull the most recent changes from GitHub
#________
#Loading the FIM package
devtools::load_all(".")
#Running the FIM calculations
source('fiscal_impact.R')
#Creating the graphs
rmarkdown::render('update-comparison.Rmd')
#To see the graphs, open the file "update-comparison.html" in the main Github/fim folder 

