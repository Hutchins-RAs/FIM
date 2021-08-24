#FIM Master Script 
#Run this after you've opened the "fim" R Project in the top right corner of the RStudio window

#Loading the FIM package
librarian::shelf('gert', 'tidyverse', 'glue')

# Check whether any changes have been made
git_fetch()

# Pull those changes onto your computer
git_pull()

devtools::load_all(".")

#Running the FIM calculations
source('fiscal_impact.R')
