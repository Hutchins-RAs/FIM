# 0.0 Source ----------------------------------------------------------------------------------------------------------
## Source custom  functions and packages
##NOTE: RUN THIS ONLY IF HAVER DLX IS NOT WORKING/DOES NOT HAVE THE LATEST DATA. IF HAVER IS WORKING, RUN HAVER_PULL.R AS USUAL.
Sys.setenv(TZ = 'UTC')
librarian::shelf(Haver, dplyr, tidyr, readxl, writexl, tsibble, purrr, openxlsx)

haver.path("//ESDATA01/DLX/DATA/")
devtools::load_all()

# 0.1 Pull Raw Data---------------------------------------------------------------

START <- "01-01-1970"

# Quarterly -------------------------------------------------------------------------------------------------------

# BEA NIPAs 
#reading in names to pull from Haver DLX
names_usna <- read_excel("data/haver_names.xlsx")

