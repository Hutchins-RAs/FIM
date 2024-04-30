# Section A: prep for new update -----------------------------------------------

# Miscellaneous: set up for Eli because has a different library directory
#Get system information
sys_info <- Sys.info()

# Check if the username is 'easdourian'
if (sys_info['user'] == 'easdourian') {
  .libPaths("C:/Users/easdourian/Documents/library")
  # Other code to run if the user is 'easdourian'
} else {
  print("The code is not being run by 'easdourian'")
}

Sys.setenv(TZ = 'UTC') # Set the default time zone to UTC (Coordinated Universal Time)

librarian::shelf(tidyverse, tsibble, lubridate, glue, TimTeaFan/dplyover, zoo, TTR, fs, gt, openxlsx, 
                 snakecase, rlang, BrookingsInstitution/ggbrookings) # Load packages
devtools::load_all() # Load all functions in package

options(digits = 4) # Limit number of digits
options(scipen = 20)# Turn off scientific notation under 20 digits 

#are we running this after a cbo baseline and pre-bea update?
post_cbo_baseline<- FALSE

if(post_cbo_baseline == TRUE){
  month_year <- glue('{format.Date(today() - 7, "%m")}-{year(today())}-post-cbo')
}else{
  # Set the value of 'month_year' to the current month and year (in the format "mm-yyyy")
  month_year <- glue('{format.Date(today() - 7, "%m")}-{year(today())}')
}

# If the month of the previous month is less than 10, set the value of 'last_month_year' to the previous month and year (in the format "0m-yyyy")
# Otherwise, set the value of 'last_month_year' to the previous month and year (in the format "mm-yyyy")
if((month(today() - 7 
          -months(1)) < 10)){
  last_month_year <- glue('0{month(today() - 7 -months(1))}-{year(today() - dmonths(1) - dweeks(1))}')
} else{
  last_month_year <- glue('{month(today() - 7 -months(1))}-{year(today() - dmonths(1) - dweeks(1))}')
  
}

#setting our reference period to be the post-cbo files if we've already produced fim output incorporating the cbo update
if(file.exists(glue('results/{month_year}-post-cbo'))){
  last_month_year<- glue('{month_year}-post-cbo')
}

# Create updatglibe folders

update_in_progress <- TRUE #set this to false if you're not running the code for a new month

if(update_in_progress == TRUE){
  dir_create(glue('results/{month_year}')) # Create folder for current update in the results directory
  dir_create(glue('results/{month_year}/input_data')) # Folder to store forecast sheet from current update
  
  # Copy the file 'forecast.xlsx' from the 'data' directory to the 'input_data' directory
  # This is the copy we keep for the current update
  file_copy(path = 'data/forecast.xlsx', new_path = glue('results/{month_year}/input_data/forecast_{month_year}.xlsx'), overwrite = TRUE)
}

# Section B: load in data ------------------------------------------------------
# Load in projections
# I still need to figure out why this data series starts in 2016!!! WHY???
fim::projections # this is the literal df
load("data/projections.rda") # this loads in a df named projections
all.equal(projections, fim::projections)

# CPI-U
cpiu <- projections$cpiu # cpiu
cpiu_g <- qagr(cpiu) # cpiu_g

# Date
date <- projections$date

# COLA rate
# If the current quarter is quarter 1, take the CPI-U from the most recent
# quarter 3 and assign it to the current observation.
cola_rate <- if_else(lubridate::quarter(date) == 1,
                    lag(cpiu_g, 2),
                    NA) %>%
  # Fill forward NAs with most recent value. Preserves initial NAs in vector
  na.locf(cola_rate, fromLast = FALSE, na.rm = FALSE)

# GFTFP
# Adjusts government final transfer payments (gftfp) for cost-of-living 
# adjustments (COLA) and health/unemployment insurance smoothing.
# 1. 'gftfp_unadj' stores the original gftfp values before adjustments.
gftfp_unadj <- projections$gftfp
# 2. 'health_ui' computes a 4-quarter simple moving average (SMA) of 
# health and unemployment insurance payments to smooth out 
# fluctuations.
yptmd <- projections$yptmd
yptmr <- projections$yptmr
yptu <- projections$yptu
health_ui <- TTR::SMA(yptmd + yptmr + yptu, n = 4)
# 3. 'smooth_gftfp_minus_health_ui' calculates a smoothed version of 
# gftfp excluding health_ui, adjusted by the COLA rate to reflect the 
# change in purchasing power.
smooth_gftfp_minus_health_ui <- TTR::SMA((gftfp_unadj - health_ui) * (1 - cola_rate), n = 4)

# Smooth budget series
# Applies a rolling mean over a 4-quarter window to smooth federal taxes, 
# health outlays, and unemployment insurance data. For each selected column, 
# the rolling mean is calculated using the current and previous three 
# quarters' data, aligning the window to the current quarter. If less than 
# four observations are available (at the data series start), the mean of 
# available observations is used instead, ensuring no initial data is left 
# without a smoothed value.
# TODO: This code OVERWRITES existing rows with smoothed data. For later refactoring,
# I may want to create a new row rather than overwrite an existing one.
#
# federal taxes
gfrpt <- zoo::rollapply(projections$gfrpt, width = 4, mean, fill = NA, min_obs = 1, align = 'right')
gfrpri <- zoo::rollapply(projections$gfrpri, width = 4, mean, fill = NA, min_obs = 1, align = 'right')
gfrcp <- zoo::rollapply(projections$gfrcp, width = 4, mean, fill = NA, min_obs = 1, align = 'right')
gfrs <- zoo::rollapply(projections$gfrs, width = 4, mean, fill = NA, min_obs = 1, align = 'right')
# health outlays
yptmd <- zoo::rollapply(projections$yptmd, width = 4, mean, fill = NA, min_obs = 1, align = 'right')
yptmr <- zoo::rollapply(projections$yptmr, width = 4, mean, fill = NA, min_obs = 1, align = 'right')
# unemployment insurance
yptu <- zoo::rollapply(projections$yptu, width = 4, mean, fill = NA, min_obs = 1, align = 'right')

# Implicit price deflators
jgf <- projections$gf/projections$gfh
jgs <- projections$gs/projections$gsh
jc <- projections$c/projections$ch


