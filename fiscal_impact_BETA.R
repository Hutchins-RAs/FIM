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
# 4. Finally, 'gftfp' is recalculated by adding the smoothed, COLA-
# adjusted gftfp (excluding health_ui) back to the smoothed health_ui, 
# providing an overall adjusted gftfp that accounts for both cost-of-
# living adjustments and smoothed health/unemployment insurance 
# payments. This ensures gftfp reflects both the impact of inflation 
# adjustments and more stable health and unemployment insurance figures
# across quarters.
gftfp <- smooth_gftfp_minus_health_ui * (1 + cola_rate) + health_ui

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

# adds 36 new columns
gftfp_growth <- qgr(gftfp) - 1
gfrpt_growth <- qgr(gfrpt) - 1
gfrpri_growth <- qgr(gfrpri) - 1
gfrcp_growth <- qgr(gfrcp) - 1
gfrs_growth <- qgr(gfrs) - 1
yptmr_growth <- qgr(yptmr) - 1
yptmd_growth <- qgr(yptmd) - 1
yptu_growth <- qgr(yptu) - 1
cpiu_growth <- qgr(cpiu) - 1 # I think we already do something similar to this and we call it cpiu_g
cpiu_g_growth <- qgr(cpiu_g) - 1 # This probably doesn't make sense
cola_rate_growth <- qgr(cola_rate) - 1 # This probably doesn't make sense
gftfp_unadj_growth <- qgr(gftfp_unadj) - 1 # I don't think we use this
health_ui_growth <- qgr(health_ui) - 1 # I don't think we use this
smooth_gftfp_minus_health_ui_growth <- qgr(smooth_gftfp_minus_health_ui) - 1 # I don't think we use this
jgf_growth <- qgr(jgf) - 1
jgs_growth <- qgr(jgs) - 1
jc_growth <- qgr(jc) - 1

fy_growth <- qgr(projections$fy) - 1 # This, for example, is nonsense and should be removed
state_ui_growth <- qgr(projections$state_ui) - 1
federal_ui_timing_growth <- qgr(projections$federal_ui_timing) - 1 # This probably doesn't make sense
federal_ui_growth <- qgr(projections$federal_ui) - 1
gdp_growth <- qgr(projections$gdp) - 1
gdph_growth <- qgr(projections$gdph) - 1
gdppothq_growth <- qgr(projections$gdppothq) - 1
gdppotq_growth <- qgr(projections$gdppotq) - 1
dc_growth <- qgr(projections$dc) - 1
jgdp_growth <- qgr(projections$jgdp) - 1
c_growth <- qgr(projections$c) - 1
ch_growth <- qgr(projections$ch) - 1
gh_growth <- qgr(projections$gh) - 1
gfh_growth <- qgr(projections$gfh) - 1
gsh_growth <- qgr(projections$gsh) - 1
g_growth <- qgr(projections$g) - 1
gf_growth <- qgr(projections$gf) - 1
gs_growth <- qgr(projections$gs) - 1
unemployment_rate_growth <- qgr(projections$unemployment_rate) - 1




