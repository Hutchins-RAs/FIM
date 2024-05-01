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
cpiu <- projections$cpiu # carried over into projections
cpiu_g <- qagr(cpiu) # used later to calculate cola_rate

# Date
date <- projections$date # carried over into projections

# COLA rate
# If the current quarter is quarter 1, take the CPI-U from the most recent
# quarter 3 and assign it to the current observation.
cola_rate <- if_else(lubridate::quarter(date) == 1, # used later to calculate other vars
                    lag(cpiu_g, 2),
                    NA) %>%
  # Fill forward NAs with most recent value. Preserves initial NAs in vector
  na.locf(cola_rate, fromLast = FALSE, na.rm = FALSE)

# GFTFP
# Adjusts government final transfer payments (gftfp) for cost-of-living 
# adjustments (COLA) and health/unemployment insurance smoothing.
# 1. 'gftfp_unadj' stores the original gftfp values before adjustments.
gftfp_unadj <- projections$gftfp # used later to calculate other vars
# 2. 'health_ui' computes a 4-quarter simple moving average (SMA) of 
# health and unemployment insurance payments to smooth out 
# fluctuations.
yptmd <- projections$yptmd # used later to calculate other vars
yptmr <- projections$yptmr # used later to calculate other vars
yptu <- projections$yptu # used later to calculate other vars
health_ui <- TTR::SMA(yptmd + yptmr + yptu, n = 4) # used later to calculate other vars
# 3. 'smooth_gftfp_minus_health_ui' calculates a smoothed version of 
# gftfp excluding health_ui, adjusted by the COLA rate to reflect the 
# change in purchasing power.
smooth_gftfp_minus_health_ui <- TTR::SMA((gftfp_unadj - health_ui) * (1 - cola_rate), n = 4) # used later to calculate other vars
# 4. Finally, 'gftfp' is recalculated by adding the smoothed, COLA-
# adjusted gftfp (excluding health_ui) back to the smoothed health_ui, 
# providing an overall adjusted gftfp that accounts for both cost-of-
# living adjustments and smoothed health/unemployment insurance 
# payments. This ensures gftfp reflects both the impact of inflation 
# adjustments and more stable health and unemployment insurance figures
# across quarters.
gftfp <- smooth_gftfp_minus_health_ui * (1 + cola_rate) + health_ui # used later to calculate other vars

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
gfrpt <- zoo::rollapply(projections$gfrpt, width = 4, mean, fill = NA, min_obs = 1, align = 'right') # used later to calculate other vars
gfrpri <- zoo::rollapply(projections$gfrpri, width = 4, mean, fill = NA, min_obs = 1, align = 'right') # used later to calculate other vars
gfrcp <- zoo::rollapply(projections$gfrcp, width = 4, mean, fill = NA, min_obs = 1, align = 'right') # used later to calculate other vars
gfrs <- zoo::rollapply(projections$gfrs, width = 4, mean, fill = NA, min_obs = 1, align = 'right') # used later to calculate other vars
# health outlays
yptmd <- zoo::rollapply(yptmd, width = 4, mean, fill = NA, min_obs = 1, align = 'right') # used later to calculate other vars
yptmr <- zoo::rollapply(projections$yptmr, width = 4, mean, fill = NA, min_obs = 1, align = 'right') # used later to calculate other vars
# unemployment insurance
yptu <- zoo::rollapply(projections$yptu, width = 4, mean, fill = NA, min_obs = 1, align = 'right') # used later to calculate other vars

# Implicit price deflators
jgf <- projections$gf/projections$gfh # used in projections
jgs <- projections$gs/projections$gsh # used in projections
jc <- projections$c/projections$ch # used in projections

# adds 36 new columns
gftfp_growth <- qgr(gftfp) - 1 # used in projections
gfrpt_growth <- qgr(gfrpt) - 1 # used to calculate gfrptCurrentLaw_growth
gfrpri_growth <- qgr(gfrpri) - 1 # used in projections
gfrcp_growth <- qgr(gfrcp) - 1 # used in projections
gfrs_growth <- qgr(gfrs) - 1 # used in projections
yptmr_growth <- qgr(yptmr) - 1 # used in projections
yptmd_growth <- qgr(yptmd) - 1 # used in projections
yptu_growth <- qgr(yptu) - 1 # used in projections
cpiu_growth <- qgr(cpiu) - 1 # I think we already do something similar to this and we call it cpiu_g
# used in projections
cpiu_g_growth <- qgr(cpiu_g) - 1 # This probably doesn't make sense
# used in projections
cola_rate_growth <- qgr(cola_rate) - 1 # This probably doesn't make sense
# used in projections
gftfp_unadj_growth <- qgr(gftfp_unadj) - 1 # I don't think we use this
# used in projections
health_ui_growth <- qgr(health_ui) - 1 # I don't think we use this
# used in projections
smooth_gftfp_minus_health_ui_growth <- qgr(smooth_gftfp_minus_health_ui) - 1 # I don't think we use this
# used in projections
jgf_growth <- qgr(jgf) - 1 # used in projections
jgs_growth <- qgr(jgs) - 1 # used in projections
jc_growth <- qgr(jc) - 1 # used in projections


fy_growth <- qgr(projections$fy) - 1 # This, for example, is nonsense and should be removed
# used in projections
state_ui_growth <- qgr(projections$state_ui) - 1 # used in projections
federal_ui_timing_growth <- qgr(projections$federal_ui_timing) - 1 # This probably doesn't make sense
# used in projections
federal_ui_growth <- qgr(projections$federal_ui) - 1 # used in projections
gdp_growth <- qgr(projections$gdp) - 1 # used in projections
gdph_growth <- qgr(projections$gdph) - 1 # used in projections
gdppothq_growth <- qgr(projections$gdppothq) - 1 # used in projections
gdppotq_growth <- qgr(projections$gdppotq) - 1 # used in projections
dc_growth <- qgr(projections$dc) - 1 # used in projections
jgdp_growth <- qgr(projections$jgdp) - 1 # used in projections
c_growth <- qgr(projections$c) - 1 # used in projections
ch_growth <- qgr(projections$ch) - 1 # used in projections
gh_growth <- qgr(projections$gh) - 1 # used in projections
gfh_growth <- qgr(projections$gfh) - 1 # used in projections
gsh_growth <- qgr(projections$gsh) - 1 # used in projections
g_growth <- qgr(projections$g) - 1 # used in projections
gf_growth <- qgr(projections$gf) - 1 # used in projections
gs_growth <- qgr(projections$gs) - 1 # used in projections
unemployment_rate_growth <- qgr(projections$unemployment_rate) - 1 # used in projections

# Construct alternative scenario for personal current taxes, under which the
# TCJA provisions for income taxes don't expire in 2025
# TODO: I think this code has a mistake. Currently, code replaces gfrpt_growth
# with the lag of itself starting in 2025 Q3. This doesn't make much sense- 
# I think original authors wanted to keep the same growth rate continuing into
# perpetuity after 2025 Q3. Figure out what we intend to do here.
# TODO: figure out if this growth rate is even used later.
#
# keep the current law version (where TCJA measures sunset in 2025 Q3)
gfrptCurrentLaw <- gfrpt # used later to calculate other vars
# keep the current law growth
gfrptCurrentLaw_growth <- gfrpt_growth # used in projections
# redefine the growth to be the lag of itself? (I believe this is a mistake)
gfrpt_growth <- if_else(date > yearquarter('2025 Q3'), lag(gfrpt_growth), gfrpt_growth, missing = NULL) # used in projections
# Note: I believe these new rows are not even used later. Determine if this is true
# or not

# Generate a few other columns we'll need for projections_beta
id <- projections$id
gdp <- projections$gdp
gdph <- projections$gdph
gdppothq <- projections$gdppothq
gdppotq <- projections$gdppotq
jgdp <- projections$jgdp
dc <- projections$dc
c <- projections$c
ch <- projections$ch
federal_ui <- projections$federal_ui
state_ui <- projections$state_ui
unemployment_rate <- projections$unemployment_rate

# Create the 'projections' data frame that matches the previous version from the
# FIM
projections_beta <- tibble(id, date, gdp, gdph, gdppothq, gdppotq, jgdp, jgf, jgs, 
                           jc, jgdp_growth, jgf_growth, jgs_growth, jc_growth, dc,
                           c, ch, 
                           fy_growth, # this one is silly and should be removed
                           gftfp_growth, gfrpt_growth, gfrpri_growth, gfrcp_growth,
                           gfrs_growth, yptmr_growth, yptmd_growth, yptu_growth,
                           state_ui_growth, 
                           federal_ui_timing_growth, # this one is silly and should be removed
                           federal_ui_growth,
                           gdp_growth, gdph_growth, gdppothq_growth, gdppotq_growth,
                           dc_growth, c_growth, ch_growth, gh_growth, gfh_growth,
                           gsh_growth, g_growth, gf_growth, gs_growth,
                           cpiu_growth, # probably not needed
                           unemployment_rate_growth,
                           cpiu_g_growth, # certainly nonsensical
                           cola_rate_growth, # almost certainly not needed
                           gftfp_unadj_growth, # probably not needed
                           health_ui_growth, # perhaps not needed
                           smooth_gftfp_minus_health_ui_growth, # almost certainly not needed
                           gfrptCurrentLaw_growth, # probably not needed
                           cpiu, federal_ui, state_ui, unemployment_rate
                )

# Convert projections_beta to a tsibble
projections_beta <- as_tsibble(projections_beta, index = date)
# Assuming 'id' should be the key and 'date' the index
projections_beta <- as_tsibble(projections_beta, index = date, key = id)

# rename the variables in projections_beta
projections_beta_renamed <- projections_beta %>%
  transmute(date,
            id,
            gdp,
            real_gdp = gdph,
            real_potential_gdp = gdppothq,
            potential_gdp = gdppotq,
            gdp_deflator = jgdp,
            federal_purchases_deflator = jgf,
            state_purchases_deflator = jgs,
            consumption_deflator = jc,
            gdp_deflator_growth = jgdp_growth,
            federal_purchases_deflator_growth = jgf_growth,
            state_purchases_deflator_growth = jgs_growth,
            consumption_deflator_growth = jc_growth,
            # idk what to do about dc. I think it gets deleted when this 
            # define_variables function is run on usna1
            dc, # should delete
            consumption = c,
            real_consumption = ch,
            fy_growth, # should delete
            federal_social_benefits_growth = gftfp_growth,
            federal_personal_taxes_growth =  gfrpt_growth,
            federal_production_taxes_growth = gfrpri_growth,
            federal_corporate_taxes_growth = gfrcp_growth,
            federal_payroll_taxes_growth = gfrs_growth,
            medicare_growth = yptmr_growth,
            medicaid_growth = yptmd_growth,
            ui_growth = yptu_growth,
            state_ui_growth, # for some reason this one doesn't have its haver name
            federal_ui_timing_growth, # should delete
            federal_ui_growth,
            gdp_growth,
            real_gdp_growth = gdph_growth,
            real_potential_gdp_growth = gdppothq_growth,
            potential_gdp_growth = gdppotq_growth,
            # idk what to do about dc. I think it gets deleted when this 
            # define_variables function is run on usna1
            dc_growth, # should delete
            consumption_growth = c_growth,
            real_consumption_growth = ch_growth,
            real_purchases_growth =  gh_growth,
            real_federal_purchases_growth = gfh_growth,
            real_state_purchases_growth = gsh_growth,
            purchases_growth = g_growth,
            federal_purchases_growth = gf_growth,
            state_purchases_growth = gs_growth,
            cpiu_growth, # CPI-U
            unemployment_rate_growth,
            cpiu_g_growth, # shouild be deleted
            cola_rate_growth, # should be deleted
            gftfp_unadj_growth, # keeping this name bc I think we will drop this var
            health_ui_growth,
            smooth_gftfp_minus_health_ui_growth,
            gfrptCurrentLaw_growth, # keeping this name bc I think we will drop this var
            cpiu,
            federal_ui,
            state_ui,
            unemployment_rate
  )
            
            