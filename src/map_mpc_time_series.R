# map_mpc_time_series.R
# 
# IMPORTANT NOTE: For this script to run, n_periods must be predefined elsewhere.
# TODO: Make this n_periods less brittle, perhaps by converting this script
# into a function that can be run with n_periods as an argument.
#
# The purpose of this script is to generate an mpc_series list - which maps 
# periods of a FIM data series with an mpc - and mpc_list - which lists all the 
# mpcs used in the FIM project.
#
# Using these lists, the script then generates mpc vectors which are saved in
# cache/mpc_matrices/name_of_series.rds, where the `name_of_series.rds` is
# replaced with a series name, like federal_ui, state_ui, subsidies, etc.
#
# This code may eventually be supplanted with an Excel sheet for an improved 
# user experience.
# TODO: explore mpc_series and mpc_list presentation options in the R Shiny app.
#
# This code requires loading functions from mpc_lorae.R, currently located in
# src/mpc_lorae.R
source("src/mpc_lorae.R")
# 
# TODO: make the two period variables below less brittle
n_periods <- n_periods #Total number of periods in the data
period_21q2 <- 206 # The index number of the 2021 Q2 period
mpc_series <- list(
  #ui = rep("mpc00", times = n_periods),
  federal_ui = c(rep("mpc01", times = (period_21q2 - 1)), # pre-21Q2 mpc regime
                 rep("mpc02", times = (n_periods- period_21q2 + 1))), # post-21Q2 regime
  state_ui = c(rep("mpc01", times = (period_21q2 - 1)), # pre-21Q2 mpc regime
               rep("mpc02", times = (n_periods- period_21q2 + 1))), # post-21Q2 regime
  subsidies = rep("mpc03", times = n_periods),
  federal_subsidies = rep("mpc03", times = n_periods),
  state_subsidies = rep("mpc03", times = n_periods),
  health_outlays = rep("mpc04", times = n_periods),
  federal_health_outlays = rep("mpc04", times = n_periods), 
  state_health_outlays = rep("mpc04", times = n_periods), 
  social_benefits = rep("mpc04", times = n_periods), 
  federal_social_benefits = rep("mpc04", times = n_periods), 
  state_social_benefits = rep("mpc04", times = n_periods),
  corporate_taxes = rep("mpc05", times = n_periods), 
  federal_corporate_taxes = rep("mpc05", times = n_periods), 
  state_corporate_taxes = rep("mpc05", times = n_periods),
  non_corporate_taxes = rep("mpc06", times = n_periods), 
  federal_non_corporate_taxes = rep("mpc06", times = n_periods), 
  state_non_corporate_taxes = rep("mpc06", times = n_periods),
  rebate_checks_arp = rep("mpc07", times = n_periods), 
  federal_other_direct_aid_arp = rep("mpc07", times = n_periods),
  federal_other_vulnerable_arp = rep("mpc02", times = n_periods),
  federal_aid_to_small_businesses_arp = rep("mpc08", times = n_periods),
  federal_student_loans = rep("mpc02", times = n_periods),
  supply_side_ira = rep("mpc_direct", times = n_periods),
  rebate_checks = rep("mpc09", times = n_periods)
)

mpc_list <- list(
  mpc00 = c(), # Non-mpc
  mpc01 = 0.9 * c(0.35, 0.35, 0.1, 0.1, 0.05, 0.05),
  mpc02 = c(0.2, 0.17, 0.16, 0.15, 0.09, 0.05, 0.05, 0.04),
  mpc03 = 0.45 * c(0.11, 0.095, 0.09, 0.085, 0.075, 0.075, 0.075, 0.075, 0.06, 0.06, 0.06, 0.06, 0.02, 0.02, 0.02, 0.02),
  mpc04 = c(0.225, 0.225, 0.225, 0.225),
  mpc05 = rep(-0.0333333333333333, 12),
  mpc06 = c(-0.12, -0.12, -0.06, -0.06, -0.06, -0.06, -0.06, -0.06),
  mpc07 = c(0.14, 0.1, 0.1, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05, 0.03, 0.03, 0.03, 0.025, 0.02, 0.015, 0.01, 0.005),
  mpc08 = c(0.04, 0.04, 0.017, 0.017, 0.017, 0.017, 0.017, 0.017, 0.017, 0.017, 0.017, 0.017),
  mpc09 = 0.7 * c(0.35, 0.15, 0.08, 0.08, 0.08, 0.08, 0.08, 0.08),
  # This is a direct MPC: meaning we think all is spent in period it's disbursed
  mpc_direct = c(1),
  # This is a direct MPC for taxes: Analogous to the above, but negative, since 
  # we assign taxes a negative mpc.
  mpc_direct_tax = c(-1)
)

# Overwrite cache with current data
# note: comp_mpc_matrix function comes from mpc_lorae.R, sourced above.
for (name in names(mpc_series)) {
  print(name)
  
  # Calculate the mpc matrix
  mpc_matrix <- comp_mpc_matrix(mpc_vector_list = mpc_list,
                                mpc_series = mpc_series[[name]])
  
  # Construct the file path for saving the matrix
  file_path <- paste0("cache/mpc_matrices/", name, ".rds")
  saveRDS(mpc_matrix, file = file_path)
}
