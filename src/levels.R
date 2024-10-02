
# Define Start and End Quarter 
start_quarter <- yearquarter("2022 Q3")
  start_position <- which(usna$date == start_quarter)
end_quarter <- yearquarter("2024 Q2")
  end_position <- which(usna$date == end_quarter) 
base_quarter <- start_quarter - 1 
  base_position <- which(usna$date == base_quarter) 

#################################
# DEFINE CONTRIBUTION FUNCTIONS #
#################################

# Define Minus Neutral Function 
minus_neutral_levels <- function(x, # the data in question
                          rpgg, # real potential gdp growth,
                          dg # consumption deflator growth
) {
  output <- x - x[base_position] * (1 + rpgg + dg)
  return(output)
}

# Define MPC Function
mpc_levels <- function(x, mpc_matrix) {
  if (nrow(mpc_matrix) != length(x)) {
    stop("The number of rows in the mpc_matrix must equal the length of the series.")
  }
  vert_x <- matrix(x, ncol = 1)
  vert_x[is.na(vert_x)] <- 0
  output <- mpc_matrix %*% vert_x
  return(output)
}

# Define Scale to GDP Function 
scale_to_gdp_levels <- function(x, gdp) {
  output =  x / gdp[base_position]
  return(output)
}

# Define Wrapper Function 
contribution_levels <- function(x, mpc_matrix = NULL, rpgg, dg, gdp) {
  # Apply MPC Function
  if (!is.null(mpc_matrix)) {
    x <- x %>%
      mpc_levels(x = ., mpc_matrix = mpc_matrix)
  }
  
  # Apply the minus_neutral function to x, setting real potential GDP growth
  result <- x %>%
    minus_neutral_levels(x = ., rpgg = rpgg, dg = dg)
  
  # Apply the scale_to_gdp function
  result %>%
    scale_to_gdp_levels(x = ., gdp = gdp)
}

#########################
# Define Level Function #
#########################


level <- function(v){
  data <- data.frame(v) %>% 
    mutate(
      x = row_number(), 
      quarters_since_start = x - base_position
    ) %>% 
    mutate(
      data_series = 100*((1+supply_side_ira_contribution_levels)^(4/quarters_since_start)-1)) %>% 
    select(
      - x,
      - quarters_since_start,
      - v
    )
  
  return(data)
}

#####################
# Get Contributions #
#####################

supply_side_ira_contribution_levels <- contribution_levels(
  x = supply_side_ira_test$data_series, # Using the new test version
  mpc_matrix = NULL,
  dg = consumption_deflator_growth_test$data_series, # Using the new test version
  rpgg = real_potential_gdp_growth_test$data_series, # Using the new test version
  gdp = gdp_test$data_series # Using the new test version
)


supply_side_ira_level <- level(supply_side_ira_contribution_levels)



