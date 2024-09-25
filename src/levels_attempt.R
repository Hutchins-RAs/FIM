
# Create a data set containing the necessary variables: supply side IRA data, consumption deflator growth, GDP, and real potential GDP growth
data_gs <- cbind(
  supply_side_ira_test, 
  consumption_deflator_growth_test, 
  gdp_test, 
  real_potential_gdp_growth_test
)

# Make the matrix a data frame 
data_gs_1 <- data.frame(data_gs)



# Clean dataset and keep only the necessary variables 
data_gs2 <- data_gs_1 %>% 
  mutate(
    supply_side_ira = data_series,
    real_potential_gdp_growth = data_series.3,
    consumption_deflator_growth_test = data_series.1,
    gdp_test = data_series.2,
    gdp_2022q3 = gdp_test[211], # generate a column that is always equal to GDP in 2022 Q3 
    supply_side_ira_2022q3 = supply_side_ira[211]
  ) %>% 
  select(
    -date.1,
    -date.2,
    -date.3,
    -data_series.1,
    -data_series.2,
    -data_series.3,
    -data_series
  )

# Fix GDP and data 
supply_side_ira_2022q2 <- data_gs2 %>% filter(date == yearquarter("2022 Q2")) %>% pull(supply_side_ira)
gdp_2022q2 <- data_gs2 %>% filter(date == yearquarter("2022 Q2")) %>% pull(gdp_test)

data_gs2 <- data_gs2 %>%
  filter(date > yearquarter("2022 Q3"))

# Get the cumulative product of the growth rates 
data_gs2 <- data_gs2 %>% 
  mutate(cumulative_product_real_potential_gdp_growth = cumprod(1 + real_potential_gdp_growth) -1, 
         cumulative_product_consumption_deflator_growth = cumprod(1 + consumption_deflator_growth_test) -1)

#######################
# Define FIM Functions#
#######################


# Minus neutral
minus_neutral <- function(x, # the data in question
                          rpgg, # real potential gdp growth,
                          dg # consumption deflator growth
) {
  output <- x - supply_side_ira_2022q3 * (1 + rpgg + dg) # here we subbed lag(x) for the value of the data in 2022 q3 
  return(output)
}

# MPC
mpc <- function(x, mpc_matrix) {
  # Input check that the dimensions of the matrix equal the length of the series
  if (nrow(mpc_matrix) != length(x)) {
    stop("The number of rows in the mpc_matrix must equal the length of the series.")
  }
  vert_x <- matrix(x, ncol = 1)
  vert_x[is.na(vert_x)] <- 0
  output <- mpc_matrix %*% vert_x
  return(output)
}

# Scale to GDP
scale_to_gdp <- function(x, gdp) {
  output = 400 * x / gdp_2022q2
  return(output)
}

# Wrapper Function
contribution <- function(x, mpc_matrix = NULL, rpgg, dg, gdp) {
  if (!is.null(mpc_matrix)) {
    x <- x %>%
      mpc(x = ., mpc_matrix = mpc_matrix)
  }
  result <- x %>%
    minus_neutral(x = ., rpgg = rpgg, dg = dg)
  result %>%
    scale_to_gdp(x = ., gdp = gdp)
}


# Calculate supply side IRA contribution 
supply_side_ira_contribution_over_time <- contribution(
  x = data_gs2$supply_side_ira,
  mpc_matrix = NULL,
  dg = data_gs2$cumulative_product_consumption_deflator_growth,
  rpgg = data_gs2$cumulative_product_real_potential_gdp_growth, 
  gdp = data_gs2$gdp_2022q2 
)


data_gs2 <- data_gs2 %>% 
  mutate(supply_side_ira_contribution = supply_side_ira_contribution_over_time)

view(data_gs2)

openxlsx::write.xlsx(data_gs, file = glue('temp.xlsx'), overwrite = TRUE)