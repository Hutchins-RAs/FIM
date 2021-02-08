impute_inflation <- function(x, rate) {
  output <- x
  y <- rep(NA, length = length(x)) #Creating an empty vector to fill in with the loop. This makes R faster to run for vectors with a large number of elements.
  
  for (i in seq_len(length(output))) {
    if (i == 1) {
      y[i] <- output[i] #To avoid an error attempting to use the 0th element.
    } else {
      y[i] <- output[i - 1]
    }
    
    if (is.na(output[i])) {
      output[i] <- y[i] * (1 + rate)
    } else {
      output[i]
    }
  }
  output
}



project_state_taxes <- function(df){
  state_taxes <- c('gsrpt', 'gsrpri', 'gsrcp', 'gsrs')
  
  df %>%
    mutate(across(all_of(state_taxes),
                  ~ zoo::na.locf(. / gdp) * gdp))
}





