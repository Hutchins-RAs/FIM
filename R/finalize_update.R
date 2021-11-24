finalize_update <- function(){
  # Update month year
  month_year <- glue('{format.Date(today() - 7, "%m")}-{year(today())}')
  
  # Create an input data folder
  if(!dir.exists(glue('results/{month_year}/input-data'))) {
    dir.create(glue('results/{month_year}/input-data'))
  }
  
  # Copy current forecast to corresponding update month input data folder
  file.copy('data/forecast.xlsx', 
            glue('results/{month_year}/input_data/forecast-{month_year}.xlsx'))
}