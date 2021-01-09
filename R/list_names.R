#' Components 
#' Names of the components we project.
#'
#' @return
#' @export
#'
#' @examples
get_components_names <- function(){
  components <-
    c(
      # GDP
      ## Actual
      'gdp', 'gdph', 'jgdp',
      ## Potential
      'gdppotq', 'gdppothq',
      # Gov't Consumption Expenditures & Gross Investment 
      ## Total 
      'g', 'gf', 'gs', 
      ## Deflators
      ### Total
      'jgf', 'jgs',
      ### S&L Consumption/Investment Expenditures
      'jgse', 'jgsi',
      # GRANTS
      ## Total
      'gfeg', 
      ### Health & Hospitals
      'gfeghhx',
      ### Medicaid
      'gfeghdx', 
      ### Investment
      'gfeigx', 
      # TAXES
      ## Personal
      'gfrpt', 'gsrpt',
      ## Social Insurance
      'gfrs' ,'gsrs', 
      ## Corporate
      'gfrcp', 'gsrcp',
      ## Production & Imports
      'gfrpri', 'gsrpri',
      # SOCIAL BENEFITS
      ## Total
      'gftfp', 'gstfp',
      ## Medicaid
      'yptmd',
      ## Medicare
      'yptmr',
      # SUBSIDIES 
      'gssub', 'gfsub', 
      # PERSONAL CONSUMPTION
      'c', 'jc'
    )
  return(components)
}