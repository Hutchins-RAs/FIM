#' Unemployment insurance reallocation 
#' Before constructing our projections, we reallocate unemployment insurance 
#' from the Federal government to S&L governments-- except for emergency legislation
#' paid for by the Federal govt (e.g. CARES act UI expansion)
#' @param df 
#' 
#' @return
#' @export
#'
#' @examples
unemployment_insurance_reallocation <- function(df){
  df %>%
    mutate(
      gftfp = gftfp - yptu + federal_unemployment_insurance_override,
      gstfp = gstfp + yptu - federal_unemployment_insurance_override
    )
}
#' Calculate fmap share
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
fmap_share <- function(df){
  df %>%
    left_join(fmap %>% filter(year >= 1970) %>% annual_to_quarter(year) %>% select(date, fshare) %>% na.locf())
}
#' Old fmap share calculations 
#'
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
fmap_share_old <- function(df){
  df %>%
    mutate(
      fshare = fmap$fshare[match(year(date), fmap$year)] %>%
        na.locf())
}
#' Medicaid reallocation
#' Before  constructing projections, we reallocate Federal Medicaid grants from 
#' S&L governments to the Federal govt.
#' 
#' @param df 
#'
#' @return
#' @export
#'
#' @examples
medicaid_reallocation <- function(df){
  df %>%
    mutate(
      # Reattribute federal grants to states back to Federal government
      # Parse between those for consumption and investment and those for transfers (Medicaid)
      
      # federal medicaid grants to states
      yfptmd = if_else(date >='1993-03-31',
                       gfeghdx,
                       yptmd*fshare),
      
      
      # state medicaid payments = total medicaid - federal medicaid grants
      ysptmd = yptmd - yfptmd,
      gfegnet = gfeg - yfptmd, # federal grants to state and local net of medicaid grants
      
      # net state and local transfer payments = state and local transfer payments - medicaid transfers paid for by the federal government
      gstfpnet = gstfp - yfptmd, 
      # net federal transfer payments = federal transfer payments + medicaid transfers paid for by the federal government
      gftfpnet = gftfp + yfptmd 
      # we reattribute the capital grants later after calculating contributions. 
    )
}