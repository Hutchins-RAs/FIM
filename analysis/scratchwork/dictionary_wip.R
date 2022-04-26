
projections_dictionary <-
  tibble::tribble(~new, ~old,
                  'gdp', 'gdp',
                  'real_gdp','gdph',
                  'gdp_deflator', 'jgdp',
                  'real_potential_gdp', 'gdppothq',
                  'real_potential_gdp_growth', 'gdppothq_growth',
                  'consumption', 'c',
                  'federal_purchases', 'gf',
                  'state_purchases', 'gs',
                  'federal_purchases_deflator', 'jgf',
                  'state_purchases_deflator', 'jgs',
                  'state_purchases_deflator_growth', 'jgs_growth',
                  'consumption_grants_deflator', 'jgse',
                  'investment_grants_deflator', 'jgsi',
                  'gross_consumption_grants', 'gfeeg',
                  'investment_grants', 'gfeigx',
                  'medicaid_grants', 'gfeghdx')

dictionary_list <- 
  list( "projections" = "projections_dictionary"
        
  )

projections <- readxl::read_xlsx('results/12-2020/projections-12-2020.xlsx')
dictionary <- function(data) { 
  
  dictionary <- deparse(substitute(data))
  
  eval(parse(text= dictionary_list[dictionary][[1]]))
  
}

dictionary_rename <- function(data){
  dictionary <- dictionary(data)
  # get the data variable names where there is a new name in the dictionary
  vars_found_in_dictionary <- intersect(names(data), unique(dictionary$old))
  
  # create a temporary dictionary as a named vector
  temp_dict <- dictionary %>% dplyr::filter(old %in% vars_found_in_dictionary) %>% tibble::deframe()
  
  
  # rename those variables only
  data %>% dplyr::rename(!!temp_dict)
}
dictionary_rename(projections)

dictionary <- dictionary(projections)
vars_found_in_dictionary <- intersect(names(data), unique(dictionary$old))
temp_dict <- dictionary %>% dplyr::filter(old %in% vars_found_in_dictionary) %>% tibble::deframe()


# rename those variables only
projections %>% dplyr::rename(!!temp_dict)

package_calc <-
  read_data() %>% 
  select(date,
         federal_purchases =  gf,
         federal_purchases_deflator = jgf,
         real_potential_gdp  = gdppothq,
         gdp) %>% 
  filter_index('2000 Q4' ~ '2019 Q4') %>% 
  as_tibble() %>% 
  select(-id) %>% 
  
  mutate(counterfactual = lag(federal_purchases) * (1   + q_g(federal_purchases_deflator) + q_g(real_potential_gdp))) %>% 
  mutate(contribution = 400 * (federal_purchases - counterfactual) / lag(gdp) )

projections_calc <-
  projections %>% 
  select(date,
         federal_purchases =  gf,
         federal_purchases_deflator = jgf,
         real_potential_gdp  = gdppothq,
         gdp) %>% 
  mutate(date = yearquarter(date)) %>% 
  as_tsibble(index = date) %>% 
  filter_index('2000 Q4' ~ '2019 Q4') %>% 
  as_tibble() %>% 
  
  mutate(counterfactual = lag(federal_purchases) * (1   + q_g(federal_purchases_deflator) + q_g(real_potential_gdp))) %>% 
  mutate(contribution = 400 * (federal_purchases - counterfactual) / lag(gdp) )

april_calc <-
  april %>% 
  select(date,
         federal_purchases =  federal_nom ,
         federal_purchases_deflator = federal_nom_pi,
         real_potential_gdp   = gdppoth,
         gdp) %>% 
  mutate(date = yearquarter(date)) %>% 
  drop_na() %>% 
  as_tsibble(index = date) %>% 
  filter_index('2000 Q4' ~ '2019 Q4') %>% 
  
  
  as_tibble() %>%
  
  mutate(counterfactual = lag(federal_purchases) * (1   + q_g(federal_purchases_deflator) + q_g(real_potential_gdp))) %>% 
  mutate(contribution = 400 * (federal_purchases - counterfactual) / lag(gdp) )

package_calc %>% 
  mutate(real_potential_gdp = q_g(real_potential_gdp),
         federal_purchases_deflator_growth = q_g(federal_purchases_deflator)) %>% 
  pivot_longer(-date,
               names_to  = 'package') %>% 
  left_join(april_calc %>% pivot_longer(-date,
                                        names_to = 'april'),
            by = c('date', 'package' = 'april'), suffix = c('_package', '_april')) %>% 
  mutate(diff = value_package - value_april) %>% 
  filter(diff != 0) %>% 
  arrange(desc(diff))

package_calc %>% 
  mutate(real_potential_gdp = q_g(real_potential_gdp),
         federal_purchases_deflator = q_g(federal_purchases_deflator)) %>% 
  pivot_longer(-date,
               names_to  = 'package') %>% 
  left_join(april_calc %>% pivot_longer(-date,
                                        names_to = 'april'),
            by = c('date', 'package' = 'april'), suffix = c('_package', '_april')) %>% 
  mutate(diff = value_package - value_april) %>% 
  filter(diff != 0) %>% 
  arrange(desc(diff))
