mpc_social_benefits = function(x){
  0.9 * rollapply(x, width = 4, mean, fill = NA, align =  'right')
}