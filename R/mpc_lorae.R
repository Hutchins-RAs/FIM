#new mpc_vulnerable_arp func 

# The mpcs variable contains how much of the impulse is spent x quarters after
# disbursement of the funds. For example, an mpcs = c(0.3, 0.2, 0.1) means that 
# if $100 is disbursed in 2020 Q1, then $30 is spent in 2020 Q1, an additional 
# $20 is spent in 2020 Q2, and an additional $10 is spent in 2020 Q3, for a total
# of $60 spent within the first 3 quarters of the fiscal injection.

# The x variable indicates how much money was injected in each period. I'm not sure
# what period is the starting period. But let's say the first element of the vector
# represents 2020 Q1. If x = c(100, 0, 0, 0, 100), that means that $100 was 
# injected in 2020 Q1, and then nothing was injected for the next 3 quarters. Then,
# an additional $100 was injected in 2021 Q1.

# The roll_sum calculates a rolling dot product. Let's use the example above, with
# mpcs = c(0.3, 0.2, 0.1) and 
# x = c(100, 0, 0, 0, 100)
# Here's a visualization of what happens
#   NA   NA    100    0     0    0    100   NA   NA
#  [0.1  0.2   0.3]
#       [0.1  0.2   0.3]
#             [0.1  0.2   0.3]
#                   [0.1  0.2   0.3]
#                         [0.1  0.2   0.3]
#                               [0.1  0.2   0.3]
#                                     [0.1  0.2   0.3]
# 2020 Q1 fiscal impulse is the following dot product:
# <NA, NA, 100> * <0.1, 0.2, 0.3> where NAs are treated as 0s
# = $30
# 2020 Q2 fiscal impulse:
# <NA, 100, 0> * <0.1, 0.2, 0.3> = $20
# And skipping to the end... 2021 Q3 fiscal impulse is...
# <100, NA, NA> * <0.1, 0.2, 0.3> = $10
# You get the idea. Implicitly by assigning the MPCs vector a length of 3, we assume
# that the impulse on the economy goes to 0 after 3 quarters. In our code, our
# MPCs vectors are longer to represent a more gradual spending pattern of the recipients
# of the transfer.


mpc_lorae <- function (x, # A vector of cash disbursement data
                       mpc) # A vector of MPCs
  {
  1 * roll::roll_sum(x, 
                    width = length(mpc), 
                    weights = rev(mpc), 
                    online = FALSE, 
                    min_obs = 1)
}

#both of these functions are the same and can probably be combined into one function.

mps_lorae <- function(x, mps){
  roll::roll_sum(x,
                 width = length(mps),
                 weights = rev(mps),
                 online = FALSE,
                 min_obs = 1)
}
  
  