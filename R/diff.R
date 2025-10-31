# diff.R
#
# This script takes two Excel spreadsheets that have identical columns (but 
# could have differing observations), and finds the differences between them for
# the shared observations.

# The output is an Excel file with three tabs 
#    1. data set 1 
#    2. data set 2 
#    3. differences 

# Read in data sets ---------------