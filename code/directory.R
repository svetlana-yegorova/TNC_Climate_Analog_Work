# 5/31/21

# create necessary files
dir.create("code")
dir.create("data")
dir.create("outputs")

#6/7/21
# explore the deficit and aet rasters, graph data attributes, convert rasters 
# to the same projection and extent, writes new raster files to the outputs 
# folder: 

# source("./code/data_exploration.R")


# Create a dataframe with reference and future climate data for
# each raster cell, add forest/non-forest classification for each
# cell.

#source("./code/analogs.R")


############## Functions  ########################
# function for calculating climate analogs (conversion of Sean's code): 
# source("./code/CA_function.R")

# analog function variation that buffers 50 km (distance can be changed): 
# around the focal pixel: 
# source("./code/CA_function_with_dist.R")

# analog function that takes focal cell, distance buffer and number of analogs 
# as its arguments (the most generalized version of function so far): 
# in addition to calculaing forest/non-forest projection, this function calculates
# percent analogs that agree with the majority analog vote. 

source("./code/CA_function_generalized.R")

# functions for calculating classification accuracy and sensitivity: 
#source("./code/accuracy_sensitivity_FUNs.R")

############## Sensitivity Tests for 
# test out nine combinations of climate variable
# thresholds for 1000 sample pixels:

# source("./code/bin_width_loop_test.R")

# variation of the climate analog code that chooses random
# seven analogs instead of the nearest neighbor (testing the effect
# of distance)

#source("./code/bin_width_loop_test_no_NN.R")


