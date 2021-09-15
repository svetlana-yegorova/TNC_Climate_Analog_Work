# Author: Svetlana Yegorova
# Created on  5/31/21
# Last edit date: 9/15/21

# create necessary files
dir.create("code")
dir.create("data")
dir.create("outputs")

#6/7/21
# explore the deficit and aet rasters, graph data attributes, convert rasters 
# to the same projection and clip them to the same extent, 
# write new raster files to the outputs folder: 

# source("./code/data_exploration.R")


# Create a dataframe with reference and future climate data for
# each raster cell, add forest/non-forest classification for each
# cell.

#source("./code/analogs.R")


############## Climate Function Scripts ########################
# over the course of sensitivity testing I wrote several functions for calculating 
# climate analogs to reflect the questions we were intersted in and to improve 
# computational efficiency. 

# function for calculating climate analogs (conversion of Sean's code): 
source("./code/CA_function.R")

# analog function variation that buffers 50 km (distance can be changed): 
# around the focal pixel: 
source("./code/CA_function_with_dist.R")

# analog function that takes focal cell, distance buffer and number of analogs 
# as its arguments : 
# in addition to calculaing forest/non-forest projection, this function calculates
# percent analogs that agree with the majority analog vote. 

source("./code/CA_function_generalized.R")
# 
# The most general climate analog function, takes focal cell, distance buffer and
# a list with different numbers of analogs to consider. That is for a given focal
# cell we can consider 5, 10, 20, etc. analogs.

source("./code/CA_function_generalized_nn_list.R")

# functions for calculating classification accuracy and sensitivity: 
# source("./code/accuracy_sensitivity_FUNs.R")


# Helper/sub-functions used by CA_function_generalized_nn_list.R
# for nearest neighbor climate analog function  
source("./code/fnf_subfunction.R")

# for random climate analog function (percentage bins only)
source("./code/fnf_subfunction_random.R")

############## Sensitivity Test Scripts #################################
# Which climate bin widths provide most accurate forest/non-forest classification? 

# The most general sensitivity test script: outputs a file with actual and predicted
# forest/non-forest classification
source("./code/generalized_sensitivity_test_NN_list.R")

# Sensitivity script that considers a single set of analogs (as opposed to multple
# sets in the srcript above)
source("./code/generalized_sensitivity_test.R")

# Sensitivity script for when climate bins defined as percentage of the focal 
# climate's values

source("./code/generalized_sensitivity_test_pct.R")

