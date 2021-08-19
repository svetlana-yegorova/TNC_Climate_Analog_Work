# 8/8/21
# author: Svetlana Yegorova

# generalized sensitivity test that goes through different combinations 
# of bins (0.5, 0.75, 1, 1.25, 2, 5), buffer distances (10, 25, 50, 100 km),
# number of analogs considered (7, 25, 99). This script considers multiple number 
# of analogs within each function iteration 

# maybe do the bin by distance combination and examine the effect of 
# the number of analogs separately. 

# researching vectorization 
# researching outer() - refers to 'outer product': we can construct a 
# matrix or an n-dimentional array of the combinations of variables. 

# libraries

library(tidyverse)
library(FNN)
library(caret)
library(data.table)

# data
# raw/non-squareroot transformed data: 
 data1<-readRDS("./outputs/XY.rds")

# read in  a subset of 5000 pixels that I am going to reuse for all of the tests: 
data<-readRDS("./outputs/sample_for_reuse_5000_focal_points.RDS")

# load generalized climate analog function: 
source("./code/CA_function_generalized_nn_list.R")

# load the fnf subfunction: 
source("./code/fnf_subfunction.R")

# # random pixel function (no nearest neighbor filter)
# source("./code/random_pixel_analog.R")

# # define distance buffer variable: 
d=25000
# 
# # define number of analogs variable: 
nn_list<-c(5, 10, 20, 30, 50, 75, 100)

# define bin width or bin width combinations: 
bin.aet=c(5, 10, 20, 30, 50, 75, 100, 150)
bin.cmd=c(5, 10, 20, 30, 50, 75, 100, 150)

# later test additional values of 50, 75, 100, 150. Breaking up the tests to avoid crashes. 


# test_bins<-crossing(bin.cmd, bin.aet)

test_bins<-tibble(bin.aet, bin.cmd)


Sys.time()
for(i in 1:nrow(test_bins)){
  
  bin.cmd<-test_bins[i, "bin.cmd"] %>% pull(.)
  bin.aet<-test_bins[i, "bin.aet"] %>% pull(.)
  
  # now figure out how to handle the output
  out<-lapply(X=1:nrow(data), FUN=the.cad.function_g1, d=d, nn_list=nn_list)
  # out<-lapply(X=1:nrow(data), FUN=the.cad.function_g1, d=d, nn_list=nn_list)
  out1<-do.call('rbind', out)
  out1<-as.data.frame(out1)
  
 # add forest prediction and variance columns to the data:
  # for(t in 1:length(nn_list)) {
  for(t in 1:(ncol(out1)/2)) {
      if(t==1){
      data[[paste0("forest.pred", bin.cmd,"_N", nn_list[t])]] <- out1[, t]
      data[[paste0("pct_agrmt", bin.cmd,"_N", nn_list[t])]] <- out1[, (t+1)]
    }
    
    if(t>1){
      data[[paste0("forest.pred", bin.cmd,"_N", nn_list[t])]] <- out1[, (2*t-1)]
      data[[paste0("pct_agrmt", bin.cmd,"_N", nn_list[t])]] <- out1[, 2*t]
    }
  }
 
  
  print(i)
  # saveRDS(data, paste0("./outputs/accuracy_assessment_raw_",bin.aet, ".RDS"))
  
}

Sys.time()


# save the output file: 
saveRDS(data, paste0("./outputs/accuracy_raw_analogs_5000_sample", bin.aet[1], "_", bin.aet[length(bin.aet)], "_", d, "km.rds"))


# save the 5000 sample data for reuse! 