# 6/29/21
# author: Svetlana Yegorova

# generalized sensitivity test that goes through different combinations 
# of bins (0.5, 0.75, 1, 1.25, 2, 5), buffer distances (10, 25, 50, 100 km)
# and number of analogs considered (7, 25, 99)

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

# create a subset of 5000 pixels that I am going to reuse for all of the tests: 
# data<- slice_sample(data1, n=5000)

# save the sample to be reused: 
# saveRDS(data, "./outputs/sample_for_reuse_5000_focal_points.RDS")
data<-readRDS("./outputs/sample_for_reuse_5000_focal_points.RDS")

# load generalized climate analog function: 
source("./code/CA_function_generalized.R")

# random pixel function (no nearest neighbor filter)
source("./code/random_pixel_analog.R")
 
# # define distance buffer variable: 
d=5000
# 
# # define number of analogs variable: 
n=100

# define bin width or bin width combinations: 
bin.aet=50
bin.cmd=50

# later test additional values of 50, 75, 100, 150. Breaking up the tests to avoid crashes. 


# test_bins<-crossing(bin.cmd, bin.aet)

test_bins<-tibble(bin.aet, bin.cmd)


Sys.time()
for(i in 1:nrow(test_bins)){
  
  bin.cmd<-test_bins[i, "bin.cmd"] %>% pull(.)
  bin.aet<-test_bins[i, "bin.aet"] %>% pull(.)
  
  # run the climate analog function: 
  out<-lapply(X=1:nrow(data), FUN=the.cad.function_g, d=d, n=n)
  
  out1<-do.call('cbind', out )
  data[[paste0("forest.pred", bin.cmd,"_", bin.aet)]] <- out1[2,]
  data[[paste0("pct_agrmt", bin.cmd,"_", bin.aet)]] <- out1[3,]
  data$n<-n
  print(i)
  # saveRDS(data, paste0("./outputs/accuracy_assessment_raw_",bin.aet, "_NN", n, ".RDS"))
  
}

Sys.time()


# save the output file: 
saveRDS(data, paste0("./outputs/accuracy_raw_analogs_5000_sample", bin.aet[1], "_", bin.aet[length(bin.aet)], "_", n, "NN", d/1000, "km.rds"))


# save the 5000 sample data for reuse! 