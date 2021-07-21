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

# take the test data used in n7 results: 
data<- slice_sample(data1, n=1000)
# data<-readRDS("./outputs/accuracy_sqrt_circa_1mm_bins_7nn.rds")
# data<-data[, c(1:5)]

# function: 
source("./code/CA_function_generalized.R")

# # mapply example: 
# # define distance buffer variable: 
# d=10000
# 
# # define number of analogs variable: 
# n=15

# define bin width combinations: 
# bin.aet=c( 0.5, 1.5, 2)
# bin.cmd=c( 0.5, 1.5, 2)
# test_bins<-crossing(bin.cmd, bin.aet)

bin.aet<-c(20, 30, 40)
bin.cmd<-c(20, 30, 40)
test_bins<-tibble(bin.aet, bin.cmd)


Sys.time()
for(i in 1:nrow(test_bins)){
  
  bin.cmd<-test_bins[i, "bin.cmd"] %>% pull(.)
  bin.aet<-test_bins[i, "bin.aet"] %>% pull(.)
  
  # run the climate analog function: 
  out<-lapply(X=1:nrow(data), FUN=the.cad.function_g, d=50000, n=7)
  
  out1<-do.call('cbind', out )
  data[[paste0("forest.pred", bin.cmd,"_", bin.aet)]] <- out1[2,]
  data[[paste0("pct_agrmt", bin.cmd,"_", bin.aet)]] <- out1[3,]
  
  print(i)
  
}

Sys.time()



# save the output file: 
saveRDS(data, "./outputs/accuracy_raw_20_40mm_7NN_50km.rds")
