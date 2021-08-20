# 7/26/21 Percentage bin sensitivity test: defines defines bin width as a 
# percentage of the focal aet and deficit:  
# 8/16/21 Altered code to project forest/nonforest for several # of analogs: 
# analogs listed in nn_list

# author: Svetlana Yegorova

# libraries

library(tidyverse)
library(FNN)
library(caret)
library(data.table)
library(purrr)

# load data
# raw/non-square root transformed data: 
data1<-readRDS("./outputs/XY.rds")

# take the 5000 focal dataset used for testing: : 
data<-readRDS("./outputs/sample_for_reuse_5000_focal_points.RDS")

# #  percentage function: 
# source("./code/generalized_sensitivity_test_pct.R")

## load percentage function that takes a list of the number of analogs to consider: 
source("./code/random_pixel_analog.R")

# percentage function that picks a random analog, instead of nearest analog
# source("./code/random_pixel_analog.R")

# load fnf_random subfunction (used inside the.cad.function_r): 
source("./code/fnf_subfunction_random.R")

# # define distance buffer variable: 
d=50000

# 
# # define the list of numbers of analogs to consider. 
nn_list<-c(20, 30, 50, 75, 100)


# define percentages to test: 
pct<-c(0.1, 0.2, 0.3, 0.4)



Sys.time()
for(i in 1:length(pct)){
  
  p<-pct[i] 
  
  # run the climate analog function: 
  out<-lapply(X=c(1:nrow(data)), FUN=the.cad.function_r, d=d, nn_list=nn_list, p=p)
  
  out1<-do.call('rbind', out)
  out1<-as.data.frame(out1)
  
  # add forest prediction and variance columns to the data:
  # for(t in 1:length(nn_list)) {
  for(t in 1:(ncol(out1)/2)) {
    if(t==1){
      data[[paste0("forest.pred", p, "pct", "_N", nn_list[t])]] <- out1[, t]
      data[[paste0("pct_agrmt", p, "pct","_N", nn_list[t])]] <- out1[, (t+1)]
    }
    
    else {
      data[[paste0("forest.pred", p, "pct","_N", nn_list[t])]] <- out1[, (2*t-1)]
      data[[paste0("pct_agrmt", p, "pct", "_N", nn_list[t])]] <- out1[, 2*t]
    }
  }
  

  print(i)
  saveRDS(data, paste0("./outputs/accuracy_random_pct_", p, "_NN_list_N50km.rds"))
  
}

Sys.time()


# save the output file: 
saveRDS(data, paste0("./outputs/accuracy_random_pct_", pct[1], "_", pct[5], "_NNlist_NN50km.rds"))
