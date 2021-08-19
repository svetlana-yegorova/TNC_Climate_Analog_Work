#7/6/21 - original start
# 8/4/21 - updated the funciton and it works. still a bit slow 

# version of the climate analog function that calculates accuracy for different 
# numbers of analogs considered. 

# libraries

library(tidyverse)
library(FNN)
library(caret)
library(data.table)
# #### junk space to define vars when testing
# 
# # data: use the subsample both as the sources of focal and analog pixels to save RAM
# 
# data<-readRDS("./outputs/sample_for_reuse_5000_focal_points.RDS")
# data1<-data

# load fnf function:
# source("./code/fnf_subfunction.R")
# 
# 
# d=50000
# 
# 
# bin.aet=50
# bin.cmd=50
# 
# nn_list<-c(5, 25, 50)

#################


the.cad.function_g1 <-function(i, d, nn_list) { # for a focal cell i, buffer distance d
  # and n, a list of nearest neighbours to consider.
  
  from.xy <- data[i, c('x', 'y')] # extract focal coordinates
  cmd <- data[i, 'def_h'] # extract focal cmd value
  et <- data[i, 'aet_h'] # extract focal et value
  
  
  # select a subset of data points that are within the climatic bin widths 
  # set outside of the function (arbitrarily or tested)
  #
  
  subset_c <- subset(data1, ((def_h >= cmd-bin.cmd) & (def_h <= cmd+bin.cmd)) &
                       ((aet_h >= et-bin.aet) & (aet_h <= et+bin.aet)))
  
  # a variation to filter out analogs based on utm values: 
  subset_d <- subset(subset_c, sqrt((from.xy$x-x)^2+(from.xy$y -y)^2)>= d)
  
  if (nrow(subset_d) > 0) {
      # set K to the maximum NN size
      if (nrow(subset_d) >= max(nn_list)) { k=max(nn_list)} else {k=nrow(subset_d)} # set the NN number to the largest sample value
      
      # calculate distance matrix between the focal and the subset points for the largest # of NN considered: 
      out.dist <- get.knnx(subset_d[,c('x', 'y')], from.xy, k=k) 
      nn.index <- as.data.frame(out.dist$nn.index)
      
      # use the fnf function to iterate through the forest/non-forest calculation for all NN variations
      #iterate the fnf function with lapply statement over the nn_list: 
      # need to add a clause of what to do if the number of analogs is not sufficient
      # to satisfy the nn_list requests.
      
      # if is smaller than anything in the nn_list (but greater than 0), give me a
      # prediction from the 
      if(k<min(nn_list)) {newlist<-k} else{newlist<-nn_list[1:findInterval(k, nn_list)]}

      out1<-lapply(newlist, fnf, out.dist=out.dist, subset_d=subset_d)
      
      
      ##### Experimental ###########3
      out2<-flatten_dbl(out1)
      # report variance/agreement: 
      # if(forest.nonforest==0) {vars= (1-sum(subset[out.dist$nn.index,]$forest)/pl)} else{vars = sum(subset[out.dist$nn.index,]$forest)/pl}
      
      #vars<-sum(subset[out.dist$nn.index,]$forest)/n
      
      
      #  nn.index <- as.data.frame(out.dist1$nn.index)
      # forest.nonforest <- round(mean(subset1[out.dist1$nn.index,]$forest))	
      # output[[p]]<-c(i, forest.nonforest, vars, pl)
      
    # output1<-do.call("rbind", out1)
    # return(output1)
    return(out2)
  }
  
  if (nrow(subset) == 0) {
    return(c(i, -99))
  }
}

### at the end combine (c) results 
# # a<-Sys.time()
# test0<-the.cad.function_g1(12, 50000, nn_list)
#  # b<-Sys.time()
# # a-b
# 
# test0<-lapply(X=c(1:10), FUN=the.cad.function_g1, d=2500, nn_list)
# head(test0)

