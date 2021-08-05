#7/6/21

# version of the climate analog function that calculates accuracy for different 
# numbers of analogs considered. 

# libraries

library(tidyverse)
library(FNN)
library(caret)
library(data.table)
#### junk space to define vars when testing

# load fnf function: 
source("./code/fnf_subfunction.R")


d=50000


bin.aet=5
bin.cmd=5

nn_list<-c(5, 25, 50)

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
  subset <- subset(subset_c, sqrt((from.xy$x-x)^2+(from.xy$y -y)^2)>= d)
  
  if (nrow(subset) > 0) {
      # set K to the maximum NN size
      if (nrow(subset) >= max(nn_list)) { k=max(nn_list) } else { k=nrow(subset) } # set the NN number to the largest sample value
      
      # calculate distance matrix between the focal and the subset points for the largest # of NN considered: 
      out.dist <- get.knnx(subset[,c('x', 'y')], from.xy, k=k) 
      nn.index <- as.data.frame(out.dist$nn.index)
      
      # use the fnf function to iterate through the forest/non-forest calculation for all NN variations
      
      # function to iterate: 
      # fnf <- function(kn){
      #   indices<-out.dist$nn.index[1:kn]
      #   # round(mean(subset[out.dist$nn.index[1:kn],]$forest))
      #   forest.nonforest<-round(mean(subset[indices,]$forest))
      #   if(forest.nonforest==0) {vars= (1-sum(subset[indices,]$forest)/kn)} else{vars = sum(subset[indices,]$forest)/kn}
      #   
      #   
      #   return(c(forest.nonforest, vars, kn))
      # }
      
      #iterate the fnf function with lapply statement over the nn_list: 
      
      out1<-lapply(nn_list, fnf)
      
      # report variance/agreement: 
      # if(forest.nonforest==0) {vars= (1-sum(subset[out.dist$nn.index,]$forest)/pl)} else{vars = sum(subset[out.dist$nn.index,]$forest)/pl}
      
      #vars<-sum(subset[out.dist$nn.index,]$forest)/n
      
      
      #  nn.index <- as.data.frame(out.dist1$nn.index)
      # forest.nonforest <- round(mean(subset1[out.dist1$nn.index,]$forest))	
      # output[[p]]<-c(i, forest.nonforest, vars, pl)
      
    # output1<-do.call("rbind", out1)
    # return(output1)
    return(out1)
  }
  
  if (nrow(subset) == 0) {
    return(c(i, -99))
  }
}

### at the end combine (c) results 
# Sys.time()
# the.cad.function_g1(10, 50000, n)
# Sys.time()
# don't know if the time cost is workth the convenience