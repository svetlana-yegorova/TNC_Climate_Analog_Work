# 6/21/21
# what is the accuracy if we take a random pixel instead of the NN? 
# climate defined as a percentage of the focal pixel's vlaue

library(purrr)

### load the fnf_rand function
source("./code/fnf_subfunction_random.R")

# junk space for testing
data<-readRDS("./outputs/sample_for_reuse_5000_focal_points.RDS")
data1<-readRDS("./outputs/XY.rds")
# # 
# # 
# # 
nn_list<-c(5, 10, 15)
# 
d= 50000
# 
 p=0.1
# i=2000
##################################################
the.cad.function_r <-function(i, d, nn_list, p) { # for a focal cell i, buffer distance d
  # and n nearest neighbours
  
  from.xy <- data[i, c('x', 'y')] # extract focal coordinates
  cmd <- data[i, 'def_h'] # extract focal cmd value
  et <- data[i, 'aet_h'] # extract focal et value
  
  
  # select a subset of data points that are within the climatic bin widths 
  # set outside of the function (arbitrarily or tested)
  #
  
  subset_c <- subset(data1, ((def_h >= cmd-cmd*p) & (def_h <= cmd+cmd*p)) &
                       ((aet_h >= et-et*p) & (aet_h <= et+et*p)))
  
  # a variation to filter out analogs based on utm values: 
  subset_d <- subset(subset_c, sqrt((from.xy$x-x)^2+(from.xy$y -y)^2)>= d)
  
  ################# new stuff ###########################
  if (nrow(subset_d) >= max(nn_list)) {
    subset_d$id<-1:nrow(subset_d)
    
    # set K to the maximum NN size
    # if (nrow(subset_d) >= max(nn_list)) { k<-max(nn_list)} else {k<-nrow(subset_d)} # set the NN number to the largest sample value
    k<-max(nn_list)
    # calculate distance matrix between the focal and the subset points for the largest # of NN considered: 
    # out.dist <- get.knnx(subset_d[,c('x', 'y')], from.xy, k=k) 
    # nn.index <- as.data.frame(out.dist$nn.index)
    
    # instead of calculating a distance matrix, draw a random set of k
    indices<-sample(subset_d$id, k)
    subset_d1<-subset_d[indices, ]
    
    # use the fnf function to iterate through the forest/non-forest calculation for all NN variations
    #iterate the fnf function with lapply statement over the nn_list: 
    # need to add a clause of what to do if the number of analogs is not sufficient
    # to satisfy the nn_list requests.
    
    # if is smaller than anything in the nn_list (but greater than 0), give me a
    # prediction from the 
    # if(k<min(nn_list)) {newlist<-k} else {newlist<-nn_list[1:findInterval(k, nn_list)]}
    
    out1<-lapply(nn_list, fnf_rand, subset_d1=subset_d1)
    
    out2<-flatten_dbl(out1)
    # report variance/agreement: 
    # if(forest.nonforest==0) {vars= (1-sum(subset[out.dist$nn.index,]$forest)/pl)} else{vars = sum(subset[out.dist$nn.index,]$forest)/pl}
    
    #vars<-sum(subset[out.dist$nn.index,]$forest)/n
    
    
    #  nn.index <- as.data.frame(out.dist1$nn.index)
    # forest.nonforest <- round(mean(subset1[out.dist1$nn.index,]$forest))	
    # output[[p]]<-c(i, forest.nonforest, vars, pl)
    
    # output1<-do.call("rbind", out1)
    # return(output1)
    # print(i)
    return(out2)
  }
  
  
  
  if (nrow(subset_d) < max(nn_list)) {
    out_99<-rep(-99, 2*length(nn_list))
    # print(i)
    return(out_99) # add the third element to signify that there is no variance calculation
  }
}

###################### OLD FUNCTION ##########################################
# i=5
# the.cad_random.function <- function(i) { # for a focal cell i
#   
#    from.xy <- data[i, c('x', 'y')] # extract coordinates
#    subset <- subset(data1, ((x <= from.xy$x-50000) | (x >= from.xy$x+50000))
#                      &((y <=from.xy$y-50000)| (y >=from.xy$y+50000)))
#   
#   if (nrow(subset) > 0) {
#     if (nrow(subset) >= 7) { n=7 } else { n=nrow(subset) }
#     # now that we have the indices, get the prediction of the 7 closest pixels
#     #indices<-out.dist1.1[1:7] # choose 7 or whatever number of closest pixels 
#     
#     # no nearest neighbor version of the function, choose 7 random records: 
#     subset$id<-1:nrow(subset)
#     indices<-sample(subset$id, n)
#     forest.nonforest<-round(mean(subset[indices,]$forest))
#     
#     return(c(i, forest.nonforest))
#   }
#   
#   if (nrow(subset) == 0) {
#     return(c(i, -99))
#   }
# }
