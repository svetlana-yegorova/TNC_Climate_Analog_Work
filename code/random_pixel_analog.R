# 6/21/21
# what is the accuracy if we take a random pixel instead of the NN? 
# climate defined as a percentage of the focal pixel's vlaue

library(purrr)

### load the fnf_rand function
source("./code/fnf_subfunction_random.R")

# ###### junk space for testing
# data<-readRDS("./outputs/sample_for_reuse_5000_focal_points.RDS")
# data1<-readRDS("./outputs/XY.rds")
# # # 
# # # 
# # # 
# nn_list<-c(5, 10, 15)
# # 
# d= 50000
# # 
#  p=0.1
# i=2000
##################################################
the.cad.function_r <-function(i, d, nn_list, p) { # for a focal cell i, buffer distance d
  # and n nearest neighbours
  
  from.xy <- data[i, c('x', 'y')] # extract focal coordinates
  cmd <- data[i, 'def_h'] # extract focal cmd value
  et <- data[i, 'aet_h'] # extract focal et value
  
  
  # select a subset of data points that are within the climatic bin widths 
  subset_c <- subset(data1, ((def_h >= cmd-cmd*p) & (def_h <= cmd+cmd*p)) &
                       ((aet_h >= et-et*p) & (aet_h <= et+et*p)))
  
  # select pixels that are located beyod distance d (need this line to prevent 
  # the algorithm from picking adjacent pixels) 
  subset_d <- subset(subset_c, sqrt((from.xy$x-x)^2+(from.xy$y -y)^2)>= d)
  
  # are there enough analogs left after filtering? If so, calculate forest projection
  # for the set of analogs in nn_list, if not, give the error value (-99)
    if (nrow(subset_d) >= max(nn_list)) {
    subset_d$id<-1:nrow(subset_d)
    
    # set K to the maximum NN size
    
    k<-max(nn_list)
   
    # instead of calculating a distance matrix, draw a random set of k
    indices<-sample(subset_d$id, k)
    subset_d1<-subset_d[indices, ]
    
    # use the fnf function to iterate through the forest/non-forest calculation for all NN variations
    #iterate the fnf function with lapply statement over the nn_list: 
    
    
    out1<-lapply(nn_list, fnf_rand, subset_d1=subset_d1)
    
    # flatten the output from a list of lists into a single vector
    out2<-flatten_dbl(out1)
    
    
    
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
