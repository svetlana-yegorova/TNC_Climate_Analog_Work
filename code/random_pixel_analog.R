# 6/21/21
# what is the accuracy if we take a random pixel instead of the NN? 

i=5
the.cad_random.function <- function(i) { # for a focal cell i
  
   from.xy <- data[i, c('x', 'y')] # extract coordinates
   subset <- subset(data1, ((x <= from.xy$x-50000) | (x >= from.xy$x+50000))
                     &((y <=from.xy$y-50000)| (y >=from.xy$y+50000)))
  
  if (nrow(subset) > 0) {
    if (nrow(subset) >= 7) { n=7 } else { n=nrow(subset) }
    # now that we have the indices, get the prediction of the 7 closest pixels
    #indices<-out.dist1.1[1:7] # choose 7 or whatever number of closest pixels 
    
    # no nearest neighbor version of the function, choose 7 random records: 
    subset$id<-1:nrow(subset)
    indices<-sample(subset$id, n)
    forest.nonforest<-round(mean(subset[indices,]$forest))
    
    return(c(i, forest.nonforest))
  }
  
  if (nrow(subset) == 0) {
    return(c(i, -99))
  }
}
