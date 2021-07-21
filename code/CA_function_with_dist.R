### modify climate analog function to include a distance limitation
### do not include analogs within 50 km of the focal location
### instead of choosing 7 NN, choose 7 random pixels


the.cad.function <- function(i) { # for a focal cell i
  
  from.xy <- data[i, c('x', 'y')] # extract coordinates
  cmd <- data[i, 'def_h'] # extract cmd value
  et <- data[i, 'aet_h'] # extract et value
  
  
  # select a subset of data points that are within the climatic bin widths 
  # set above (arbitrarily or tested)
  #
  # subset <- subset(data1, ((def_h >= cmd-bin.width) & (def_h <= cmd+bin.width)) &
  #                    ((aet_h >= et-bin.width) & (aet_h <= et+bin.width)))
  
  # variation where bin widthds can be different for def and aet: 
  # bin.cmd = 1
  # bin.aet = 1
   subset <- subset(data1, ((def_h >= cmd-bin.cmd) & (def_h <= cmd+bin.cmd)) &
                      ((aet_h >= et-bin.aet) & (aet_h <= et+bin.aet)))
  
  # subset5 <- subset(data1, ((def_h >= cmd-bin.cmd) & (def_h <= cmd+bin.cmd)) &
  #                    ((aet_h >= et-bin.aet) & (aet_h <= et+bin.aet)))
  # 
  
  if (nrow(subset) > 0) {
    
    # calculate distances between the focal and the subset points: 
    out.dist1 <- get.knnx(subset[,c('x', 'y')], from.xy, k=nrow(subset)) # 
    
    # get distances and indices for the subset defined above
    
    # out.dist <- get.knnx(subset[,c('x', 'y')], from.xy, k=30)
    #out.dist1.1<-subset(out.dist1, out.dist1$nn.dist>=5000)
   # out.dist1
   # gets the indices of the out.dist1
   # for which values are greater than 5000 meters
    out.dist1.1<- which(out.dist1$nn.dist > 50000) 
# ### need to add instructions for when out.dist1.1 is empty
    
    
    # the variation without nearest neighbors: 
    if (length(out.dist1.1) >= 7) { n=7 } else { n=length(out.dist1.1) }
  # now that we have the indices, get the prediction of the 7 closest pixels
    #indices<-out.dist1.1[1:7] # choose 7 or whatever number of closest pixels 
    
  # no nearest neighbor version of the function, choose 7 random records: 
    indices<-sample(out.dist1.1, n)
    forest.nonforest<-round(mean(subset[indices,]$forest))
  
    
    #  nn.index <- as.data.frame(out.dist1$nn.index)
    # forest.nonforest <- round(mean(subset1[out.dist1$nn.index,]$forest))	
    
    return(c(i, forest.nonforest))
  }
  
  if (nrow(subset) == 0) {
    return(c(i, -99))
  }
}
