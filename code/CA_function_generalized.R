# 6/29/21 This is the generalized version of the climate analog function
# takes additional arguments of distance buffer (d) and n number of nearest
# neighbour cells (n)



the.cad.function_g <-function(i, d, n) { # for a focal cell i, buffer distance d
  # and n nearest neighbours
  
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
    
    if (nrow(subset) >= n) { k=n } else { k=nrow(subset) }
    
    # loop through different number of analogs: 

    
    # calculate distances between the focal and the subset points: 
    
    out.dist <- get.knnx(subset[,c('x', 'y')], from.xy, k=k) # get 7 (or whatever k is) nearest neighbors in the subset defined above
    nn.index <- as.data.frame(out.dist$nn.index)
    forest.nonforest <- round(mean(subset[out.dist$nn.index,]$forest))	
    
    # report variance/agreement: 
    if(forest.nonforest==0) {vars= (1-sum(subset[out.dist$nn.index,]$forest)/n)} else{vars = sum(subset[out.dist$nn.index,]$forest)/n}

    #vars<-sum(subset[out.dist$nn.index,]$forest)/n
    
    
    #  nn.index <- as.data.frame(out.dist1$nn.index)
    # forest.nonforest <- round(mean(subset1[out.dist1$nn.index,]$forest))	
    
    return(c(i, forest.nonforest, vars))
  }
  
  if (nrow(subset) == 0) {
    return(c(i, -99, -99)) # add the third element to signify that there is no variance calculation
  }
}

