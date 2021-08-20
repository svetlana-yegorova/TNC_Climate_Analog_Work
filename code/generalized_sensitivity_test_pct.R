# 7/16/21 This is the generalized version of the climate analog function
# takes additional arguments of distance buffer (d) and n number of nearest
# neighbour cells (n)

# !!!! Instead of using a pre-set bin, this function makes a custom bin for 
# each plot by taking 10% of the focal aet and deficit values on each side of the 
# focal value 


the.cad.function_pct <-function(i, d, nn_list, p) { # for a focal cell i, buffer distance d
  # list of n nearest neighbours, and p percent to inform climate bin width
  
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
  
  # if (nrow(subset) > 0) {
  #   
  #   if (nrow(subset) >= n) { k=n } else { k=nrow(subset) }
  #   
  #   
  #   # calculate distances between the focal and the subset points: 
  #   
  #   out.dist <- get.knnx(subset[,c('x', 'y')], from.xy, k=k) # get 7 (or whatever k is) nearest neighbors in the subset defined above
  #   nn.index <- as.data.frame(out.dist$nn.index)
  #   forest.nonforest <- round(mean(subset[out.dist$nn.index,]$forest))	
  #   distance<-mean(out.dist$nn.dist)
  #   # report variance/agreement: 
  #   if(forest.nonforest==0) {vars= (1-sum(subset[out.dist$nn.index,]$forest)/n)} else{vars = sum(subset[out.dist$nn.index,]$forest)/n}
  #   
  #   #vars<-sum(subset[out.dist$nn.index,]$forest)/n
  #   
  #   
  #   #  nn.index <- as.data.frame(out.dist1$nn.index)
  #   # forest.nonforest <- round(mean(subset1[out.dist1$nn.index,]$forest))	
  #   
  #   return(c(i, forest.nonforest, vars, distance))
  # }
  # 
  if (nrow(subset_d) == 0) {
    return(c(i, -99, -99)) # add the third element to signify that there is no variance calculation
  }
}

