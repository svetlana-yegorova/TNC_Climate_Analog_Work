# climate analog calculating function 
# author: Sean Parks

the.function <- function(i) { # for a focal cell i
  from.xy <- data[i, c('x', 'y')] # extract coordinates
  cmd <- data[i, 'def_h'] # extract cmd value
  et <- data[i, 'aet_h'] # extract et value
  
  
  # select a subset of data points that are within the climatic bin widths 
  # set above (arbitrarily or tested)
  #
  # subset <- subset(data1, ((def_h >= cmd-bin.width) & (def_h <= cmd+bin.width)) &
  #                    ((aet_h >= et-bin.width) & (aet_h <= et+bin.width)))
  
  # variation where bin widthds can be different for def and aet: 
  subset <- subset(data1, ((def_h >= cmd-bin.cmd) & (def_h <= cmd+bin.cmd)) &
                     ((aet_h >= et-bin.aet) & (aet_h <= et+bin.aet)))
  if (nrow(subset) > 0) {
    
    if (nrow(subset) >= 7) { k=7 } else { k=nrow(subset) }
    out.dist <- get.knnx(subset[,c('x', 'y')], from.xy, k=k) # get 7 (or whatever k is) nearest neighbors in the subset defined above
    nn.index <- as.data.frame(out.dist$nn.index)
    forest.nonforest <- round(mean(subset[out.dist$nn.index,]$forest))	
    
    return(c(i, forest.nonforest))
  }
  
  if (nrow(subset) == 0) {
    return(c(i, -99))
  }
}
