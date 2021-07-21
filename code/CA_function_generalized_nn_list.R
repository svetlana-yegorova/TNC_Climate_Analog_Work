#7/6/21

# version of the climate analog function that calculates accuracy for different 
# numbers of analogs considered. 

#### junk space to define vars when testing
n=list(15, 11, 5)
i=1
d=50000

bin.aet=1
bin.cmd=1

hist(data$def_h)
#################


the.cad.function_g1 <-function(i, d, n) { # for a focal cell i, buffer distance d
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
    # loop through different number of analogs: 
    output<-list()
    for(p in 1:length(n)){
      
      pl=n[[p]]
      if (nrow(subset) >= pl) { k=pl } else { k=nrow(subset) }
      
      # calculate distances between the focal and the subset points: 
      
      out.dist <- get.knnx(subset[,c('x', 'y')], from.xy, k=k) # get 7 (or whatever k is) nearest neighbors in the subset defined above
      nn.index <- as.data.frame(out.dist$nn.index)
      forest.nonforest <- round(mean(subset[out.dist$nn.index,]$forest))	
      
      # report variance/agreement: 
      if(forest.nonforest==0) {vars= (1-sum(subset[out.dist$nn.index,]$forest)/pl)} else{vars = sum(subset[out.dist$nn.index,]$forest)/pl}
      
      #vars<-sum(subset[out.dist$nn.index,]$forest)/n
      
      
      #  nn.index <- as.data.frame(out.dist1$nn.index)
      # forest.nonforest <- round(mean(subset1[out.dist1$nn.index,]$forest))	
      output[[p]]<-c(i, forest.nonforest, vars, pl)
      
    }
    output1<-do.call("c", output)
    return(output1)
    
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