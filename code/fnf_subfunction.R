# 8/4/2021

# fnf function is used in the generalized climate analog function, takes the number of the nearest analogs (kn)
# as an argument, returns forest-nonforest classification, variance individual analog votes and the number of analogs considered



fnf <- function(kn, out.dist, subset_d){
  indices<-out.dist$nn.index[1:kn]
  # round(mean(subset[out.dist$nn.index[1:kn],]$forest))
  forest.nonforest<-round(mean(subset_d[indices,]$forest))
  if(forest.nonforest==0) {vars= (1-sum(subset_d[indices,]$forest)/kn)} else{vars = sum(subset_d[indices,]$forest)/kn}
  
  
  # return(c(forest.nonforest, vars, kn))
  return(c(forest.nonforest, vars))
}
