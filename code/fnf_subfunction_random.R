# 8/14/21 fnf subfunction for random analog seleciton 

# fnf function is used in the generalized climate analog function, takes the number of the nearest analogs (kn)
# as an argument, returns forest-nonforest classification, variance individual analog votes and the number of analogs considered



fnf_rand <- function(kn, subset_d1){
  # indices<-subset_d1$id[1:kn]
  # round(mean(subset[out.dist$nn.index[1:kn],]$forest))
  forest.nonforest<-round(mean(subset_d1[1:kn,]$forest))
  if(forest.nonforest==0) {vars= (1-sum(subset_d1[1:kn,]$forest)/kn)} else{vars = sum(subset_d1[1:kn,]$forest)/kn}
  
  
  # return(c(forest.nonforest, vars, kn))
  return(c(forest.nonforest, vars))
}
