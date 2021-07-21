# 6/18/2021 Functions that extract accuracy, sensitivity and specificity out of 
# forest classification confusion matrices. 


# Author: Svetlana Yegorova

# straight up accuracy function
acc_forest<-function(i){
  return(c(colnames(data)[i], (confusionMatrix(as.factor(data[, 4]), 
                                               as.factor(data[,i]), 
                                               positive = "1")$overall[[1]])))
}



# accuracy, accuracy 90% CI (lower, upper), sensitivity and specificity: 

acc_forest_s_s<-function(i){
  return(c(colnames(data)[i], 
           (confusionMatrix(as.factor(data[, 4]), 
                            as.factor(data[,i]), positive = "1")$overall[[1]]),
           (confusionMatrix(as.factor(data[, 4]), 
                            as.factor(data[,i]), positive = "1")$overall[[3]]),
           (confusionMatrix(as.factor(data[, 4]), 
                            as.factor(data[,i]), positive = "1")$overall[[4]]),
           (confusionMatrix(as.factor(data[, 4]), 
                            as.factor(data[,i]), positive ="1")$byClass[[1]]),
           confusionMatrix(as.factor(data[, 4]), 
                           as.factor(data[,i]), positive = "1")$byClass[[2]]))
}

kappa_forest<-function(i) {
  return(c(colnames(data)[i], (confusionMatrix(as.factor(data[, 4]), 
                                               as.factor(data[,i]), positive = "1")$overall[[2]])))}

