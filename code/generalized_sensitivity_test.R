# 6/29/21
# author: Svetlana Yegorova

# generalized sensitivity test that goes through different combinations 
# of bins (0.5, 0.75, 1, 1.25, 2, 5), buffer distances (10, 25, 50, 100 km)
# and number of analogs considered (7, 25, 99)

# maybe do the bin by distance combination and examine the effect of 
# the number of analogs separately. 

# researching vectorization 
# researching outer() - refers to 'outer product': we can construct a 
# matrix or an n-dimentional array of the combinations of variables. 

# libraries

library(tidyverse)
library(FNN)
library(caret)
library(data.table)
 
# data
# raw/non-squareroot transformed data: 
data1<-readRDS("./outputs/XY.rds")

# take the test data used in n7 results: 
data<- slice_sample(data1, n=1000)
# data<-readRDS("./outputs/accuracy_sqrt_circa_1mm_bins_7nn.rds")
# data<-data[, c(1:5)]

# function: 
source("./code/CA_function_generalized.R")

# # mapply example: 
# # define distance buffer variable: 
# d=10000
# 
# # define number of analogs variable: 
# n=15

# define bin width combinations: 
# bin.aet=c( 0.5, 1.5, 2)
# bin.cmd=c( 0.5, 1.5, 2)
# test_bins<-crossing(bin.cmd, bin.aet)

bin.aet<-c(20, 30, 40)
bin.cmd<-c(20, 30, 40)
test_bins<-tibble(bin.aet, bin.cmd)


Sys.time()
for(i in 1:nrow(test_bins)){
  
  bin.cmd<-test_bins[i, "bin.cmd"] %>% pull(.)
  bin.aet<-test_bins[i, "bin.aet"] %>% pull(.)
  
  # run the climate analog function: 
  out<-lapply(X=1:nrow(data), FUN=the.cad.function_g, d=50000, n=7)
  
  out1<-do.call('cbind', out )
  data[[paste0("forest.pred", bin.cmd,"_", bin.aet)]] <- out1[2,]
  data[[paste0("pct_agrmt", bin.cmd,"_", bin.aet)]] <- out1[3,]
  
  print(i)
  
}

Sys.time()



# save the output file: 
saveRDS(data, "./outputs/accuracy_raw_20_40mm_7NN_50km.rds")


# examine the accuracy (moved the accuracy assessement into a separate script): 
# # first filter out the prediction columns: 
# data<-data[-922, ]
# 
# accy<-data%>%
#   select(starts_with("forest"))
# 
# str(accy)
# data_accy<-cbind(data[, c(1:3)], accy)
# data<-data_accy
# # are there n/a's in this new dataframe? what about -99s?
# errors<-which((data == -99), arr.ind=TRUE)
# err_names<-colnames(data[, c(5:7, 10:11, 15:16, 20:21, 25:26)])
# errors<-cbind(errors, err_names)
# 
# # remove row 88 from the data, it has no analogs for the narrow values of def:
#  # data<-data[-922, ]
# 
# head(data)
# # load the accuracy function:
# source("./code/accuracy_sensitivity_FUNs.R")
# test<-lapply(X=c(5:ncol(data)), acc_forest_s_s)
# 
# # get the kappa values:
# test0<-lapply(X=c(5:ncol(data)), kappa_forest)
# 
# test1<-data.frame(do.call("rbind", test))
# test_kappa<-data.frame(do.call("rbind", test0))
# 
# # attach kappa values to the accuracy data:
# test1<-cbind(test1, test_kappa[, 2])
# 
# test1
# 
# # calculate the average agreement score for each model:
# # get agreement data:
# data<-readRDS("./outputs/accuracy_sqrt_extra_bins_7nn_50km.rds")
# agrmt<-data[-922, ]%>%
#   dplyr::select(starts_with("pct"))%>%
#   summarise(across(where(is.numeric), ~mean(.x)))
# 
# 
# ## add agreement data to unsorted accuracy data:
# str(agrmt)
# agrmt1<-transpose(agrmt)
# test1<-cbind(test1, agrmt1)
# colnames(test1)<-c("model", "accuracy", "sensitivity", "specificity", "kappa",
#                    "mean_analog_agreement")
# 
# # filter out any model name with 0.9 in it:
# 
# # sort by greatest accuracy values:
# test11<-test1[ order(test1$accuracy, decreasing = TRUE), ]
# head(test11)
# 
# # save the accuracy table
# write_csv(test11, "./outputs/accuracy_sqrt_extra_bins_7nn_50km.csv")
# 
# test1[order(test1$mean_analog_agreement, decreasing = TRUE), ]
# 
# ## are there duplicate model names/mistakes?
# test1[order(test1$model, decreasing = TRUE), ]
# nrow(test1)



