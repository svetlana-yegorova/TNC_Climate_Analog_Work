#7/10/21 This file contains accuracy, kappa and agreement assessment scripts 
# for contemporary forest/non-forest predictions

# libraries
library(tidyverse)
library(caret)


# data
# data1<-readRDS("./outputs/accuracy_sqrt_circa_1mm_bins_15nn.rds")

# for the 5 n dataset, need to remove the 0.9 data first (left over from previous
# test). Remove columnts that have 0.9 in their names: 
# data0<-data1[,-grep("0.9",colnames(data1))]
# ncol(data0)
# str(data0)
# 
# data1<-data0
# examine the accuracy: 
# first extract/select the prediction columns: 

accy<-data1%>%
  select(starts_with("forest"))

str(accy)
data<-cbind(data1[, c(1:2, 4)], accy)

# are there n/a's in this new dataframe? what about -99s?
errors<-data.frame(which((data == -99), arr.ind=TRUE))
error_rows<-unique(errors$row)

# remove errow rows from the data, it has no analogs for the narrow values of def: 
if(nrow(errors)>0) { data<-data[-error_rows, ]} 
  
# nrow(data)
# load the accuracy function: 
# source("./code/accuracy_sensitivity_FUNs.R")
test<-lapply(X=c(5:ncol(data)), acc_forest_s_s)

# get the kappa values: 

test0<-lapply(X=c(5:ncol(data)), kappa_forest)

test1<-data.frame(do.call("rbind", test))
test_kappa<-data.frame(do.call("rbind", test0))

# attach kappa values to the accuracy data: 
test1<-cbind(test1, test_kappa[, 2])

# test1

# calculate the average agreement score for each model: 
# get agreement data: 
# data<-readRDS("./outputs/accuracy_sqrt_circa_1mm_bins_15nn.rds")
agrmt<-data1%>% # remove -99 rows before calculating accuracy
  dplyr::select(starts_with("pct"))%>%
  summarise(across(where(is.numeric), ~mean(.x)))


## add agreement data to unsorted accuracy data: 
# str(agrmt)
agrmt1<-t(agrmt)
# head(agrmt1)

test1<-cbind(test1, agrmt1[, 1])
head(test1)
# 
# 
# colnames(test1)<-c("model", "accuracy", "acc_CI_lower", "acc_CI_upper",
# "sensitivity", "specificity", "kappa")

 colnames(test1)<-c("model", "accuracy", "acc_CI_lower", "acc_CI_upper", 
                   "sensitivity", "specificity", "kappa", 
                    "mean_analog_agreement")
# 

# sort by greatest accuracy values: 
test11<-test1[ order(test1$accuracy, decreasing = TRUE), ]
test11
# save the accuracy table 
# write_csv(test11, "./outputs/accuracy_sqrt_075_125_50km_15analogs.csv")

test1[order(test1$mean_analog_agreement, decreasing = TRUE), ]

## are there duplicate model names/mistakes?
test1[order(test1$model, decreasing = TRUE), ]

### clean the output: 
# 
# test11<-test11%>%
#   separate(model, into=c("model", "pct_nn"), sep=11)%>%
#   separate(pct_nn, into=c("pct", "nn"), sep="pct_N")

