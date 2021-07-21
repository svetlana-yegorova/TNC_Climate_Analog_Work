#7/11/21 This file processes sensitivity test RDSs and records 

data1<-readRDS("./outputs/accuracy_sqrt_circa_1mm_bins_7nn_10km.rds")
source("./code/model_assessment_code.R")

d10<-test11


data1<-readRDS("./outputs/accuracy_sqrt_circa_1mm_bins_7nn.rds")
source("./code/model_assessment_code.R")
d50<-test11


data1<-readRDS("./outputs/accuracy_sqrt_circa_1mm_bins_7nn_100km.rds")
source("./code/model_assessment_code.R")
d100<-test11

str(d10)
plot(as.numeric(as.character(accuracy))~model, data=d10)
plot(as.numeric(as.character(accuracy))~model, data=d100)


d10$accuracy<-as.numeric(as.character(d10$accuracy))
d50$accuracy<-as.numeric(as.character(d50$accuracy))
d100$accuracy<-as.numeric(as.character(d100$accuracy))


d10[order(d10$accuracy), ]
d50[order(d10$accuracy), ]
d100[order(d10$accuracy), ]

accy_tbl<-data.frame(rbind(d10[1, c(1,2)], d50[1, c(1,2)], d100[1, c(1,2)]))
accy_tbl$buffer<-c(10, 50, 100)

write_csv(accy_tbl, "./outputs/max_accy_by_dbuffer_7nn.csv")
