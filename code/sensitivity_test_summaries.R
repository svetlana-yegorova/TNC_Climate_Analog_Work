# This script processes and visualizes the effects of distance buffer, 
# climate bins and number of analogs on accuracy and agreement. 

# libraries
library(tidyverse)
library(splitstackshape)
library(stringr)
library(ggplot2)
library(patchwork)

# data where the number of analogs varies, but distance stays the same
n5<-read_csv("./outputs/accuracy_sqrt_05_125_50km_5analogs.csv")
n7<-read_csv("./outputs/accuracy_sqrt_075_125_50km_7analogs.csv")
n9<-read_csv("./outputs/accuracy_sqrt_075_125_50km_9analogs.csv")
n15<-read_csv("./outputs/accuracy_sqrt_075_125_50km_15analogs.csv")
n100<-read_csv("./outputs/accuracy_sqrt_075_125_50km_100analogs.csv")

# remove 0.5 bins from n5 for now: 
n5.1<-n5[!grepl("0.5", n5$model),]

# make a plot of maximum accuracies since the tibbles above are sorted 
# by accuracy, take the first row of each of the tables: 
acc_tbl<-rbind(n5.1[1,  c("model", "accuracy", "sensitivity", "specificity", "kappa", "mean_analog_agreement")],
               n7[1,  c("model", "accuracy",  "sensitivity", "specificity", "kappa", "mean_analog_agreement")], 
               n9[1,   c("model", "accuracy",  "sensitivity", "specificity", "kappa", "mean_analog_agreement")],
               n15[1,  c("model", "accuracy",  "sensitivity", "specificity", "kappa", "mean_analog_agreement")], 
               n100[1,   c("model", "accuracy",  "sensitivity", "specificity", "kappa",  "mean_analog_agreement")])
acc_tbl$n<-c(5, 7, 9, 15, 100)

### combine accuracy outputs from different number of analogs to look at the effect of 
# the number of analogs for a given bin width. 


head(n7)
n7$n<-7
n9$n<-9
n15$n<-15
n100$n<-100
n5.1$n<-5

big_table<-rbind(n5.1,  n7[, -c(3:4)], n9, n15[, -c(3:4)], n100) ### removing CI columns

str(big_table)

# separate out the bin values from "forest"
big_table1<-big_table %>% 
  separate(model, sep=11, into=c("forest", "new_model")) %>% 
  separate(new_model, sep="_", into=c("def", "aet"), remove=FALSE)


# convert model names to a factor: 
big_table1$new_model<-as.factor(big_table1$new_model)

t_label<-as.character(big_table1$new_model)
big_table1 %>% 
  mutate(def=as.numeric(def))

# sort results by deficit values: 
def<-big_table1 %>% 
  arrange(by= "def") %>% 
  ggplot( aes(x=def, y=accuracy, color=as.factor(n)))+
  geom_point() +
  theme_bw()+
  labs(title = "accuracy by deficit and # of analogs", color="# of analogs")
  
  # scale_x_discrete(name="climate bins combinations (def_aet)", labels=t_label)+
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
# sort results by aet values: 
aet<-big_table1 %>% 
  arrange(by= "aet") %>% 
  ggplot( aes(x=aet, y=accuracy, color=as.factor(n)))+
  geom_point() +
  theme_bw()+
  labs(title = "accuracy by deficit and # of analogs", color="# of analogs")

(def+aet)

#reorder by accuracy: 
head(acc_tbl) 
acc_tbl<-acc_tbl[order(acc_tbl$accuracy, decreasing=TRUE), ]

# split up the "model" column in a way that makes bin size clear: 
how<-cSplit(acc_tbl, "model", sep="_", stripWhite=TRUE, type.convert=FALSE)
how
# remove "forest.pred" from the deficit values: 
how$cwd<-gsub("forest.pred", "", how$model_1)
how<-how[, !"model_1"]
colnames(how)[7]<-"aet"

# write the accuracy table: 
write_csv(how, "./outputs/accy_smry.csv")

plot(accuracy~n, acc_tbl, ylim=c(0.75, 0.95), main="Accuracy ")


# look at the effect of the number of analogs on maximum agreement and agreement range: 
# the range and the maximum agreement do not seem to vary with the number of analogs considered. 

range(n7$mean_analog_agreement)
model_list<-c(n5.1, n7, n9, n15, n100)

range_agr<-function(model){
 range(model$mean_analog_agreement)
}

agr_list<-rbind(range_agr(n5.1), range_agr(n7), range_agr(n9), range_agr(n15), range_agr(n100))

agreement_table<-data.frame(cbind(agr_list, c(5, 7, 9, 15, 100)))

head(agreement_table)
colnames(agreement_table)<-c('min_agreement', "max_agreement", "n_analogs")

# save the agreement table: 
write_csv(agreement_table, "./outputs/agreement_by_number_of_analogs.csv")


# look at the effects of distance on accuracy and agreement: 
d10<-readRDS("./outputs/accuracy_sqrt_circa_1mm_bins_7nn_10km.rds")

