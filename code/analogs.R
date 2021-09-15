# 6/7/21
# author: Svetlana Yegorova
# This script sets up the dataframe combining forest/non-forest class, current and 
# future data. 

# code steps (mirrored from Sean's code):
# 1) create a dataframe (referenced with raster cell number) to hold 
# climate data
# 2) populate the dataframe with climate data from the reference and future 
# periods

# 3) write a function that, based on the climate tolerance window returns
# a forest/non forest prediction (think of a list of climate tolerance windows)




# libraries
library(raster)
#install.packages("terra")
#library(terra)
library(future.apply)
library(tidyverse)
library(FNN)
library(ggplot2)

# 
# data
def.h<-raster("./outputs/def.tif")
def.f<-raster("./outputs/f_def.tif")
aet.h<-raster("./outputs/aet.tif")
aet.f<-raster("./outputs/f_aet.tif")
forest<-raster("./outputs/forest.tif")

# forest_old<-raster("./outputs/forest_old_crop.tif")
# hist(forest)
# 1) create a dataframe (referenced with raster cell number) to hold climate data

# Which (of the raster library) function below returns cell numbers
# (because cells=T are specified) that fulfill the logical condition
cell.num <- Which(aet.h >= 0, cells=T)
xy <- as.data.frame(xyFromCell(def.h, cell.num)) # then form an xy dataset, with coordinates from the cmd.1995
# cell centers (the ones that are listed in cell.num)


# 2) populate the dataframe with climate data from the reference and future 
# periods
## Extract reference period and future climate
### extracting data from the raster at the locations specified by 
### the xy dataframe
xy$def_h <- raster::extract(def.h, xy[,c('x', 'y')]) # started at 4:11 computer time, finished by 4:19
xy$aet_h <- raster::extract(aet.h, xy[,c('x', 'y')])
xy$def_f <- raster::extract(def.f, xy[,c('x', 'y')])
xy$aet_f <- raster::extract(aet.f, xy[,c('x', 'y')]) # done by 4:20-ish
xy$forest<- raster::extract(forest, xy[, c('x', 'y') ])


# xy$forest_old<-raster::extract(forest_old, xy[, c('x', 'y') ])

### clean up forest values (get them to 0 = no forest or 1 = forest)
### in the 'raw' format the values range between 1 and 2. 

xy_new<-xy%>%
  filter(!is.na(forest))

xy_new<-mutate(xy_new, forest_rcls=case_when(forest==2 ~0, 
                                             forest==1 ~1))

# remove the extra forest column: 
xy_new1<-xy_new%>%
  select(-forest)
colnames(xy_new1)[7]<-"forest"

# xy$forst1<-ifelse(xy$forest<1.5, 0, 1)
# range(xy$forest1) # there are still "NA" values, where are they?
# which(is.na(xy$forest))
# 
# # remove rows with "NA" values from the dataset
# xy<-subset(xy, !is.na(forest))
# xy<-xy[, c(1:3, 5:8)]
# colnames(xy)[4]<-"forest"

 saveRDS(xy_new1, "./outputs/XY_new.rds")
xy_new<-readRDS("./outputs/XY_new.rds")
# remove "forest" column: 

# 3a) perform quadratic tranformation on the data
# 
xy_sq<-sqrt(xy[, -c(1,2)])
xy_sq<-cbind(xy[, c(1,2)], xy_sq)
# save the sqrt dataset: 

saveRDS(xy_sq, "./outputs/xy_sqrt_upd.rds")
xy_sq<-readRDS("./outputs/xy_sqrt.rds")
# data<-cbind(xy[, c(1:2)], xy_sq)
# head(data)
# 
# 
# # 3b) subset the "data" dataframe to 1/1000 to test the timing  
# # 
# test_data<-slice_sample(data, n=10000)
# data1<-data
# data1<-test_data
# par(mfrow=c(1,1))
# hist(test_data$forest)
# 
# 
# # plot AET and Deficit Values: 
# # par(mfrow=c(2,2))
# # hist(data1$aet_h[data1$forest==0], main="AET, NF")
# # hist(data1$aet_h[data1$forest==1], main="AET, F")
# # hist(data1$def_h[data1$forest==0], main="Def, NF")
# # hist(data1$def_h[data1$forest==1], main="Def, F")
# # 
# aet_hist<-ggplot(data=xy_sq, aes(x=aet_h))+
#   geom_histogram()+
#   facet_grid(cols = vars(forest))+
#   theme_classic()+
#   ggtitle("Reference period sqrt(AET)")+
#   theme(axis.text.x = element_text( size=20,  angle=70),
#         axis.text.y = element_text( size = 20),
#         axis.title = element_text(size = 20),
#         plot.title = element_text(size = 20, face = "bold"),
#         legend.title=element_text(size=20),
#         legend.text=element_text(size=15),
#         strip.text = element_text(size = 20))
# 
# 
# png("./outputs/sqrt_aet_h.png", height=700, width=1100)
# aet_hist
# dev.off()
# 
# 
# #   
# def_hist<-ggplot(data=xy_sq, aes(x=def_h))+
#   geom_histogram()+
#   facet_grid(cols = vars(forest))+
#   theme_classic()+
#   ggtitle("Reference period sqrt(Def)")+
#   theme(axis.text.x = element_text( size=20,  angle=70),
#         axis.text.y = element_text( size = 20),
#         axis.title = element_text(size = 20),
#         plot.title = element_text(size = 20, face = "bold"),
#         legend.title=element_text(size=20),
#         legend.text=element_text(size=15),
#         strip.text = element_text(size = 20))
# 
# 
# 
# png("./outputs/sqrt_def_h.png", height=700, width=1100)
# def_hist
# dev.off()
# # 3) write a function that, based on the climate tolerance window returns
# # a forest/non forest prediction (think of a list of climate tolerance windows)
# 
# bin.width<- 1 # set the bin width here
# 
# the.function <- function(i) { # for a focal cell i
#   from.xy <- data[i, c('x', 'y')] # extract coordinates
#   cmd <- data[i, 'def_h'] # extract cmd value
#   et <- data[i, 'aet_h'] # extract et value
#   
#   
#   # select a subset of data points that are within the climatic bin widths 
#   # set above (arbitrarily or tested)
#   #
#   subset <- subset(data1, ((def_h >= cmd-bin.width) & (def_h <= cmd+bin.width)) &
#                      ((aet_h >= et-bin.width) & (aet_h <= et+bin.width)))
# 
#   if (nrow(subset) > 0) {
# 
#     if (nrow(subset) >= 7) { k=7 } else { k=nrow(subset) }
#     out.dist <- get.knnx(subset[,c('x', 'y')], from.xy, k=k) # get 7 (or whatever k is) nearest neighbors in the subset defined above
#     nn.index <- as.data.frame(out.dist$nn.index)
#     forest.nonforest <- round(mean(subset[out.dist$nn.index,]$forest))
# 
#     return(c(i, forest.nonforest))
#   }
# 
#   if (nrow(subset) == 0) {
#     return(c(i, -99))
#   }
# }
# 
# 
# 
# # 4) test function accuracy on the 3a subset
# 
# 
# # 
# # Sys.time()
# # the.function(4)
# # Sys.time()
# 
#   
# # load and run CA_function_chunks: 
# source("./code/CA_function_chunks.R")
# 
# # 5) try out a chunked up function with lapply: 
# plan(multisession, workers=7)
# data<-slice_sample(data, n=100)
# 
# Sys.time()
# test<-future_lapply(X=1:nrow(data), FUN=CA_chunk_function)
# Sys.time()
