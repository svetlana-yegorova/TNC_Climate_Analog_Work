# file for exploring TNC data. 

# libraries
library(raster)
library(rgdal)

# historical data
def_h<-raster("./data/Deficit/Deficit.img")
aet_h<-raster("./data/AET/AET.img")

# future data
def_f<-raster("./data/Deficit_f/Deficit/Deficit.img")
aet_f<-raster("./data/AET_f/AET/AET.img")

# # quick visualization: 
# plot(def_h)
# 
# # look at the raster attributes: 
# def_h
# 
# 
# # look at the distribution of values: 
# hist(def_h, maxpixels=100,000)
# 
# # take a square root of the values: 
# hist(sqrt(def_h), maxpixels=1000000)
# 
# # view square root and raw data side by side:
# par(mfrow=c(1, 2))
# 
# hist(def_h, maxpixels=1000000, main="def")
# hist(sqrt(def_h), maxpixels=1000000, main="sqrt def")

# square root transformation stretches out the low end of the data and compresses
# the high end of the data

# load the forest/non-forest mask:  
forest<-raster("./data/R6Forest_90m/r6forest_90m/")
forest
plot(forest)

# forest and deficit layers have different projections (crs).
# also different extents 
# different number of cells 
# makes sense because the forest/nonforest extent is much larger
# and has more cells. 

# 1= non forest
# 2 = forest
# reproject forest raster: 
# 1. get the crs to which forest is going to be reprojected: 
crs(def_h)

# 2. do the reprojecting:

rasterOptions(maxmemory=1e+08) # to avoid crashing R, limit how much
# memory can be used by the package

forest_r<-projectRaster(forest, crs=crs(def_h), res=90) # add "res=90"
# to make sure that the output cell size is 90 meters

# check how re-projection worked: 
forest_r
def_h

# plot rasters on top of each other: 
plot(forest_r)
plot(def_h, alpha=0.4, add=T) # forest_r has a greater extent- it 
#continues into washington, while def_h is in oregon only


# clip rasters to the same extent: 
forest_crop<-crop(forest_r, def_h)

# plot rasters on top of each other again: 
plot(forest_crop)
plot(def_h, alpha=0.4, add=T)

forest_crop
def_h

extent(forest_crop) <- alignExtent(forest_crop, def_h) 
extent(def_h)

def_h
forest_crop
forest_r


# try clipping the other way (clipping to smaller raster extent): 
def_crop<-crop(def_h, forest_crop)
aet_crop<-crop(aet_h, forest_crop)

# crop future climate data: 
deff_crop<-crop(def_f, forest_crop)
aetf_crop<-crop(aet_f, forest_crop)

aet_crop
def_crop
forest_crop

# plot the two rasters: 
par(mfrow=c(2, 2))
plot(def_crop, main="Historic DEF")
plot(aet_crop, main="Historic AET")
plot(deff_crop, main="Future DEF")
plot(aetf_crop, main="Future AET")


# save newly cropped and reprojected rasters: 
writeFormats()

raster_list<-list(def_crop, aet_crop, deff_crop, aetf_crop, forest_crop)
names_list<-list("def", "aet", "f_def", "f_aet", "forest")

for(i in 1:length(raster_list)){
  filename=paste0(names_list[[i]], ".tif")
  writeRaster(raster_list[[i]], paste0("./outputs/", filename) ,
              format="GTiff" )
}

