library(raster)

setwd('D:/ALWRI/projects/Sean/fire_facilitated_conversion/FIA.analysis.v2/gis.data/climate.data/')

cmd.1995 <- raster('cmd.1995.tif')
et.1995 <- raster('et.1995.tif')

cmd.2055 <- raster('cmd.2055.tif')
et.2055 <- raster('et.2055.tif')

# Which (of the raster library) function below returns cell numbers
# (because cells=T are specified) that fulfill the logical condition
cell.num <- Which(cmd.1995 >= 0, cells=T)
xy <- as.data.frame(xyFromCell(cmd.1995, cell.num)) # then form an xy dataset, with coordinates from the cmd.1995
# cell centers (the ones that are listed in cell.num)

## Extract reference period and future climate
### extracting data from the raster at the locations specified by 
### the xy dataframe
xy$cmd.1995 <- extract(cmd.1995, xy[,c('x', 'y')])
xy$et.1995 <- extract(et.1995, xy[,c('x', 'y')])
xy$cmd.2055 <- extract(cmd.2055, xy[,c('x', 'y')])
xy$et.2055 <- extract(et.2055, xy[,c('x', 'y')])

## Set climate bin width. You will need to test different options (retrospective validation)
## to see what works best. You might want to test bin widths are not the same for cmd and et.
## For reference, we square rooted cmd and et in a different paper and then used a bin.width=1.

bin.width <- 50


###########################################
## Current distribution of forest
## Some version of this can be used to conduct a retrospective validation 
## (but will need to be edited)
## Perhaps exclude all pixels from within 50km, or something like that, for 
## predicting reference period forest/non-forest and use that for a validation
###########################################

the.function <- function(i) { # for a focal cell i
	from.xy <- xy[i, c('x', 'y')] # extract coordinates
	cmd <- xy[i, 'cmd.1995'] # extract cmd value
	et <- xy[i, 'et.1995'] # extract et value
  
	
	# select a subset of data points that are within the climatic bin widths 
	# set above (arbitrarily or tested)
	#
	subset <- subset(data, ((CMD_1995 >= cmd-bin.width) & (CMD_1995 <= cmd+bin.width)) &
	                   ((ET_1995 >= et-bin.width) & (ET_1995 <= et+bin.width)))

	if (nrow(subset) > 0) {

		if (nrow(subset) >= 7) { k=7 } else { k=nrow(subset) }
		out.dist <- get.knnx(subset[,c('x', 'y')], from.xy, k=k) # get 7 (or whatever k is) nearest neighbors in the subset defined above
		nn.index <- as.data.frame(out.dist$nn.index)
		forest.nonforest <- round(mean(subset[out.dist$nn.index,]$forest))	

		return(c(i, forest.nonforest))
	}

	if (nrow(subset) == 0) {
		return(c(i, -99))
	}
}

Sys.time()

# This is for parallel processing
cl <- makeCluster(3)
registerDoParallel(cl)
clusterExport(cl, c('xy', 'data'))
clusterEvalQ(cl, library(FNN)) 			

## The below can be used for testing if things are working and for an estimate of how long it will take to run all of Oregon
## Also, if you want to do a bunch of validations to test different climate bin widths, you probably want to go with a subset to save time
## pixel.subset <- sample(1:nrow(pixel.subset), 10000, replace=F) ## this takes a random sample of 1000 focal pixels (use this or the one below, not both)
## pixel.subset <- sample(1:nrow(pixel.subset), nrow(subset * 0.01), replace=F) ## this takes a 1% random sample of focal pixels (use this or the one above, not both)
## out <- clusterApply(cl, x=pixel.subset, fun=the.function)

## This will run on all of Oregon
out <- clusterApply(cl, x=1:nrow(xy), fun=the.function)

stopCluster(cl)

out1 <- do.call('cbind', out)

xy$forest.1995 <- out1[2,]
head(xy)
Sys.time()


###########################################
## Future distribution of forest
###########################################


cell.num <- Which(cmd.1995 >= 0, cells=T)
xy <- as.data.frame(xyFromCell(cmd.1995, cell.num))

xy$cmd.1995 <- extract(cmd.1995, xy[,c('x', 'y')])
xy$et.1995 <- extract(et.1995, xy[,c('x', 'y')])
xy$cmd.2055 <- extract(cmd.2055, xy[,c('x', 'y')])
xy$et.2055 <- extract(et.2055, xy[,c('x', 'y')])


the.function <- function(i) {
	from.xy <- xy[i, c('x', 'y')]
	cmd <- xy[i, 'cmd.2055']
	et <- xy[i, 'et.2055']

	subset <- subset(data, ((CMD_1995 >= cmd-bin.width) & (CMD_1995 <= cmd+bin.width)) & ((ET_1995 >= et-bin.width) & (ET_1995 <= et+bin.width)))
	if (nrow(subset) > 0) {

		## here, we used the seven nearest neighbors. Svetlana, you will want to change this probably.
		if (nrow(subset) >= 7) { k=7 } else { k=nrow(subset) }
		out.dist <- get.knnx(subset[,c('x', 'y')], from.xy, k=k)
		nn.index <- as.data.frame(out.dist$nn.index)
		forest.nonforest <- round(mean(subset[out.dist$nn.index,]$forest))	

		return(c(i, forest.nonforest))
	}

	if (nrow(subset) == 0) {
		return(c(i, -99))
	}
}



# This is for parallel processing on a WIndows machine. There are easier ways to do this on Linux.
cl <- makeCluster(4) ## this sets the number of cores to parallel process
registerDoParallel(cl)
clusterExport(cl, c('xy', 'data'))
clusterEvalQ(cl, library(FNN)) 			

## The below can be used for testing if things are working and for an estimate of how long it will take to run all of Oregon
## Also, if you want to do a bunch of validations to test different climate bin widths, you probably want to go with a subset to save time
## pixel.subset <- sample(1:nrow(pixel.subset), 10000, replace=F) ## this takes a random sample of 1000 focal pixels (use this or the one below, not both)
## pixel.subset <- sample(1:nrow(pixel.subset), nrow(subset * 0.01), replace=F) ## this takes a 1% random sample of focal pixels (use this or the one above, not both)
## out <- clusterApply(cl, x=pixel.subset, fun=the.function)

## This will run on all of Oregon
out <- clusterApply(cl, x=1:nrow(xy), fun=the.function)

stopCluster(cl)

out1 <- do.call('cbind', out)

xy$forest.2055 <- out1[2,]
head(xy)


forest.1995 <- rasterFromXYZ(xy[,c('x','y', 'forest.1995')], res=cmd.1995, crs=cmd.1995)
forest.2055 <- rasterFromXYZ(xy[,c('x','y', 'forest.2055')], res=cmd.1995, crs=cmd.1995)

setwd('D:/ALWRI/projects/Sean/fire_facilitated_conversion/FIA.analysis.v2/gis.data/predicted.forest')

writeRaster(forest.1995, 'forest.1995.tif', format='GTiff')
writeRaster(forest.2055, 'forest.2055.tif', format='GTiff')

write.csv(xy, 'FIA.to.forest.predictions.csv', row.names=F)
















#####################
### Run this first
#####################

library(rgdal)
library(FNN)
library(parallel)
library(doParallel)
library(randomForest)

setwd('D:/ALWRI/projects/Sean/fire_facilitated_conversion/FIA.analysis/FIA.data/draft1.tables/')


climate <- read.csv('SP_ClimateLayers.csv')
cond.tree.seed <- read.csv('SP_COND_TREE_SEED.csv')
list.of.conds <- read.csv('SP_ListOfCONDs.csv') 
mtbs <- read.csv('SP_MTBS_CMBJoin.csv') 
veg.join1 <- read.csv('SP_P2Veg_JoinLayer1.csv')
veg.join2 <- read.csv('SP_P2Veg_JoinLayer2.csv')
veg.join3 <- read.csv('SP_P2Veg_JoinLayer3.csv')
veg.join4 <- read.csv('SP_P2Veg_JoinLayer4.csv')
plot <- read.csv('SP_PLOT_data.csv')
prism.1971.2000 <- read.csv('SP_Prism_71_00.csv')
prism.1981.2010 <- read.csv('SP_Prism_81_10.csv') 
time.since.fire <- read.csv('SP_TimeSinceFire.csv') 

## plot lat/long 
plot(plot$LON_FS, plot$LAT_FS, pch=19, cex=0.1)
## It looks as though data from California, Oregon, and Washington are missing
## Wyoming looks 'sparse'

#######################
## choose plots where all 4 subplots sampled the same stand
########################

cond.tree.seed <-subset(cond.tree.seed,CONDPROP_UNADJ==1)


######################
### create unique identifier for each plot
######################

cond.tree.seed$UNIQUEID <- paste(cond.tree.seed$PLOT,cond.tree.seed$STATECD,cond.tree.seed$COUNTYCD,cond.tree.seed$INVYR,sep='_')
cond.tree.seed$PLOTID <- paste(cond.tree.seed$PLOT,cond.tree.seed$STATECD,cond.tree.seed$COUNTYCD,sep='_')

###############################
# join coordinates to cond.tree.seed table
###############################
#plot$PLOTID<-paste(plot$PLOT,plot$STATECD,plot$COUNTYCD,sep='_')
data <- merge(cond.tree.seed, plot[,c(1,10,11,12)], by.x ='PLT_CN',by.y='CN')
nrow(data)
#plot(data$LON_FS, data$LAT_FS)


#####################
### How many times has a plot been visited?
####################
num.samp <- data.frame(table(data$PLOTID))
names(num.samp)[1] <- "PLOTID"


##################
## subset data to plots visited more than once
################

plot.rev <- num.samp[num.samp$Freq>1,]

## append lat lon and elevation
ind <- match(plot.rev$PLOTID,data$PLOTID)
plot.rev$LAT <- data$LAT_FS[ind]
plot.rev$LON <- data$LON_FS[ind]
plot.rev$elev <- data$ELEV[ind]

## map revisited plots
plot(plot.rev$LON, plot.rev$LAT,cex=plot.rev$Freq)


# how many duplicates?
foo <- data.frame(table(data$UNIQUEID))
names(foo)[1] <- "PLOTID"

foo2 <- foo[foo$Freq > 1,]   
dim(foo2) ## 213 are duplicates

#### drop duplicates
data <- data[!duplicated(data$PLOTID), ]
nrow(data)


# how many duplicates?
foo <- data.frame(table(paste(data$LAT_FS, data$LON_FS, sep='_')))
names(foo)[1] <- "PLOTID"

foo2 <- foo[foo$Freq > 1,]   
dim(foo2) ## 25 are duplicates

#### drop duplicates
data <- data[!duplicated(paste(data$LAT_FS, data$LON_FS, sep='_')), ]
nrow(data)

####################################################
####################################################
####################################################

data <- merge(data, climate, by.x=c('STATECD', 'INVYR', 'COUNTYCD', 'PLOT'), by.y=c('STATECD', 'INVYR', 'COUNTYCD', 'PLOT_NIMS'))
rm(climate, cond.tree.seed)

data$ET_1995 <- data$Eref_1995 - data$CMD_1995

data1 <- data
coordinates(data1) <- c("LON_FS", "LAT_FS")
proj4string(data1) <- CRS("+proj=longlat + ellps=WGS84")
data1 <-spTransform(data1, CRS="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
data1 <- as.data.frame(coordinates(data1))
data$x <- data1$LON_FS
data$y <- data1$LAT_FS
rm(data1)

data <- data[,c('PLT_CN', 'INVYR', 'QA_STATUS', 'FORTYPCD', 'FLDTYPCD', 'CMD_1995', 'ET_1995', 'MCMT_1995', 'LON_FS', 'LAT_FS', 'x', 'y')]

data$forest <- 0
data$forest[data$FORTYPCD > 0] <- 1

nrow(subset(data, forest.obs == 1)) ## 21071

##########################
##########################
