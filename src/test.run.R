#Anna M Calderon
#Matthew K Lau
#Harvard Forest
#gENM-Data Set Up
#Part 0
#8 July 2016

## Step 0. Set a working directory and File Paths
wd <- '/Users/annacalderon/Desktop/gENM/src'
setwd(wd)

## Step 1. Source Helpers Script
source("helpers.R")


## Step 2. Importing Climate Variables for NE
neClim <- stack("../data/neClim.grd")

mintemp.2006  <- raster("../data/01_01_2006.tiff")
mintemp.2050 <- raster("../data/01_01_2050.tiff")
mintemp.2099 <- raster("../data/01_01_2099.tiff")


## Step 3. Import Species Presence Data
gsp <-read.csv("../data/RICTMEdukesnantucket.csv")
if (is.matrix(gsp) == FALSE){gsp <- data.matrix(gsp)}

## Step 5. Making Clusters and running gENMs

clust <- gClust(x=gsp, vp=mintemp.2006)
invisible(out <- gENM(x=gsp, clust=clust, p=neClim))
gAnalysis(x=out)

test <- list()
for (i in 1:length(out)){
    test[[i]] <- (out[[1]]$pred - out[[i]]$pred)
}

csum <- out[[2]]$pred
for (i in 3:length(out)){
    csum <- csum + out[[i]]$pred
}

cprod <- out[[2]]$pred
for (i in 3:length(out)){
    cprod <- cprod * out[[i]]$pred
}


par(mfcol=c(3,3))
plot(out[[1]]$pred);plot(csum);plot(cprod);invisible(lapply(test[-1],plot))
invisible(lapply(out[-1],function(x) plot(x$pred)))

out.stack <- stack(lapply(out,function(x) x$pred))
layerStats(out.stack,stat='pearson')
plot(out[[1]]$pred)
