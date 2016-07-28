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
out <- gENM(x=gsp, clust=clust, p=mintemp.2006) 
gAnalysis(x=out)

