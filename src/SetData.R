#Anna M Calderon
#Matthew K Lau
#Harvard Forest
#gENM-Data Set Up
#Part 0
#8 July 2016

## Step 0. Set a working directory and File Paths

wd <- '/Users/annacalderon/Desktop/gENM/data'
setwd(wd)

path <- ("") 
if (path == ""){path <- "../data"}
filename <- ("GspeciesBC_2.5.grd")
croppeddata<- paste(path,filename, sep="/")

## Step 1. Installing and loading Packages

library(FedData)

## Step 2. Downloading BioClim Data

leftlon <- ''
rightlon <- ''
lowerlat <- ''
upperlat <- ''

if (leftlon == ''){leftlon <- -71.45 }
if (rightlon == ''){rightlon <- -71.322200}
if (lowerlat == ''){lowerlat <- 42.400}
if (upperlat == ''){ upperlat <- 42.45}

require(raster)
BClim = getData("worldclim", var="bio", res=2.5, path="")
GspeciesRange = extent(leftlon, rightlon,lowerlat, upperlat)
BClim = crop(BClim, GspeciesRange)
writeRaster(BClim, filename=croppeddata, overwrite=T)
BClim = brick(croppeddata)

## Step 3. Downloading Elevation Data

vepPolygon <- polygon_from_extent(raster::extent(leftlon, rightlon, lowerlat, upperlat),
                                  proj4string="+proj=longlat +ellps=WGS84 +datum=WGS84")

NED <- get_ned(template=vepPolygon,raw.dir='../data/NED/RAW',extraction.dir=
                 '../data/NED/EXTRACTIONS',label="HF",res='1',force.redo=TRUE)

## Step 4. Downloading Species Presence Data

