#Anna M Calderon
#Matthew K Lau
#Harvard Forest
#gENM-Data Set Up
#Part 0
#8 July 2016

## Step 0. Set a working directory and File Paths

wd <- '../src'
setwd(wd)

# path <- ("") 
# if (path == ""){path <- "../data"}
# filename <- ("GspeciesBC_2.5.grd")
# croppeddata<- paste(path,filename, sep="/")

## Step 1. Installing and loading Packages
require(raster)
library(FedData)


## Step 2. Downloading BioClim Data

neClim <- stack("../data/neClim.grd")

# BClim = getData("worldclim", var="bio", res=2.5, path="")
# GspeciesRange = extent(leftlon, rightlon,lowerlat, upperlat)
# BClim = crop(BClim, GspeciesRange)
# writeRaster(BClim, filename=croppeddata, overwrite=T)
# BClim = brick(croppeddata)


## Step 3. Downloading Elevation Data

## Getting climate change projections
library(maptools)
vepPolygon <- polygon_from_extent(raster::extent(xmin, xmax, ymin, ymax),
                                  proj4string="+proj=longlat +ellps=WGS84 +datum=WGS84")
IDs <- sapply(slot(vepPolygon, "polygons"), function(x) slot(x, "ID"))
df <- data.frame(rep(0, length(IDs)), row.names=IDs)
SPDFxx <- SpatialPolygonsDataFrame(vepPolygon, df)
#tf <- tempfile()
#writePolyShape(SPDFxx, tf)
#getinfo.shape(tf)

library(rgdal)
## shape <- readOGR('../data/neExtent',layer='neExtent')
writeOGR(SPDFxx,dsn='../data/neExtent',layer='neExtent',driver='ESRI Shapefile',overwrite_layer=TRUE)


## Step 4. Downloading Species Presence Data
gspecies <- ''

prespoints <- read.csv('http://harvardforest.fas.harvard.edu/data/p14/hf147/hf147-13-antData_use4R_snappedToClim.csv')
if (gspecies == ''){gspecies <- "aphrud"}
colnames(prespoints) = c("spcode", "lon","lat")
gspecies <- prespoints[grep(gspecies,as.character(prespoints$spcode)),]
gspecies$spcode <- NULL

if (identical(colnames(gspecies),c( "lat", "lon"))){gspecies <- gspecies[,c('lon','lat')]}
if (is.matrix(gspecies) == FALSE){gspecies <- data.matrix(gspecies)}



