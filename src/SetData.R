#Anna M Calderon
#Matthew K Lau
#Harvard Forest
#gENM-Data Set Up
#Part 0
#8 July 2016

## Step 0. Set a working directory and File Paths

wd <- '/Users/annacalderon/Desktop/gENM/src'
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

#gspecies <- read.csv('../data/gspecies.csv')

if (identical(colnames(gspecies),c( "lat", "lon"))){gspecies <- gspecies[,c('lon','lat')]}
if (is.matrix(gspecies) == FALSE){gspecies <- data.matrix(gspecies)}


### Getting state data
gsp <- gspecies
us <- getData('GADM',country='usa',level=1)
ne <- c('Connecticut','Maine','Massachusetts','New Hampshire','Rhode Island','Vermont')
keep <- c('Maine','Connecticut','Rhode Island')
dont <- c('Massachusetts','New Hampshire','Vermont') 
newengland <- us[us$NAME_1 %in% ne,]
ne.keep <- newengland[newengland$NAME_1 %in% keep,]

# pkp <- logical()
# for (i in 1:nrow(gsp)){
#     print(i/nrow(gsp))
#     pkp[i] <- inMap(gsp[i,],ne.keep) 
    #I think this doesn't work because gContains is expecting
    # spatialpoints as y. I converted gsp to a datafram and then coverted into SpatialPoints
    
}

nan.mart <- c((-70.62917, -70.63000, -70.63750, -70.73750, -70.79583, -70.65000), 
              (41.35771,41.37000, 41.36604, 41.36604, 41.31604, 41.45000))         
             # what are the values for the points in nantucket and martha's vineyard?
             
             
gsp.k <- gsp[pkp | ((1:nrow(gsp)) %in% nant.mart),]

plot(ne.keep)
points(gsp.k,pch=20,cex=0.5,col='pink')
text(gsp.k,labels=(1:nrow(gsp.k)),cex=0.5)
