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
#neClim <- stack("../data/neClim.grd")

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

## Step 6. Making Histograms for each cluster


gsp_bc <-  extract(mintemp.2006, gsp) 
mt.c <- split(gsp_bc, clust)
par(mfrow=c(1,3))
#lapply(mt.c, hist)
hrange <- c(265:280)

pdf('~/Desktop/gENM/results/clust_hist.pdf', bg="black")
par(mfrow=c(3,1))

hist(mt.c$`1`,axes=F, col="mediumpurple1", xlim=c(265,280))
axis(1, col = 'white', col.axis = 'white', col.ticks = 'white',cex.axis = 1.5, font = 1)
axis(2, col = 'white', col.axis = 'white', col.ticks = 'white', cex.axis = 1.5, font =1)

hist(mt.c$`2`, axes= F, col="yellow", xlim=c(265,280), ylim=c(0, 400))
axis(1, col = 'white', col.axis = 'white', col.ticks = 'white',cex.axis = 1.5, font = 1)
axis(2, col = 'white', col.axis = 'white', col.ticks = 'white', cex.axis = 1.5, font =1)

hist(mt.c$`3`, col="turquoise1",axes= F,xlim=c(265,280))
axis(1, col = 'white', col.axis = 'white', col.ticks = 'white',cex.axis = 1.5, font = 1)
axis(2, col = 'white', col.axis = 'white', col.ticks = 'white', cex.axis = 1.5, font=1)


hist(gsp_bc,col="violetred1", axes=F, xlim=c(265,280), ylim=c(0,400))

axis(1, col = 'white', col.axis = 'white', col.ticks = 'white',cex.axis = 1.5, font = 1)
axis(2, col = 'white', col.axis = 'white', col.ticks = 'white', cex.axis = 1.5, font=1)


dev.off()
system('open ../results/clust_hist.pdf')


