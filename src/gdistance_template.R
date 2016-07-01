# install.packages("gdistance")
# install.packages('FedData')
library("gdistance")
library(FedData)



## 1. working directory                                        
wd <- '/Users/annacalderon/Desktop/gENM/data' # default is the current working directory
setwd(wd)

## 2. Limiting Extent
#must replace '' with a numeric value

xminimum <- ''
xmaximum <- ''
yminimum <- ''
ymaximum <- ''

## 3. Set Seed

seed <- ''

## 4. Importing Presence Data

genus <- ''
species <- ''

#######################################################################################
if (xminimum == ''){xminimum <- -72.237179 }
if (xmaximum == ''){xmaximum <- -72.132233}
if (yminimum == ''){yminimum <- 42.499695}
if (ymaximum == ''){ ymaximum <- 42.536449}

vepPolygon <- polygon_from_extent(raster::extent(xminimum, xmaximum, yminimum, ymaximum),
                                  proj4string="+proj=longlat +ellps=WGS84 +datum=WGS84")

NED <- get_ned(template=vepPolygon,raw.dir='../data/NED/RAW',extraction.dir=
                 '../data/NED/EXTRACTIONS',label="HF",res='1',force.redo=TRUE)

image(NED,  xlab="longitude", ylab= "latitude")
r <- NED

if (seed == ''){seed <- 123} 
set.seed(seed)

altDiff <- function(x){x[2] - x[1]}
hd <- transition(r, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
adj <- adjacent(r, cells=1:ncell(r), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)


#defining two points on the graph
xlims <- as.vector(r@extent)[1:2]
ylims <- as.vector(r@extent)[3:4]
A <- c(runif(1,xlims[1],xlims[2]),runif(1,ylims[1],ylims[2]))
B <- c(runif(1,xlims[1],xlims[2]),runif(1,ylims[1],ylims[2]))
AtoB <- shortestPath(Conductance, A, B, output="SpatialLines")
BtoA <- shortestPath(Conductance, B, A, output="SpatialLines")
plot(r, xlab="x coordinate (m)", ylab="y coordinate (m)",
     legend.lab="Altitude (masl)")
lines(AtoB, col="red", lwd=2)
lines(BtoA, col="blue")
text(A[1] - 10, A[2] - 10, "A")
text(B[1] + 10, B[2] + 10, "B")

#Calculating Distances
packs<-c("rgbif","mapproj","mapdata","sp","maptools","dismo","rJava","rgdal")
lapply(packs, require, character.only = TRUE)

if (genus == ''){genus <- 'Aphaenogaster';species <- 'picea'}
rawdata <- gbif(genus = genus, species = species) 
na.omit(rawdata[,c('lat','lon')])
Gspecies <- na.omit(rawdata[,c('lat','lon')])


points(Gspecies, col="snow", pch=20, cex=.75)
costDistance(Conductance, Gspecies)
cd <- costDistance(Conductance, Gspecies)
cd/max(cd)
Bprob <- cd/max(cd) #the probability that an individual will encounter a barrier
1-Bprob
m <- 1-Bprob #the probability that individuals will migrate 
1/(1+4*m)
Fst <- 1/(1+4*m)
diag(Fst) <- 0




