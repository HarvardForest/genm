# install.packages("gdistance")
# install.packages('FedData')
source("helpers.R")
library(gdistance)
library(FedData)



## 1. working directory                                        
wd <- '/Users/annacalderon/Desktop/gENM/data' # default is the current working directory
setwd(wd)

## 2. Limiting Extent of Elevation Data
#must replace '' with a numeric value

leftlon <- -91
rightlon <- -88
lowerlat <- 37
uppperlat <- 40

## 3. Set Seed

seed <- ''

## 4. Importing Species Presence Data

genus <- ''
species <- ''

N <- '' #number of individuals

## 5. Cluster

## 6. SDM

#######################################################################################
if (leftlon == ''){leftlon <- -71.45 }
if (rightlon == ''){rightlon <- -71.322200}
if (lowerlat == ''){lowerlat <- 42.400}
if (uppperlat == ''){ uppperlat <- 42.45}

vepPolygon <- polygon_from_extent(raster::extent(leftlon, rightlon, lowerlat, uppperlat),
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


#defining  points on the graph


####  How would generalize this for N points?
# n <- 10
# for (i in 1:n){plot(i)}

p1  <- c(-71.33917, 42.43222)
p2  <- c(-71.39083, 42.42972)
p3  <- c(-71.39611, 42.42250)

# xlims <- as.vector(r@extent)[1:2]
# ylims <- as.vector(r@extent)[3:4]
# A <- c(runif(1,xlims[1],xlims[2]),runif(1,ylims[1],ylims[2]))
# B <- c(runif(1,xlims[1],xlims[2]),runif(1,ylims[1],ylims[2]))
p1top2 <- shortestPath(Conductance, p1, p2, output="SpatialLines")
p2top1 <- shortestPath(Conductance, p2, p1, output="SpatialLines")
p1top3 <- shortestPath(Conductance, p1, p3, output="SpatialLines")
p3top1 <- shortestPath(Conductance, p3, p1, output="SpatialLines")
p2top3 <- shortestPath(Conductance, p2, p3, output="SpatialLines")
p3top2 <- shortestPath(Conductance, p3, p2, output="SpatialLines")

plot(r, xlab="x coordinate (m)", ylab="y coordinate (m)",
     legend.lab="Altitude (masl)")
lines(p1top2, col="navy", lwd=3)
lines(p2top1, col="aliceblue", lwd=1)
lines(p1top3, col="red4", lwd=3)
lines(p3top1, col="indianred1", lwd=1)
lines(p2top3, col="forestgreen", lwd=3)
lines(p3top2, col="greenyellow", lwd=0.7)
# text(A[1] - 10, A[2] - 10, "A")
# text(B[1] + 10, B[2] + 10, "B")

#Calculating Distances


if (genus == ''){genus <- 'Aphaenogaster';species <- 'picea'}
rawdata <- gbif(genus = genus, species = species) 
Gspecies <- na.omit(rawdata[,c('lon','lat')])


ylimit <- subset(Gspecies, lat >= 42.2300 & lat <= 42.46)
xlimit <- subset(ylimit, lon >= -71.50 & lon <= -71.322200)
if (identical(colnames(xlimit),c("lat", "lon"))){xlimit <- xlimit[,c('lon','lat')]}
if (is.matrix(xlimit) == FALSE){data <- data.matrix(xlimit)}

points(xlimit, col="black", pch=20, cex=.30)

text(x= -71.3450, y=42.43222, "1",col="black", pch=20, cex=.40 )
text(x= -71.39500,y=42.42972, "2", col="black", pch=20, cex=.40)
text(x= -71.398000,y= 42.41890, "3", col="black", pch=20, cex=.40)


#### check if xlimit is a matrix -> is.matrix(xlimit)
#### to check if a matrix is symmetric use: isSymmetric.matrix()


cd <- costDistance(Conductance, data)
scd <- symSum(cd)
Bprob <- scd/max(scd) #the probability that an individual will encounter a barrier
m <- 1-Bprob #the probability that individuals will migrate 
if (N == ''){N <- 1}
Fst <- 0.20*(1/(1+4*N*m)) #see Conner & Hartle pg. 84






### Clustering

### ENM

