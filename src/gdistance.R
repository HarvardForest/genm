#install.packages("gdistance")
#install.packages("FedData")
#library("FedData")
# library("gdistance")


set.seed(123)
r <- raster(ncol=3,nrow=3)
r[] <- 1:ncell(r)
r
plot(r, main="r", xlab="Longitude (degrees)", ylab="Latitude (degrees)")
text(r)

#4. Transition Classes
r[] <- 1
tr1 <- transition(r, transitionFunction=mean, directions=8)
tr1

#Creating an Asymmetric Matrix

r[] <- runif(9)
ncf <- function(x) max(x) - x[1] + x[2]
tr2 <- transition(r, ncf, 4, symm=FALSE)
tr2

#The class dgCMatrix holds an asymmetric matrix.

tr3 <- tr1*tr2
tr3 <- tr1+tr2
tr3 <- tr1*3
tr3 <- sqrt(tr1)

tr3[cbind(1:9, 1:9)] <- tr2[cbind(1:9, 1:9)]
tr3[1:9, 1:9] <- tr2[1:9, 1:9]
tr3[1:5, 1:5]
image(transitionMatrix(tr1))

############  Hiking around Maunga Whau ##############

#First, we read in the altitude data for the volcano.
r <- raster(system.file("external/maungawhau.grd", package="gdistance"))
#finding the slope

# altDiff <- function(x){x[2] - x[1]}
# hd <- transition(r, altDiff, 8, symm=FALSE)
# slope <- geoCorrection(hd) # use the geoCorrection function to divide by the distance between cells.
# #adjacent cells have a slope ≠ 0; this f() restricts calculations to adjacent cells
# adj <- adjacent(r, cells=1:ncell(r), pairs=TRUE, directions=8)
# speed <- slope
# speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
# Conductance <- geoCorrection(speed)

## Retrieve a Conductance matrix:DDDDD

Conductance[1:87, 1:61]
image(Conductance[1:87, 1:61]) #I think darker numbers equal highest conductance


#defining two points on the graph
A <- c(2667670, 6479000)
B <- c(2667800, 6479400)
AtoB <- shortestPath(Conductance, A, B, output="SpatialLines")
BtoA <- shortestPath(Conductance, B, A, output="SpatialLines")


plot(r, xlab="x coordinate (m)", ylab="y coordinate (m)",
    legend.lab="Altitude (masl)", xlim = c(2667200, 2668000))
lines(AtoB, col="red", lwd=2)
lines(BtoA, col="blue")
text(A[1] - 10, A[2] - 10, "A")
text(B[1] + 10, B[2] + 10, "B")

#Calculating Distances

sP <- cbind(c(2667670,2667800 ), c(6479000,6479400))
costDistance(Conductance, sP)
rSPDistance(Conductance, sP, sP, theta=1e-12, totalNet = "total")



################ Geographical Genetics #########################

Europe <- raster(system.file("external/Europe.grd", package="gdistance"))
Europe[is.na(Europe)] <- 0
data(genDist)
data(popCoord)
pC <- as.matrix(popCoord[c("x","y")])

geoDist <- pointDistance(pC, longlat=TRUE)
geoDist <- as.dist(geoDist)
Europe <- aggregate(Europe,3)
tr <- transition(Europe, mean, directions=8)
trC <- geoCorrection(tr, "c", scl=TRUE)
trR <- geoCorrection(tr, "r", scl=TRUE)
cosDist <- costDistance(trC,pC)
resDist <- commuteDistance(trR, pC)
cor(genDist,geoDist)

cor(genDist,cosDist)
cor(genDist,resDist)

origin <- unlist(popCoord[22,c("x","y")])
pI <- pathInc(trC, origin=origin, from=pC,functions=list(overlap))
cor(genDist,pI[[1]])


