install.packages("gdistance")
install.packages('FedData')
library("gdistance")
library(FedData)

vepPolygon <- polygon_from_extent(raster::extent(672800,740000,4102000,4170000),proj4string="+proj=utm +datum=NAD83 +zone=12")
NED <- get_ned(template=vepPolygon,label="VEPIIN")
NED <- projectRaster(NED,crs="+proj=utm +datum=NAD83 +zone=12")


set.seed(123)
r <- NED

altDiff <- function(x){x[2] - x[1]}

hd <- transition(r, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
adj <- adjacent(r, cells=1:ncell(r), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)

## Retrieve a Conductance matrix:DDDDD

#Conductance[1:3, 1:3]
image(Conductance[1:500, 1:500]) #I think darker numbers equal highest conductance

#defining two points on the graph
A <- c(672800, 4102000)
B <- c(673000, 4103000)

AtoB <- shortestPath(Conductance, A, B, output="SpatialLines")
BtoA <- shortestPath(Conductance, B, A, output="SpatialLines")


plot(r, xlab="x coordinate (m)", ylab="y coordinate (m)",
     legend.lab="Altitude (masl)")
lines(AtoB, col="red", lwd=2)
lines(BtoA, col="blue")
text(A[1] - 10, A[2] - 10, "A")
text(B[1] + 10, B[2] + 10, "B")

#Calculating Distances

sP <- cbind(c(,), c(,))
costDistance(Conductance, sP)
rSPDistance(Conductance, sP, sP, theta=1e-12, totalNet = "total")
