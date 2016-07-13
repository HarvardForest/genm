# Anna M. Caldeorn
# Matthew K. Lau
# Harvard Forest
# gENM script
# 07 July 2016

## 0. Installing Required Packages
source("/Users/annacalderon/Desktop/gENM/src/helpers.R")
packs<-c("rgbif","mapproj","mapdata","sp","maptools","dismo","rJava", "rgeos","rgdal", "gdistance", "FedData")
lapply(packs, require, character.only = TRUE)

## 1. Set a working directory and defining file paths                                        
wd <- '/Users/annacalderon/Desktop/gENM/data/' # default is the current working directory
setwd(wd)
path <- ("") #data folder path


if (path == ""){path <- "../data"}
filename <- ("GspeciesBC_2.5.grd")
paste(path,filename, sep="/")
croppeddata<- paste(path,filename, sep="/")

## 2. Limiting Extent 


                #must replace '' with a numeric value

leftlon <- ''
rightlon <- ''
lowerlat <- ''
upperlat <- ''

if (leftlon == ''){leftlon <- -71.45 }
if (rightlon == ''){rightlon <- -71.322200}
if (lowerlat == ''){lowerlat <- 42.400}
if (upperlat == ''){ upperlat <- 42.45}

## 3. Set Seed

seed <- ''

## 4. Importing Species Presence Data

genus <- ''
species <- ''
N <- ''  #number of individuals
if (genus == ''){genus <- 'Aphaenogaster';species <- 'picea'}




## 5. Cluster

## 6. SDM


############################# gDistance Code ################################

vepPolygon <- polygon_from_extent(raster::extent(leftlon, rightlon, lowerlat, upperlat),
                                  proj4string="+proj=longlat +ellps=WGS84 +datum=WGS84")

NED <- get_ned(template=vepPolygon,raw.dir='../data/NED/RAW',extraction.dir=
                 '../data/NED/EXTRACTIONS',label="HF",res='1',force.redo=TRUE)

image(NED,  xlab="longitude", ylab= "latitude")
r <- NED

if (seed == ''){seed <- 123} 
set.seed(seed)

altDiff
hd <- transition(r, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
adj <- adjacent(r, cells=1:ncell(r), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)
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

############################# Clustering Code ##########################################

############################## Species Distribution Model ############################

data(stateMapEnv)

plot(c(leftlon, rightlon), c(lowerlat, upperlat), mar=par("mar"), xlab="longitude",
     ylab="latitude", xaxt="n", yaxt="n", type="n", main="Presence and Absence Points")
rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4], col="lightcyan")
map("state", xlim=c(leftlon, rightlon), ylim=c(lowerlat, upperlat), fill=T, col="honeydew", add=T)

points(Gspecies$lon, Gspecies$lat, col="darkolivegreen4", pch=20, cex=0.5)
axis(1,las=1)
axis(2,las=1)
box()

longrid = seq(leftlon, rightlon,0.05)
latgrid = seq(lowerlat, upperlat,0.05)

subs = c()
for(i in 1:(length(longrid)-1)){
  for(j in 1:(length(latgrid)-1)){
    gridsq = subset(Gspecies, lat > latgrid[j] & lat < latgrid[j+1] & lon > longrid[i] & lon < longrid[i+1])    
    if(dim(gridsq)[1]>0){
      subs = rbind(subs, gridsq[sample(1:dim(gridsq)[1],1 ), ])
    }
  }
}

dim(subs) 

x=circles(subs[,c("lon","lat")], d=50000, lonlat=T)
plot(x@polygons, axes=T, col=rgb(0,0,0,0.1), border=NA, add=T)
bg = spsample(x@polygons, 1000, type='random', iter=1000)
points(bg,col="khaki4",pch=1,cex=0.3)



require(raster)
BClim = getData("worldclim", var="bio", res=2.5, path="")

#crop data
GspeciesRange = extent(leftlon, rightlon,lowerlat, upperlat)
BClim = crop(BClim, GspeciesRange)
writeRaster(BClim, filename=croppeddata, overwrite=T)
BClim = brick(croppeddata)

Gspecies_bc = extract(BClim, subs[,c("lon","lat")]) 
bg_bc = extract(BClim, bg) 
Gspecies_bc = data.frame(lon=subs$lon, lat=subs$lat, Gspecies_bc)

bgpoints = bg@coords
colnames(bgpoints) = c("lon","lat")
bg_bc = data.frame(cbind(bgpoints,bg_bc))
length(which(is.na(bg_bc$bio1))) 
bg_bc = bg_bc[!is.na(bg_bc$bio1), ] 
group_p = kfold(Gspecies_bc, 1) 
group_a = kfold(bg_bc, 1) 

test=3
train_p = Gspecies_bc[group_p!=test, c("lon","lat")]
train_a = bg_bc[group_a!=test, c("lon","lat")]
test_p = Gspecies_bc[group_p==test, c("lon","lat")]
test_a = bg_bc[group_a==test, c("lon","lat")]
me = maxent(BClim, p=train_p, a=train_a)
e = evaluate(test_p, test_a, me, BClim)
e
pred_me = predict(me, BClim) 

plot(pred_me, 1, cex=0.5, legend=T, mar=par("mar"), xaxt="n", yaxt="n", main="Predicted Species Distribution")
map("state", xlim=c(leftlon,rightlon), ylim=c(lowerlat,upperlat), fill=F, col="black", add=T)
points(bg,col="snow",pch=1,cex=0.2)
points(Gspecies$lon, Gspecies$lat, col="darkgreen", pch=20, cex=0.5)

# add axes
axis(1,las=1)
axis(2,las=1)
box()

#source("/Users/annacalderon/Desktop/gENM/src/RDataTracker.R")
#ddg.run("/Users/annacalderon/Desktop/gENM/src/BClimBug.R")