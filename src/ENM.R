#Anna M Calderon
#Matthew K Lau
#Harvard Forest
#gENM-ENM Modeling
#Part 2
#11 July 2016

## Step 1. Installing and loading Packages

packs<-c("rgbif","mapproj","mapdata","sp","maptools","dismo","rJava","rgdal", "rgeos")
unlist(lapply(packs, require, character.only = TRUE))

## Step 2. Limiting Extent

leftlon <- -73.725 
rightlon <- -66.958333333 
lowerlat <- 40.995206022
upperlat <- 47.453539355 

########################################################################################
# create sequences of latitude and longitude values to define the grid
longrid = seq(leftlon,rightlon,0.05)
latgrid = seq(lowerlat, upperlat,0.05)
lat <- gObs$`1`[,"lat"]
lon <- gObs$`1`[,"lon"]
subs = c()
for(i in 1:(length(longrid)-1)){
  for(j in 1:(length(latgrid)-1)){
    gridsq = subset(gObs$`1`, lat > latgrid[j] & lat < latgrid[j+1] & lon > longrid[i] & lon < longrid[i+1])    
    if(dim(gridsq)[1]>0){
      subs = rbind(subs, gridsq[sample(1:dim(gridsq)[1],1 ), ])
    }
  }
}


dim(subs) 

x=circles(subs[,c("lon","lat")], d=50000, lonlat=T)
random <- spsample(x@polygons, 1000, type='random', iter=1000)




gspecies_bc = extract(neClim, subs[,c("lon","lat")]) 
random_bc = extract(neClim, random) 
gspecies_bc = data.frame(lon=subs[,"lon"], lat=subs[,"lat"], gspecies_bc)

randompnts = random@coords
colnames(randompnts) = c("lon","lat")
random_bc = data.frame(cbind(randompnts,random_bc))
length(which(is.na(random_bc$bio1))) 
random_bc = random_bc[!is.na(random_bc$bio1), ] 

####################################  BUILDIG YOUR SDM  ############################################

me = maxent(BClim, Gspecies_bc[,c("lon", "lat")], random_bc[,c("lon", "lat")])
e = evaluate(Gspecies_bc[,c("lon", "lat")], random_bc[,c("lon", "lat")], me, BClim)
pred_me = predict(me, BClim) 




#source("/Users/annacalderon/Desktop/gENM/src/RDataTracker.R")
#ddg.run("/Users/annacalderon/Desktop/gENM/src/BClimBug.R")
