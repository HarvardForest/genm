packs<-c("gdistance", "fossil" , "igraph", "rgbif","mapproj","mapdata","sp","maptools","dismo","rJava","rgdal", "rgeos")
unlist(lapply(packs, require, character.only = TRUE))

### Creates a symmetric matrix comprised of 
### the sum of the upper and lower triangles.
### MKLau - 06July2016

symSum <- function(x='matrix',zero.diag=TRUE){
    if (zero.diag == TRUE){diag(x) <- 0}
    sum.lu <- t(x)[lower.tri(x)]  + x[lower.tri(x)]
    x[lower.tri(x)] <- sum.lu
    x <- t(x)
    x[lower.tri(x)] <- sum.lu
    if (isSymmetric(x)){x}else{
        warning('Output matrix is not symmetirc.')
    }
}

### 
altDiff <- function(x){x[2] - x[1]}

###
NED.plot <- function(NED){plot(NED, xlab="x coordinate (m)", ylab="y coordinate (m)",
                               legend.lab="Altitude (masl)")}
p.points <- function(gspecies){points(gspecies, col="black", pch=20, cex=.30)}

###
m <- function(scd){(1-(scd/max(scd)))}

###



### gClust - models genetic clusters based on landscape features using circuit 
### theory based landscape resistance. Returns a list of observation matrices 
### grouped by the 'genetic' clusters.

### MKLau and ACalderon - Summer 2016

### x = Distribution data for a given organism using lon and lat coordinates.
### y = Environmental 
### N = effective population size

gClust <- function(x='distribution',y='environment',N=1){
                                        # Conductance matrix is used to produce 
                                        # an initial matrix of "flow" between observations
hd <- transition(y, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
adj <- adjacent(y, cells=1:ncell(y), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)
cd <- costDistance(Conductance, x)
                                        # Summing over assymmetry
scd <- symSum(cd)
                                        # Re-scaling using basic population
                                        # demography to approximate migration.
                                        # N = effective population size, by 
                                        # default this is set to one for mathematical
                                        # convenience. The diagonal is set to zero 
                                        # because each observation should be 
                                        # genetically identical to itself.
                                        # See Conner & Hartl pg. 84
Fst <- 0.20*(1/(1+4*N*m(scd))) 
diag(Fst) <- 0
                                        # Invert Fst so that it is a similarity rather
                                        # than a dissimilarity matrix and find
                                        # the minimally connected graph to focus 
                                        # on the most important connections.
Fst.g <- 1-Fst
diag(Fst.g) <- 0
Fst.mg <- dino.mst(Fst.g)
Fst.ig <- graph.adjacency(Fst.mg,weighted=TRUE,mode='undirected')
                                        # Determine clusters using a graph theoretic 
                                        # module/cluster detection algorithm.
fg.mP <- fastgreedy.community(Fst.ig)
gc <- fg.mP$membership
names(gc) <- rownames(Fst)
                                        # Output observations in a format for the 
                                        # gENM. 
gObs <- split(x,gc)
gObs <- lapply(gObs,matrix,ncol=2)
for (i in 1:length(gObs)){colnames(gObs[[i]]) <- c("lon","lat")}
return(gObs)
}

######

ENM <- function(x="coordinates", p="predictors"){
  
circ=circles(x, d=50000, lonlat=T)
random <- spsample(circ@polygons, 1000, type='random', iter=1000)
                                    # Makes circles with a 5K radius of each
                                    # point and adds 1000 randomized points.
  
clust_bc <-  extract(p, x) 
clust_bc <-  data.frame(cbind(x,clust_bc))
                                    # Extracts the climate variables which 
                                    # which correspond to each presence point
                                    # of a cluster. Binds climate variables
                                    # with their resepctive coordinates, 
                                    # Then turns that matrix into a list. 
  
  random_bc <- extract(p, random) 
  random  <- random@coords
  colnames(random) <- c("lon","lat")
                                    # Extracts the climate variables which 
                                    # which correspond to each random point
                                    # of a cluster. Coordinates of random are 
                                    # saved as a matrix. 
  
random_bc <-  data.frame(cbind(random,random_bc))
random_bc  <-  random_bc[!is.na(random_bc[,"bio1"]), ] 
                                    # Binds the random point coordinates and 
                                    # and the extracted climate variables 
                                    # that correspond to those random points
                                    # in order to create a list. Also removes
                                    # any NAs in the list. 
                                    
me <- maxent(p, clust_bc[,c("lon", "lat")], random_bc[,c("lon", "lat")])
e <- evaluate(clust_bc[,c("lon", "lat")], random_bc[,c("lon", "lat")], me, p)
pred_me <- predict(me, p) 
                                    # Build a "MaxEnt" (Maximum Entropy) species
                                    # distribution model based on predictors 
                                    # and produces a model that is used by 
                                    # the predic() fucntion to predict
                                    # the suitability of other locations.
out <- list(eval = e, pred = pred_me, model = me)
return(out)
}
############
g
