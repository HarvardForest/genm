packs <- c("gdistance", "fossil" , "igraph", "rgbif","mapproj","mapdata","sp",
           "maptools","dismo","rJava","rgdal", "rgeos", "raster", "FedData")

lapply(packs[!(packs %in% installed.packages()[,'Package'])],install.packages)
all(unlist(lapply(packs, require, character.only = TRUE,quietly=TRUE)))

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
m <- function(scd){(1-(scd/max(scd)))}

###

### gClust - models genetic clusters based on landscape features using circuit 
### theory based landscape resistance. Returns a list of observation matrices 
### grouped by the 'genetic' clusters.

### MKLau and ACalderon - Summer 2016

### x = Distribution data for a given organism using lon and lat coordinates.
### p = Environmental 
### N = effective population size

gClust <- function(x='coordinates',p='predictor',N=1){
  # Conductance matrix is used to produce 
  # an initial matrix of "flow" between observations
  hd <- transition(p, altDiff, 8, symm=FALSE)
  slope <- geoCorrection(hd)
  adj <- adjacent(p, cells=1:ncell(p), pairs=TRUE, directions=8)
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
  return(gc)
}

######

### Creates an environmental niche model
### based on MaxEnt alogrithims
### to predict habitat suitability.

### ACalderon and MKLau - 15July2016

### x = Distribution data for a given organism using lon and lat coordinates.
### p = Environmental predictor



ENM <- function(x="coordinates", p="predictors"){
  
  set.seed(123)
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

########

### Applies an environmental niche model
### to clusters of a population as well 
### as to the entire species, and predicts
### habitat suitability for each cluster. 

### ACalderon and MKLau - 15July2016

### x = Distribution data for a given organism using lon and lat coordinates.




gENM <- function(x='coordinates', clust='gen clusters'){
  df.gspecies <- data.frame(x)
  groups <-  split(df.gspecies, clust)
  
  analysis <- (lapply(groups, ENM, p=neClim))
  enm.all <- list(ENM(do.call(rbind,groups),neClim))
  out <- append(enm.all, analysis)
 
}

gAnalysis <- function(x="gENM output"){

jpeg(filename = "/Users/annacalderon/Desktop/Rplot.jpeg", width = 1700, height = 1700,
     units = "px", pointsize = 35, quality = 90,
     bg="white")
  
  par(mfrow=c(3,2), oma=rep(0,4),omi=rep(0,4), bty = 'n')
  for(i in 1: length(x)){zoom(x[[i]]$pred, ext=extent(-73.70833, -66.95833, 41, 47.45833),
         xaxt='n', yaxt='n', new=FALSE, asp=1)}
  
dev.off()


  }


