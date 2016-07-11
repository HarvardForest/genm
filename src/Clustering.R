#Anna M Calderon
#Matthew K Lau
#Harvard Forest
#gENM-Clustering
#Part 1
#8 July 2016

## Step 1. Installing and loading Packages

#install.packages("fossil")
library(gdistance)
source("../src/helpers.R")
library(fossil)
library(igraph)

## Step 2. Setting Seed/N

seed <- ''
N <- ''

###########################################################################
if (seed == ''){seed <- 123} 
set.seed(seed)

altDiff 
hd <- transition(NED, altDiff, 8, symm=FALSE)
slope <- geoCorrection(hd)
adj <- adjacent(NED, cells=1:ncell(NED), pairs=TRUE, directions=8)
speed <- slope
speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
Conductance <- geoCorrection(speed)

NED.plot(NED = NED)
p.points(gspecies = gspecies)


cd <- costDistance(Conductance, gspecies)
scd <- symSum(cd)
m(scd)
if (N == ''){N <- 1}
Fst <- 0.20*(1/(1+4*N*m(scd))) #see Conner & Hartle pg. 84



## returns a minimally connected graph
Fst.mst <- Fst
Fst.dm <- dino.mst(1-Fst)
Fst.mst[Fst.dm == 0] <- 0 
Fst.mstP <- Fst.mst
Fst.mstP[Fst.mstP != 0] <- (1-Fst.mst[Fst.mst != 0])

Fst.ig <- graph.adjacency(Fst,weighted=TRUE,mode='undirected')
Fstm.ig <- graph.adjacency(Fst.mst,weighted=TRUE,mode='undirected')
Fst.igP <- graph.adjacency((1-Fst),weighted=TRUE,mode='undirected')
Fstm.igP <- graph.adjacency(Fst.mstP,weighted=TRUE,mode='undirected')

par(mfrow=c(2,2))
plot(Fst.ig)
plot(Fstm.ig)
plot(Fst.igP)
plot(Fstm.igP)

fastgreedy.community(Fst.igP)
fastgreedy.community(Fstm.ig)
fg.mP <- fastgreedy.community(Fstm.igP)

set.seed(123)
spinglass.community(Fst.igP)
spinglass.community(Fstm.ig)
sg.mP <- spinglass.community(Fstm.igP)

par(mfrow=c(1,2))
plot(Fstm.igP,vertex.color=
       rainbow(max(fg.mP$membership))[fg.mP$membership])
plot(Fstm.igP,vertex.color=
       rainbow(max(sg.mP$membership))[sg.mP$membership])

gclust <- fg.mP$membership
names(gclust) <- rownames(Fst.mst)

gclust

### ENM

gObs <- split(aphrud,gclust)
Veiw(gObs)
