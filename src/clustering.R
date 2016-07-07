### Looking into algorithms to cluster Fst based
### matrices to return genetic clusters.
### MKLau - 07July2016


Fst <- matrix(runif(100),10)
diag(Fst) <- 0

library(fclust)

library(fossil)

Fst.mst <- Fst
Fst.dm <- dino.mst(1-Fst)
Fst.mst[Fst.dm == 0] <- 0
plot(graph.adjacency(Fst.mst,weighted=TRUE,mode='undirected'))



library(igraph)

Fst.ig <- graph.adjacency(Fst,weighted=TRUE,mode='undirected')
plot(Fst.ig)
min_cut(Fst.ig,2,9)

fastgreedy.community(Fst.ig)
spinglass.community(Fst.ig)

