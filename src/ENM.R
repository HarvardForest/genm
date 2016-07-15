#Anna M Calderon
#Matthew K Lau
#Harvard Forest
#gENM-ENM Modeling
#Part 2
#11 July 2016

clust <- gClust(x=gspecies, y=neClim$bio1)
set.seed(123)
ENM(x=clust$`2`)


