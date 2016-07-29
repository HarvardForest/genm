source('helpers.R')

mintemp.2006  <- raster("../data/01_01_2006.tiff")
mintemp.2050 <- raster("../data/01_01_2050.tiff")
mintemp.2099 <- raster("../data/01_01_2099.tiff")

gspecies <-read.csv("../data/RICTMEdukesnantucket.csv")
if (is.matrix(gspecies) == FALSE){gspecies <- data.matrix(gspecies)}

mt.l <- list(mintemp.2006,mintemp.2050,mintemp.2099)
gc <- gClust(gspecies,mt.l[[1]])
mt.enm <- lapply(mt.l,gENM,x=gspecies,clust=gc)

mt.2006 <- extract(mintemp.2006,gspecies)
mt.c <- split(mt.2006,gc)

mt.mc <- melt(mt.c)
colnames(mt.mc) <- c('MinTemp','Cluster')

ggplot(mt.mc, aes(MinTemp, fill = Cluster, colour = Cluster)) +
    geom_density(alpha = 0.1) 


pdf('~/Desktop/minTcompare.pdf',height=8,width=12)
par(mfrow=c(2,3))
plot(mintemp.2050);plot(mintemp.2006);plot(mintemp.2099)
plot(mintemp.2006-mintemp.2006);plot(mintemp.2006-mintemp.2050);plot(mintemp.2006-mintemp.2099)
dev.off()
system('open ~/Desktop/minTcompare.pdf')
