source('helpers.R')

gDensCurv <- function(x='coordinates',  p="mintemp.2006", 
mintemp.2006  <- raster("../data/01_01_2006.tiff")
mintemp.2050 <- raster("../data/01_01_2050.tiff")
mintemp.2099 <- raster("../data/01_01_2099.tiff")
gspecies <-read.csv("../data/RICTMEdukesnantucket.csv")

mt.l <- list(mintemp.2006,mintemp.2050,mintemp.2099)
gc <- gClust(gspecies,mt.l[[1]])
mt.enm <- lapply(mt.l,gENM,x=gspecies,clust=gc)
mt.2006 <- extract(mintemp.2006,gspecies)
mt.c <- split(mt.2006,gc)
mt.mc <- melt(mt.c)
colnames(mt.mc) <- c('MinTemp','Cluster')

pdf('~/Desktop/gENM/results/clust_dens.pdf', bg="black")
mt.mcplot <- ggplot(mt.mc, aes(MinTemp,colour = Cluster, fill=Cluster)) +
    geom_density(alpha = 0.7) 
fill <- mt.mcplot+scale_color_manual(values=c("turquoise2", "orchid4", "lightcoral"))
fill+scale_fill_manual(values=c("turquoise2", "orchid4", "lightcoral"))
dev.off()
system('open ../results/clust_dens.pdf')

