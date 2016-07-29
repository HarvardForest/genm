source('helpers.R')

gDensCurv <- function(x='coordinates',  p="mintemp.2006"){
                
gc <- gClust(x,p)
                      mt.2006 <- extract(p,x)
                      mt.c <- split(mt.2006,gc)
                      do.call(rbind,lapply(mt.c,range))
                      mt.mc <- melt(mt.c)
                      colnames(mt.mc) <- c('MinTemp','Cluster')
                      
jpeg('~/Desktop/gENM/results/clust_dens.jpeg', bg="black")
                      set.seed(49)
                      mt.mcplot <- ggplot(mt.mc, aes(MinTemp,colour = Cluster, fill=Cluster)) +
                        geom_density(alpha = 0.7) +
                        geom_vline(xintercept=c(256.9,261.2,260.8, 263.9,250.8, 256.7), linetype="dotted", colour = "blue")
             
                      
        fill <- mt.mcplot+scale_color_manual(values=c("greenyellow", "pink2", "lightskyblue1"))+
                      theme(panel.background = element_rect(fill = "gray8"),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        legend.position = "none",axis.line = element_line(colour = "white"),
                        axis.title = element_text(size = 20, color = "white"),
                        axis.ticks = element_line(colour = "white"),
                        axis.text = element_text(colour = "white"))
                      
       fill+scale_fill_manual(values=c("chartreuse", "lightcoral", "steelblue1")) 
                     
                      
                      dev.off()
                      system('open ../results/clust_dens.jpeg')}
                      
                       
                      