
jpeg(filename = "../results/gENM.jpeg", width = 1700, height = 1700,
     units = "px", pointsize = 35, quality = 90,
     bg="black")
par(oma=rep(0,4),omi=rep(0,4), bty = 'n',mar=rep(0.01,4),mai=rep(0,4))
plot(out$`2`[["pred"]])
yellow_blue <- c("black", colorRampPalette(c("black", "snow"))(10))
plot(out$`2`[["pred"]], axes=F, zlim=c(0, 1), col=yellow_blue,legend=T, box=F)
#points(gsp, col="violetred1", cex=1.3,pch =19) 

# df.gsp <- data.frame(gsp)
# groups <-  split(df.gsp, clust)
# 
# points(groups$`1`, col="mediumpurple1", cex=1.3,pch =19) 
# points(groups$`2`, col="yellow", cex=1.3,pch =19)
# points(groups$`3`, col="turquoise1", cex=1.3,pch =19)

## Step 6. Making Histograms for each cluster


gsp_bc <-  extract(mintemp.2006, gsp) 
mt.c <- split(gsp_bc, clust)
par(mfrow=c(1,3))
#lapply(mt.c, hist)
hrange <- c(265:280)

pdf('~/Desktop/gENM/results/clust_hist.pdf', bg="black")
par(mfrow=c(3,1))

hist(mt.c$`1`,axes=F, col="mediumpurple1", xlim=c(265,280))
axis(1, col = 'white', col.axis = 'white', col.ticks = 'white',cex.axis = 1.5, font = 1)
axis(2, col = 'white', col.axis = 'white', col.ticks = 'white', cex.axis = 1.5, font =1)

hist(mt.c$`2`, axes= F, col="yellow", xlim=c(265,280), ylim=c(0, 400))
axis(1, col = 'white', col.axis = 'white', col.ticks = 'white',cex.axis = 1.5, font = 1)
axis(2, col = 'white', col.axis = 'white', col.ticks = 'white', cex.axis = 1.5, font =1)

hist(mt.c$`3`, col="turquoise1",axes= F,xlim=c(265,280))
axis(1, col = 'white', col.axis = 'white', col.ticks = 'white',cex.axis = 1.5, font = 1)
axis(2, col = 'white', col.axis = 'white', col.ticks = 'white', cex.axis = 1.5, font=1)


hist(gsp_bc,col="violetred1", axes=F, xlim=c(265,280), ylim=c(0,400))

axis(1, col = 'white', col.axis = 'white', col.ticks = 'white',cex.axis = 1.5, font = 1)
axis(2, col = 'white', col.axis = 'white', col.ticks = 'white', cex.axis = 1.5, font=1)


dev.off()
system('open ../results/clust_hist.pdf')





dev.off()



