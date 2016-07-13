
## Plotting ENM
e
me

plot(pred_me, 1, cex=0.5, legend=T, mar=par("mar"), xaxt="n", yaxt="n", main="Predicted Species Distribution")
map("state", xlim=c(leftlon,rightlon), ylim=c(lowerlat,upperlat), fill=F, col="black", add=T)
points(random,col="snow",pch=1,cex=0.2)
points(Gspecies$lon, Gspecies$lat, col="darkgreen", pch=20, cex=0.5)

# add axes
axis(1,las=1)
axis(2,las=1)
box()


###
