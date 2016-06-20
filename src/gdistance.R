install.packages("gdistance")
library("gdistance")
set.seed(123)
r <- raster(ncol=3,nrow=3)
r[] <- 1:ncell(r)
r
plot(r, main="r", xlab="Longitude (degrees)", ylab="Latitude (degrees)")
text(r)
