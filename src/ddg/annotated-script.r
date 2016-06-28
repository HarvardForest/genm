1: wd <- "."
2: genus <- ""
3: species <- ""
4: leftlon <- ""
5: rightlon <- ""
6: lowerlat <- ""
7: upperlat <- ""
8: path <- ("")
9: if (path == "") {
9:     path <- "../data"
9: }
10: filename <- ("GspeciesBC_2.5.grd")
11: paste(path, filename, sep = "/")
12: croppeddata <- paste(path, filename, sep = "/")
13: setwd(wd)
14: packs <- c("rgbif", "mapproj", "mapdata", "sp", "maptools", "dismo", 
14:     "rJava", "rgdal")
15: if (!require("pacman")) {
15:     install.packages("pacman")
15: }
16: library(pacman)
17: pacman::p_load(packs)
18: lapply(packs, require, character.only = TRUE)
19: if (leftlon == "") {
19:     leftlon <- -99.2
19: }
20: if (rightlon == "") {
20:     rightlon <- -63
20: }
21: if (lowerlat == "") {
21:     lowerlat <- 23.6
21: }
22: if (upperlat == "") {
22:     upperlat <- 45.5
22: }
23: if (genus == "") {
23:     genus <- "Aphaenogaster"
23:     species <- "picea"
23: }
24: rawdata <- gbif(genus = genus, species = species)
25: na.omit(rawdata[, c("lat", "lon")])
26: Gspecies <- na.omit(rawdata[, c("lat", "lon")])
27: data(stateMapEnv)
28: plot(c(leftlon, rightlon), c(lowerlat, upperlat), mar = par("mar"), 
28:     xlab = "longitude", ylab = "latitude", xaxt = "n", yaxt = "n", 
28:     type = "n", main = "Presence and Absence Points")
29: rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
29:     col = "lightcyan")
30: map("state", xlim = c(leftlon, rightlon), ylim = c(lowerlat, 
30:     upperlat), fill = T, col = "honeydew", add = T)
31: points(Gspecies$lon, Gspecies$lat, col = "darkolivegreen4", pch = 20, 
31:     cex = 0.5)
32: axis(1, las = 1)
33: axis(2, las = 1)
34: box()
35: longrid = seq(leftlon, rightlon, 0.05)
36: latgrid = seq(lowerlat, upperlat, 0.05)
37: subs = c()
38: for (i in 1:(length(longrid) - 1)) {
38:     for (j in 1:(length(latgrid) - 1)) {
38:         gridsq = subset(Gspecies, lat > latgrid[j] & lat < latgrid[j + 
38:             1] & lon > longrid[i] & lon < longrid[i + 1])
38:         if (dim(gridsq)[1] > 0) {
38:             subs = rbind(subs, gridsq[sample(1:dim(gridsq)[1], 
38:                 1), ])
38:         }
38:     }
38: }
39: dim(subs)
40: x = circles(subs[, c("lon", "lat")], d = 50000, lonlat = T)
41: bg = spsample(x@polygons, 1000, type = "random", iter = 1000)
42: points(bg, col = "khaki4", pch = 1, cex = 0.3)
43: require(raster)
44: BClim = getData("worldclim", var = "bio", res = 2.5, path = "")
