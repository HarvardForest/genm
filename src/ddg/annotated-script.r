1: library(FedData)
2: vepPolygon <- polygon_from_extent(raster::extent(672800, 672950, 
2:     4102000, 4104000), proj4string = "+proj=utm +datum=NAD83 +zone=12")
3: NED <- get_ned(template = vepPolygon, label = "VEPIIN")
4: set.seed(123)
5: r <- raster(NED)
6: altDiff <- function(x) {
6:     ddg.function()
6:     ddg.return.value(x[2] - x[1])
6: }
7: hd <- transition(r, altDiff, 8, symm = FALSE)
8: slope <- geoCorrection(hd)
9: adj <- adjacent(r, cells = 1:ncell(r), pairs = TRUE, directions = 8)
10: speed <- slope
11: speed[adj] <- 6 * exp(-3.5 * abs(slope[adj] + 0.05))
12: Conductance <- geoCorrection(speed)
13: image(Conductance[1:3, 1:3])
14: A <- c(672800, 4102000)
15: B <- c(740000, 4170000)
16: AtoB <- shortestPath(Conductance, A, B, output = "SpatialLines")
