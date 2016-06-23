1: set.seed(13)
2: r <- raster(ncol = 3, nrow = 3)
3: r[] <- 1:ncell(r)
4: r
5: plot(r, main = "r", xlab = "Longitude (degrees)", ylab = "Latitude (degrees)")
6: text(r)
7: r[] <- 1
8: tr1 <- transition(r, transitionFunction = mean, directions = 8)
9: tr1
10: r[] <- runif(9)
11: ncf <- function(x) {
11:     ddg.function()
11:     ddg.return.value(max(x) - x[1] + x[2])
11: }
12: tr2 <- transition(r, ncf, 4, symm = FALSE)
13: tr2
14: tr3 <- tr1 * tr2
15: tr3 <- tr1 + tr2
16: tr3 <- tr1 * 3
17: tr3 <- sqrt(tr1)
18: tr3[cbind(1:9, 1:9)] <- tr2[cbind(1:9, 1:9)]
19: tr3[1:9, 1:9] <- tr2[1:9, 1:9]
20: tr3[1:5, 1:5]
21: image(transitionMatrix(tr1))
22: r <- raster(system.file("external/maungawhau.grd", package = "gdistance"))
23: altDiff <- function(x) {
23:     ddg.function()
23:     ddg.return.value(x[2] - x[1])
23: }
24: hd <- transition(r, altDiff, 8, symm = FALSE)
