
#' map_occurrences: creates a distribution map of species presence data
#' x = longitudinal range
#' y = latitudinal range 
#' z = "Graph Title"
#' example: map_occurrences(c(-73.7, -67.4), c(41, 47.5), "Aphaenogaster Presence Data") 
#' ## returns a map with points

map_occurrences <- function(xrange, yrange, title=""){
  data("stateMapEnv")
  plot(xrange, yrange, mar=par("mar"),xlab="longitude", ylab="latitude",
       xaxt="n", yaxt="n", type="n", main=title)
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="slategray2")
  map("state", xlim=xrange, ylim=yrange, fill=T, col="lavenderblush1", add=T)
  points(Data$longitude, Data$latitude, col="darkolivegreen4", pch=20, cex=0.5)
  axis(1,las=1)
  axis(2,las=1)
  box()}