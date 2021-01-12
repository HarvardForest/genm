#' subset_species: subsets a dataframe for a species of interest
#' x = "species name"
#'example: picea <- subset_species("picea") stores a dataframe of unique coordinates

subset_species <- function(species=""){
                  subset(Data, ant.species==species) %>% 
                  select(-ant.species) %>% unique()}

#' map_occurrences: creates a distribution map of species presence data
#' x = longitudinal range
#' y = latitudinal range 
#' z = "Graph Title"
#' example: map_occurrences(df = picea, xrange = c(min(picea$longitude), max(picea$longitude)),
#'          yrange = c(min(picea$latitude), max(picea$latitude)),
#'          title = "Picea Presences") returns a map with points
#' source: The Molecular Ecologist          

map_occurrences <- function(df, xrange, yrange, title=""){
  data("stateMapEnv")
  plot(xrange, yrange, mar=par("mar"),xlab="longitude", ylab="latitude",
       xaxt="n", yaxt="n", type="n", main=title)
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="slategray2")
  map("state", xlim=xrange, ylim=yrange, fill=T, col="lavenderblush1", add=T)
  points(df$longitude, df$latitude, col="darkolivegreen4", pch=20, cex=0.5)
  axis(1,las=1)
  axis(2,las=1)
  box()}
