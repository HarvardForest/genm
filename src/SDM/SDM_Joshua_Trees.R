#If you don't already have these packages, install them (from CRAN)
install.packages("mapproj")
install.packages("mapdata")
install.packages("sp")
install.packages("maptools")
install.packages("dismo")
install.packages("JoTrPresence02202008.txt")

#require basically "loads" your R packages. 
#But, I've read that "library" is the more correct way to do this.
#Instead "require" TRYs to load the package from the library.
#Whereas "library" will stop if the package you are looking for isn't already installed. 

require(mapproj)
require(mapdata)
require(maptools)
require(dismo)

# load the table of latitude and longitude coordinates
locs = read.csv (file="JoTrPresence02202008.txt", header=T, sep="\t")

## this ^^^ one will give me an error. Can you take a look at it. 