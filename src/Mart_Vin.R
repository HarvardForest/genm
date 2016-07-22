gspecies <- ''

prespoints <- read.csv('http://harvardforest.fas.harvard.edu/data/p14/hf147/hf147-13-antData_use4R_snappedToClim.csv')
if (gspecies == ''){gspecies <- "aphrud"}
colnames(prespoints) = c("spcode", "lon","lat")
gspecies <- prespoints[grep(gspecies,as.character(prespoints$spcode)),]
gspecies$spcode <- NULL

gspecies <- read.csv('../data/gspecies.csv')

if (identical(colnames(gspecies),c( "lat", "lon"))){gspecies <- gspecies[,c('lon','lat')]}
if (is.matrix(gspecies) == FALSE){gspecies <- data.matrix(gspecies)}


### Getting state data
gsp <- gspecies



us <- getData("GADM", country="USA", level=1)
# extract states (need to uppercase everything)
nestates <- c("Massachusetts")
MA = us[match(toupper(nestates),toupper(us$NAME_1)),]
plot(MA)


point <- data.frame(gsp)
MA.points <- sapply(1:294,function(i)
  list(point[i,],
       gContains(MA,SpatialPoints(point[i,1:2],proj4string=CRS(proj4string(MA))))))
                                                # this piece of code will tell you what points
                                                # are contained within Massachusetss
MA  <- as.matrix(MA.points[-1,])
points(gspecies[unlist(MA),],col='violet')

#clearn up your data
#because I didn't know how to look at TRUE
#for a list, I just exported MA as a cvs
#and got my values from there.---> #mass.data  <- subset(gsp[c(1,2,...............292])
#I only used the TRUE values from gsp (aka, mass.data)
#to subset the gsp data  into  points that belonged to
#Massachusetts. I then re-subesetted the Massachusetts
#coordinates into only points that belonged to a 
#latitude lower than the tip of M.V and a lon
#greater than the minimum of M.V. Saved that into
#a csv file and re-uploaded that. 




MVin.points <- read.csv(file = "/Users/annacalderon/Desktop/gENM/data/marthas_vineyard.csv")
MVin.points$X <- NULL
if (is.matrix(MVin.points) == FALSE){MVin.points <- data.matrix(MVin.points)}
points(MVin.points, col="red", cex=0.5, pch=20)

