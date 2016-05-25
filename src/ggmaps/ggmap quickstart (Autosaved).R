#A.Calderon
#Harvard Forest
#160524
#Installation (Maps)-How To


#>install.packages("ggmap")
#>install.packages("ggplot2")
#>library(ggplot2)

#THECODE
#>myLocation<-c(lon=________, lat=_______)
#>maptype=c("_____", "_______","______", "______")
#>myMap<-get_map(location=myLocation, source="mapwebsite", maptype="_____")

#Putting_It_Together!

#Example: >myMap<-get_map(location=myLocation, source="google", maptype="terrain")

#Try_number_1B
#Patagonia Map using Google and different maptypes

Patagonia_Location<-c(lon=-68.081488, lat=-41.464458)
PatagoniaMap<-get_map(location=Patagonia_Location, source="google", maptype="satellite")
PatagoniaMap<-get_map(location=Patagonia_Location, source="google", maptype="hybrid")
PatagoniaMap<-get_map(location=Patagonia_Location, source="google", maptype="roadmap")

#Try_number_2B
#Patagonia Map using Different Sources and Maptypes

PatagoniaMap<-get_map(location=Patagonia_Location, source="stamen", maptype="terrain")
PatagoniaMap<-get_map(location=Patagonia_Location, source="stamen", maptype="watercolor")

#sidenote: source="stamen" produces same maptypes.

#Plotting Your Map
ggmap(PatagoniaMap)
ggmap(PatagoniaMap)+

geom_point(aes(x=Longitude, y=Latitude, size=sqrt(estArea)), data=data, alpha=.5, color="darkred", size=3) 




