library(leaflet)
library(sp)

coords <- cbind(Apicea$lon, Apicea$lat)
sp_Apieca <- SpatialPointsDataFrame(coords = coords, data = Apicea, proj4string = CRS("+proj=longlat +datum=WGS84"))
sp_Apieca_proj <- spTransform(sp_Apieca, CRS("+init=epsg:26914"))

m <- leaflet()
m <- addCircleMarkers(m, data = Apicea, lat = ~ lat, lng = ~lon, radius=4, opacity=3, fillColor="yellowgreen", fillOpacity=6, weight=0.5, popup = Apicea$Name) 
m %>% addTiles()



m<- leafletProxy()