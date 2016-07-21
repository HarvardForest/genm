setwd("/Users/annacalderon/Desktop/gENM/src")

library(geoknife)
library(rgdal)
library(rasterVis)

url(knife) <- 'http://cida-test.er.usgs.gov/gdp/process/WebProcessingService'
algorithm(knife) <- query(knife, 'algorithms')[1]
knife <- webprocess(wait = TRUE)
fabric <- webdata(url='dods://cida.usgs.gov/thredds/dodsC/macav2metdata_daily_future',
          variable='tasmax_CSIRO-Mk3-6-0_r1i1p1_rcp85',times=c('2014-07-15','2014-8-16'))
#stencil <- simplegeom(data.frame('point1' = c(-73,41), 'point2' = c(-68,47.45853))) 
stencil <- webgeom('state::Massachusetts')
job <- geoknife(stencil, fabric,  wait = TRUE)
test <- result(job)
file <- download(job, destination = '../data/geoknife', overwrite=TRUE)
tiff.dir <- '../data/geoknife'
unzip(file,exdir = tiff.dir)
rast <- raster(file.path(tiff.dir , dir(tiff.dir)))
