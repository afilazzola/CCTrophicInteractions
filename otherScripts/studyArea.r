### Generate Study Area
library(dismo)
library(raster)


## raster for basis (extent and resolution) of spatial data
## raster for basis (extent and resolution) of spatial data
r0 <- raster("data//rasters//alt.bil") #altitude
west.ext <- extent(-144,-100,32,83.10833) ## clip to not go into Alaska
r0 <- crop(r0, west.ext)


### Generate study area
## load parnassius
pasm <- read.csv("data//parnassius.clean.csv")

## assign spatial points DF
crs.world <-CRS("+proj=longlat +datum=WGS84")
coordinates(pasm) <- ~lon+lat
proj4string(pasm) <- crs.world

## set 500 km buffer
samplearea <- circles(pasm, d=500000, lonlat=T)
studyarea <- samplearea@polygons

## convert polygon into raster
r1 <- raster(ncol=2000,nrow=1000)
extent(r1) <- extent(studyarea)
studyarea <- raster::rasterize(studyarea, r1) ## convert to raster

studyarea <- crop(studyarea, extent(-148.75, -101, 32.05,69), snap="in")

writeRaster(studyarea, "data//studyArea.tif")
