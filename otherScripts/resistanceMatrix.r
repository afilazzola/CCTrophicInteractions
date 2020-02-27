### Resistance Matrix
library(raster)
library(dismo)

## ## load study area raster
plot(studyarea)

## Load parnassius points
pasm <- read.csv("data//parnassius.clean.csv")
pasm <- pasm[,1:2]
crs.world <-CRS("+proj=longlat +datum=WGS84")
coordinates(pasm) <- ~lon+lat
proj4string(pasm) <- crs.world


### calculate nearest point from raster
distances <- distanceFromPoints(object = studyarea, xy = pasm[1:10,])

## parallarlize to speed up calculation of nearpoint from raster pixel
beginCluster(6)
distall <- clusterR(studyarea, raster::distanceFromPoints, args = list(xy = pasm))
endCluster()

## distance raster is in meters
distkm <- distall/1000 ## convert to km
plot(distkm) ## plot output

## reclassify matrix, small increments between 0-500 km distance, and zero beyond.
reclass <- data.frame(from=c(seq(0,499, 1),500), to=c(seq(1,500, 1),5000) , becomes=seq(1,0,length.out=501))
reclassdist <- reclassify(distkm, rcl=reclass)
plot(reclassdist)

writeRaster(reclassdist, "data//resistanceRasters//pasm.tif")

## generate resistance matrix with other species
spp.list <- c("data//bias2//Erebia.epipsodea.csv","data//bias2//Pontia.occidentalis.csv","data//bias2//Chlosyne.palla.csv","data//bias2//Pieris.rapae.csv","data//parnassius.clean.csv","data//sedumall.csv")

## load coordinates
for(i in 1:length(spp.list)){
gps <- read.csv(spp.list[i])
gps <- gps[,1:2]
crs.world <-CRS("+proj=longlat +datum=WGS84")
coordinates(gps) <- ~lon+lat
proj4string(gps) <- crs.world

## parallarlize to speed up calculation of nearpoint from raster pixel
beginCluster(6)
distall <- clusterR(studyarea, raster::distanceFromPoints, args = list(xy = gps))
endCluster()


## distance raster is in meters
distkm <- distall/1000 ## convert to km
reclass <- data.frame(from=c(seq(0,499, 1),500), to=c(seq(1,500, 1),5000) , becomes=seq(1,0,length.out=501))
reclassdist <- reclassify(distkm, rcl=reclass)

## write new raster to disk
writeRaster(reclassdist, paste0(gsub(".csv","",basename(spp.list[i])),".tif"))
print(i)
}



## resample to match resolution of predictive rasters

## list rasters that were created
occurdat <- list.files("data//resistanceRasters//", pattern=".tif$", full.names = T)

## specify new resolution
tr <- res(allstack)
## specify new extent
e <- bbox(extent(allstack))

library(parallel)
cl <- makeCluster(4) ## define cluster
clusterEvalQ(cl, library(gdalUtils))
clusterExport(cl, c('tr', 'e')) ## export resolution and extent to loop

system.time(parLapply(cl, occurdat, function(f) {
  gdalwarp(f, sub('\\.tif', '_clipped.tif', f), tr=tr, 
           r='bilinear', te=c(e), multi=TRUE)
}))
stopCluster(cl)


