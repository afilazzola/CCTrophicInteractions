## New extent
pasm <- read.csv("data//parnassius.csv")

## assign spatial points DF
crs.world <-CRS("+proj=longlat +datum=WGS84")
coordinates(pasm) <- ~lon+lat
proj4string(pasm) <- crs.world

## crop to western NA only
west.ext <- extent(-155,-100,32,83)
pasm <-crop(pasm, west.ext)

## add buffer
samplearea <- circles(pasm, d=1000000, lonlat=T)
studyarea <- samplearea@polygons

## convert polygon into raster
r1 <- raster(ncol=2000,nrow=1000)
extent(r1) <- extent(studyarea)
studyarea <- rasterize(studyarea, r1) ## convert to raster



## split raster into tiles
studyarea <- crop(studyarea, west.ext) ## crop to a reasonable extent of study area
library(SpaDES)
tiles <- splitRaster(studyarea, nx=4, ny=4, buffer=c(5,10))

for(i in 1:16){
writeRaster(tiles[[i]], paste0("data//tiles//studyarea",i,".tif"), overwrite=T)
}

## drop tile 1 : too small, no values
## drop tiles 2 & 5 : the ocean



## assign the rasters to tiles
elvtiles <- read.csv("data//elevation//demlist.csv", stringsAsFactors = F)

for(i in 2:16){
  tryCatch({
study <- raster(paste0("data//tiles//studyarea",i,".tif")) ## drop study 1 because too small
bounds <- extent(study) ## describe boundaries for study plot
relvTiles <- elvtiles %>% filter(xmin < bounds[2] & xmax > bounds[1]) %>% ## find all elevation tiles within study tiles
  filter(ymin < bounds[4] & ymax > bounds[3])

elevFiles <- list.files("data//elevation//DEMtiff//", full.names = T) ## list all the elevation tiles
tileNames <- gsub("^.*?grd","",elevFiles) %>% gsub("_1.tif","", .)   ## strip down file name to tile name
filedata <- data.frame(elevFiles, tileNames, stringsAsFactors = F) ## create dataset that matches tile name to file name

filesMerge <- filedata[filedata$tileNames %in% relvTiles$filename,"elevFiles"] ## select files that fit within study tile
if ( length(filesMerge) == 0) stop("no elevation tiles in study tile")

rlist <- lapply(filesMerge, raster) ## load all files as a raster
tiledata <- do.call(merge, rlist) ## merge all rasters together into one tile

tilefinal <- crop(tiledata, study) ## mask/crop the file to study area

## write elevation tiles to file
writeRaster(tilefinal, paste0("data//elevationtiles//elevationStudy",i,".tif"))
print(i)
}, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}




### Load climate rasters and crop tiles
r3 <- raster("data\\rasters\\bio_12.bil") # annual precip
r4 <- raster("data\\rasters\\bio_6.bil") # min temperature in coldest month
r11 <- raster("data\\rasters\\tmax\\tmax_11.bil") # average max temperature in November for majority of spp range
topsoiloc <- raster("data//soildata//topsoiloc.bil") ## organic carbon in soil
topsoilsilt <- raster("data//soildata//topsoilsilt.bil")  ## silt in topsoil

predStack <- stack(r3,r4,r11,topsoiloc, topsoilsilt)

## crop rasters to tiles

tiles <- list.files("data//tiles", full.names=T)



for(i in 1:13){
  tryCatch({
    t1 <- raster(tiles[i])
    climt1 <- crop(predStack, t1)
    proj4string(climt1) <- crs(t1)
    if ( class(climt1) != "RasterBrick") stop("crop failed")
    writeRaster(climt1, paste0("data//pasmClimTiles//pasmClim",i,".grd"), options="INTERLEAVE=BAND")
    print(i)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}



### Generate elevation variation tiles
tile0 <- list.files("data//elevationtiles", full.names=T)
pasmClim0 <- list.files("data//pasmClimTiles", full.names=T, pattern="*.grd")
# 
# ## order files
# ex <- lapply(tile0, function(x){extent(raster(x))})
# ex2 <- lapply(pasmClim0, function(x){extent(raster(x))})

for(i in 1:13){
  tile1 <- raster(tile0[i])
  clim1 <- raster(pasmClim0[i], band=1)
  agg <- aggregate(tile1, fact=30, fun=sd)
  resample(agg, clim1, method="bilinear", filename= paste0("data//elevationtiles//elvdiff",i,".tif"))
  print(i)
  gc()
}


## combine elevation tiles into one
elvtile <- list.files("data//elevationtiles", full.names=T, pattern="elvdiff*")


jointiles <- raster(elvtile[1])
for(i in 2:13){
tile <- raster(elvtile[i])
jointiles <- mosaic(jointiles, tile, fun=mean)
}



### Check elevation of parnassius

## reading in parnassius points
pasm <- read.csv("data//parnassius.csv")

## remove outliers
pasm <- subset(pasm, lon > -150) ## points in western alaska like P. phoebus
pasm <- subset(pasm, lon < -100) ## points in eastern Canada likely incorrect
## Kept Parnassius behrii (sierra Nevada parnassian) because it also feeds on sedum and is found in similar habitats

## assign spatial points DF
crs.world <-CRS("+proj=longlat +datum=WGS84")
coordinates(pasm) <- ~lon+lat
proj4string(pasm) <- crs.world

## list DEM tiles
elvtile <- list.files("data//elevationtiles", full.names=T, pattern="elevation*")

## extract elevation for each parnassius points from all the tiles
demout <- sapply(elvtile, FUN=function(x){extract(raster(x), pasm)})

## elevation per point
demsingle <- apply(demout, 1, FUN=function(x) {max(x ,na.rm=T)})

hist(demsingle)

## drop points with elevation below 500
dropcol <- ifelse(demsingle < 500, 1, 0)

plot(pasm[dropcol==1,])

## new parnassius dataset
pasm2 <- pasm[dropcol!=1,]
write.csv(pasm2, "parnassius.clean.csv", row.names = F)


