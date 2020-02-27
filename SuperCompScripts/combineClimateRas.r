### Combining all the climate rasters in stacks
## load packages 
library(raster)
library(dismo)
library(rgdal)
library(rgeos)
library(foreach)
library(doParallel)

## set working directory
setwd("~/projects/def-sapna/afila/PASMmodels")


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

### Select climate & soil variables to be used
## load in "soft" variables - climate

c1 <- raster("worldclim//tmax//tmax11.bil") ## max in Nov
c2 <- raster("worldclim//prec//prec07.bil") ## prec in July
c3 <- raster("worldclim//bioclim//bio6.bil") ## min temp in coldest month
c4 <- raster("worldclim//bioclim//bio1.bil") ## mean annual temp
c5 <- raster("worldclim//bioclim//bio3.bil") ## Isothermality 
c6 <- raster("worldclim//bioclim//bio19.bil") ## precipitation in the coldest quarter

climstack <- stack(c1, c2, c3, c4, c5, c6)
climstack <- crop(climstack, studyarea, snap="in")

## Load in "hard" variables
r1 <- raster("data//rasters//alt.bil") #altitude
r1 <- crop(r1, studyarea , snap="in")
# dropped elv diff because contributed very little
r2 <- raster("data//rasters//elevationdiff.tif") ## "slope" - sd of elevation per tile
r2 <- crop(r2, studyarea , snap="in")

## soil rasters as suggests by Jens as proxies for bedrock
topsoiloc <- raster("data//rasters//topsoiloc.bil") ## organic carbon in soil
topsoilsilt <- raster("data//rasters//topsoilsilt.bil")  ## silt in topsoil

soilstack <- stack(topsoiloc, topsoilsilt)
soilstack <- crop(soilstack, studyarea, snap="in")
crs(soilstack) <- crs.world

## stack all
allstack <- stack(r1,r2, soilstack, climstack)

stackNames <- names(allstack)
climModel <- c("he26","he60", "he85","gf26","gf45","gf85","mp26","mp45","mp85","mg26","mg45","mg85")


registerDoParallel(cores=4)
foreach(i = 1:12,  .combine=rbind, .packages=c("dismo","raster","rgdal","rgeos","snow")) %dopar% {
## list climate models

climList <- list.files("futureClimate//", recursive=T, pattern=paste0(climModel[i],"*"), full.names=T) ## list all files with that climate model

## select only the needed variables
## convert temperature rasters with parallel processing
c1 <- raster(climList[21]) ## max in Nov
c3 <- raster(climList[16]) ## min temp in coldest month
c4 <- raster(climList[1]) ## mean annual temp
c2 <- raster(climList[20]) ## prec in July
c5 <- raster(climList[13]) ## Isothermality 
c6 <- raster(climList[11]) ## preciptation in coldest quarter

## stack and clip
futureClimStack <- stack(c1, c2, c3, c4, c5, c6)
futureClimStack <- crop(futureClimStack, r1, snap="near")

## stack climate to other variables
futureClim <- stack(r1, r2, soilstack, futureClimStack) 
names(futureClim) <- stackNames ## rename climate variables to same name
crs(futureClim) <- crs.world

writeRaster(futureClim,  paste0("futureClimate//Stacks//model",climModel,".grd")[i], overwrite=T)


}