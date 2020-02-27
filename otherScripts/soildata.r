## Soil Data - Regridded Harmonized World Soil Database v1.2
## https://daac.ornl.gov/SOILS/guides/HWSD.html



soildepth <- raster("data//soildata//REF_DEPTH.nc4") ## reference soil depth as a proxy for bedrock
soilgravel <- raster("data//soildata//T_GRAVEL.nc4") ## topsoil gravel content that favours sedum cover

## Reproject to same resolution as Worldclim rasters
r1 <- raster("data\\rasters\\alt.bil") #altitude


newdepth <- resample(soildepth, r1, method="ngb") ## resample using nearest neighbour
writeRaster(newdepth,"data//soildata//soildepth.bil" ) ## write to raster file

newgravel <- resample(soilgravel, r1, method="ngb") ## resample using nearest neighbour
writeRaster(newgravel,"data//soildata//soilgravel.bil" ) ## write to raster file


## Find relevant soil variables
occurdat<-list.files("data//soildata//",pattern=".nc4$",full=T)

soilstack <- stack()
for(i in 1:length(occurdat)){
temp <- raster(occurdat[i])
soilstack <- stack(soilstack, temp)
}

library(bestglm)

## Read in Parnassius occurrence
sedum <- read.csv("data//sedum.csv")


## convert facilitated occurrence into spatial points
sedum.gps <- sedum[,1:2]
crs.world <- crs(soilstack)
coordinates(sedum.gps) <- ~lon+lat
proj4string(sedum.gps) <- crs.world



## extract climate values for sedum
sedum.climate <- extract(soilstack, sedum.gps)

## extract climate values for background
backgr <- randomPoints(soilstack, 1000)
backgr.climate <- extract(soilstack, backgr)

## join two datasets and add presence absence column
climate.data <- rbind(data.frame(sedum.climate),data.frame(backgr.climate))
climate.data[,"y"] <- as.numeric( c(rep("1",nrow(sedum.climate)),rep("0",nrow(backgr.climate))))

# ## run best subsets
m1 <- bestglm(climate.data, family=binomial, IC="AIC")


## Resample new rasters based on relevant soil variables


topsoiloc <- raster("data//soildata//AWT_T_SOC.nc4") ## topsoil.organic.carbon.by.percent.weight
topsoilph <- raster("data//soildata//T_PH_H2O.nc4") ## topsoil.pH.in.water
topsoilcec  <- raster("data//soildata//T_CEC_CLAY.nc4") ##  cation.exchange.capacity.of.the.clay.fraction.in.the.topsoil
topsoilsand  <- raster("data//soildata//T_SAND.nc4") ##  topsoil.sand.fraction.by.percent.weight
topsoilsilt  <- raster("data//soildata//T_SILT.nc4") ##  topsoil.silt.fraction.by.percent.weight


newoc <- resample(topsoiloc, r1, method="ngb") ## resample using nearest neighbour
writeRaster(newoc,"data//soildata//topsoiloc.bil" ) ## write to raster file

newph <- resample(topsoilph, r1, method="ngb") ## resample using nearest neighbour
writeRaster(newph,"data//soildata//topsoilph.bil" ) ## write to raster file

newcec <- resample(topsoilcec, r1, method="ngb") ## resample using nearest neighbour
writeRaster(newcec,"data//soildata//topsoilcec.bil" ) ## write to raster file

newsilt <- resample(topsoilsilt, r1, method="ngb") ## resample using nearest neighbour
writeRaster(newsilt,"data//soildata//topsoilsilt.bil" ) ## write to raster file

newsand <- resample(topsoilsand, r1, method="ngb") ## resample using nearest neighbour
writeRaster(newsand,"data//soildata//topsoilsand.bil" ) ## write to raster file

