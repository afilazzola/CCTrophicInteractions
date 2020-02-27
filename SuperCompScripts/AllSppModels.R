
## load packages 
library(raster)
library(dismo)
library(rgdal)
library(rgeos)
library(rJava)
library(foreach)
library(doParallel)

## set working directory
setwd("~/projects/def-sapna/afila/PASMmodels")


## Set CRS for study
crs.world <-CRS("+proj=longlat +datum=WGS84")

## Load study area
studyarea <- raster("data//studyArea.tif")

allstack <- stack("data//currentClim.grd")
crs(allstack) <- crs.world

## load bias file
biasfile <- raster("data//biasfile.bil")
biasfile <- crop(biasfile, studyarea)

## Erebia epipsodea - alpine and polyphagous
## Pontia occidentalis - polyphagous and lives in rockies
## Chlosyne palla - polyphagous and montane

## list butterflies and resistance matrices
spp.list <- c("data//bias2//Erebia.epipsodea.csv","data//bias2//Pontia.occidentalis.csv","data//bias2//Chlosyne.palla.csv","data//bias2//Pieris.rapae.csv","data//parnassius.clean.csv","data//sedumRed.csv")
resistance.list <- c("data//resistance//Erebia.epipsodea.tif","data//resistance//Pontia.occidentalis.tif","data//resistance//Chlosyne.palla.tif","data//resistance//Pieris.rapae.tif","data//resistance//parnassius.clean.tif","data//resistance//sedumall.tif")

## list climate models
climList <- list.files("futureClimate//Stacks//", recursive=T, pattern="*.grd", full.names=T) ## list all files with that climate model

## specify number of cores available
cl <- makeCluster(24, type="FORK")
clusterEvalQ(cl, { library(MASS); RNGkind("L'Ecuyer-CMRG") })
registerDoParallel(cores = cl)

finalResults = foreach(k = 1:10, .combine=rbind, .packages=c("rJava","dismo","raster","rgdal","rgeos"), .errorhandling="remove") %:%
  foreach(j = 6,  .combine=rbind)  %:%
  foreach(i = 1:12,  .combine=rbind) %dopar% {
    
    #creates unique filepath for temp directory
    dir.create(file.path(paste0("~//scratch//temp//iteration",i,j,k)), showWarnings = FALSE)
    
    set.seed(k) ## set randomization parameters
    
    ### specify species parameters for iteration
    csv.file <- spp.list[j]
    resistance.file <- resistance.list[j]
    
    ## load gps points for species
    gps <- read.csv(csv.file)
    
    ## convert facilitated occurrence into spatial points
    gps <- gps[,1:2]
    coordinates(gps) <- ~lon+lat
    proj4string(gps) <- crs.world
    
    ##crop out points to western Canada
    gps <- crop(gps, extent(biasfile))
    
    ## generate sample area with background points
    samplearea <- circles(gps, d=100000, lonlat=T)
    
    #generate random points in sample area
    backgr <- spsample(samplearea@polygons, 10000, type='random', iter=1000)
    
    
    ## withhold 20% for sample testing
    fold.p <- kfold(gps, k=5)
    occtest.p <-gps[fold.p == 4, ]
    occtrain.p <- gps[fold.p != 4, ]
    fold.a <- kfold(backgr, k=5)
    occtest.a <-backgr[fold.a == 4, ]
    occtrain.a <- backgr[fold.a != 4, ]
    
    ## maxent species
    max3 <- maxent(allstack, p=occtrain.p, a=occtrain.a, biasfile=biasfile)
    
    ## get species name
    spp.name <- basename(csv.file)
    spp.name <- gsub(".csv","", spp.name)
    
    ## evaluate pasm only
    eval3 <- evaluate(occtest.p, occtest.a, max3, allstack)
    
    ## select only the climate model for this iteration
    futureClim <- stack(climList[i])
    modelName <- gsub("model", "",   basename(climList[i])) ## remove model tag
    modelName <- gsub(".grd", "",   modelName) ## remove file type
    
    
    ## predict climate
    pred1 <- predict(allstack,  max3)
    pred2 <- predict(futureClim, max3)
    
    
    ## Load resistance matrix
    resist <- raster(resistance.file)
    
    ## Apply resistance matrix to habitat suitability
    pred1 <- pred1 * resist
    pred2 <- pred2 * resist
    
    
    ## calculate area of distribution
    currentArea <- sum(area(pred1)[pred1>=0.00])
    oldRange <- sum(area(pred1)[pred1>0.05]) / currentArea  ## area where occurrence > 0.05 relative to raster
    newRange <- sum(area(pred2)[pred2>0.05]) / currentArea ## future area where occurrence > 0.05 relative to raster
    areaLoss <- sum(area(pred1)[pred2<0.05 & pred1 > 0.05]) / currentArea   ## area where future is <0.05 and current is > 0.05 
    areaGain <- sum(area(pred2)[pred2>0.05 & pred1 < 0.05]) / currentArea  ## area where current is <0.05 and future is > 0.05 
    
    ## output
    spp.results <- data.frame(model=modelName,cor.model=eval3@cor,auc.model=eval3@auc, oldRange=oldRange, newRange=newRange, areaLoss=areaLoss, areaGain = areaGain)
    
    
    ## specify species in model
    spp.results[,"species"] <- spp.list[j]
    
    ## specify randomization parameter
    spp.results[,"iter"] <- k
    data.frame(spp.results)
    write.csv(spp.results, paste0("~//scratch//modelOut//otherspp",i,j,k,".csv"), row.names=F)
    

    ## Skip saving rasters after first iteration
    if(k == 1){
      
      ## Print Figures
      ## Write raster for predicted outputs
      if(i == 1){ ## output one current climate
        writeRaster(pred1, paste0("~//scratch//rasOut//",spp.name,"currentdistro.tif"), overwrite=T)
        writeRaster(pred2, paste0("~//scratch//rasOut//",spp.name,modelName,"futuredistro.tif"), overwrite=T)
      } else{
        writeRaster(pred2, paste0("~//scratch//rasOut//",spp.name,modelName,"futuredistro.tif"), overwrite=T)
      }
      ## write raster with difference in climate projections
      diffRas <- pred2 - pred1
      writeRaster(diffRas, paste0("~//scratch//RasterDiff//",spp.name,modelName,".tif"), overwrite=T)
      
      ## remove large objects
      rm(pred1)
      rm(pred2)
      rm(futureClim)
      
    } else{
      ## remove large objects
      rm(pred1)
      rm(pred2)
      rm(futureClim)
      
    }
    
    ## clean up temporary files to speed up process
    unlink(file.path(paste0("~//scratch//temp//iteration",i,j,k)), recursive = TRUE)
    
  }
stopCluster(cl)

write.csv(finalResults, "~//scratch//OtherSpecies.csv", row.names=FALSE)
