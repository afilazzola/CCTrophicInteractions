
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

## list climate models
climList <- list.files("futureClimate//Stacks//", recursive=T, pattern="*.grd", full.names=T) ## list all files with that climate model

## define resistance matrix
resist <- raster("data//resistance//pasm.tif")

############ Model parnassius and sedum #########

## specify number of cores available
## specify number of cores available
cl <- makeCluster(24, type="FORK")
clusterEvalQ(cl, { library(MASS); RNGkind("L'Ecuyer-CMRG") })
registerDoParallel(cores = cl)


pasmOut <- foreach(k = 1:10, .combine=rbind,  .packages=c("rJava","dismo","raster","rgdal","rgeos"), .errorhandling="pass") %:% 
  foreach(i = 1:12, .combine=rbind) %dopar% {
  
  # setting random seed to always create the same
  # random set of points for this example
  set.seed(k)
  
  #creates unique filepath for temp directory
  dir.create(file.path(paste0("~//scratch//temp//pasm",i,k)), showWarnings = FALSE)
  
  ## species list
  spp.list <- c("data//parnassius.clean.csv","data//sedumRed.csv")
  
  ## Run model for pasm
  gps <- read.csv(spp.list[1])
  
  ## convert facilitated occurrence into spatial points
  gps <- gps[,1:2]
  coordinates(gps) <- ~lon+lat
  proj4string(gps) <- crs.world
  
  ##crop out points to western Canada
  gps <- crop(gps, extent(biasfile))
  
  samplearea <- circles(gps, d=100000, lonlat=T)
  
  ### SDM with variables
  
  #generate random points
  #extract climate data
  backgr <- spsample(samplearea@polygons, 10000, type='random', iter=1000)
  
  
  ## withhold 20% for sample testing
  fold.p <- kfold(gps, k=5)
  occtest.p <-gps[fold.p == 4, ]
  occtrain.p <- gps[fold.p != 4, ]
  fold.a <- kfold(backgr, k=5)
  occtest.a <-backgr[fold.a == 4, ]
  occtrain.a <- backgr[fold.a != 4, ]
  
  
  ## maxent pasm only
  max1 <- maxent(allstack, p=occtrain.p, a=occtrain.a, biasfile=biasfile)

  ## evaluate pasm only
  eval1 <- evaluate(occtest.p, occtest.a,max1, allstack)
  
  ## Run model for sedum
  gps <- read.csv(spp.list[2])
  
  ## convert facilitated occurrence into spatial points
  gps <- gps[,1:2]
  coordinates(gps) <- ~lon+lat
  proj4string(gps) <- crs.world
  
  ##crop out points to western Canada
  gps <- crop(gps, extent(biasfile))
  
  samplearea <- circles(gps, d=100000, lonlat=T)
  
  #generate random points
  #extract climate data
  backgr <- spsample(samplearea@polygons, 10000, type='random', iter=1000)
  
  
  ## withhold 20% for sample testing
  fold.p <- kfold(gps, k=5)
  occtest.p <-gps[fold.p == 4, ]
  occtrain.p <- gps[fold.p != 4, ]
  fold.a <- kfold(backgr, k=5)
  occtest.a <-backgr[fold.a == 4, ]
  occtrain.a <- backgr[fold.a != 4, ]
  
  
  ## maxent pasm only
  max2 <- maxent(allstack, p=occtrain.p, a=occtrain.a, biasfile=biasfile)

  ## evaluate pasm only
  eval2 <- evaluate(occtest.p, occtest.a,max1, allstack)
  
  
  #### Project future models
  futureClim <- stack(climList[i])
  modelName <- gsub("model", "",   basename(climList[i])) ## remove model tag
  modelName <- gsub(".grd", "",   modelName) ## remove file type
    
    ## predict climate
    pred1 <- predict(allstack, max1)
    pred1.sed <- predict(allstack, max2)
    pred2 <- predict(futureClim, max1)
    pred2.sed <- predict(futureClim,  max2)
    
   
    ## correlation between pasm and sedum
    cor.current <- cor(pred1@data@values, pred1.sed@data@values,use="pairwise.complete.obs", method= "spearman")
    cor.future <- cor(pred2@data@values, pred2.sed@data@values,use="pairwise.complete.obs", method= "spearman")
    
    
    ## calculate truncated parnassius based on sedum distro
    pred1.sed <- reclassify(pred1.sed, data.frame(from=c(0,0.05),to=c(0.05,1), becomes=c(0,1))) ## generate sedum presence absence raster
    pred2.sed <- reclassify(pred2.sed, data.frame(from=c(0,0.05),to=c(0.05,1), becomes=c(0,1))) ## generate sedum presence absence raster
	
    ## drop parnassius occurence with sedum < 0.05
    pred1 <- pred1 * pred1.sed ## current
    pred2 <- pred2 * pred2.sed ## future
	
	## cut points below threshold
    pred1 <- pred1 * resist
	pred2 <- pred2 * resist
    
    ## calculate area of distribution
    currentArea <- sum(area(pred1)[pred1>=0.00])
    oldRange <- sum(area(pred1)[pred1>0.05]) / currentArea  ## area where occurrence > 0.05 relative to raster
    newRange <- sum(area(pred2)[pred2>0.05]) / currentArea ## future area where occurrence > 0.05 relative to raster
    areaLoss <- sum(area(pred1)[pred2<0.05 & pred1 > 0.05]) / currentArea   ## area where future is <0.05 and current is > 0.05 
    areaGain <- sum(area(pred2)[pred2>0.05 & pred1 < 0.05]) / currentArea  ## area where current is <0.05 and future is > 0.05 
   
    unlink(file.path(paste0("~//scratch//temp//pasm",i,k)), recursive = TRUE)
    
     ## output
      results <- data.frame(model=modelName,cor.pasm=eval1@cor,auc.pasm=eval1@auc,cor.current=cor.current, cor.future=cor.future, oldRange=oldRange, newRange=newRange, areaLoss=areaLoss, areaGain = areaGain)
      results[,"iter"] <- k
      data.frame(results)
      write.csv(results, paste0("~//scratch//modelOut//pasm",i,k,".csv"), row.names=F)
    
	  
      ## Skip saving rasters after first iteration
      if(k == 1){
        
        ## Print Figures
        ## Write raster for predicted outputs
        if(i == 1){ ## output one current climate
          writeRaster(pred1, paste0("~//scratch//rasOut//currentdistro.tif"), overwrite=T)
          writeRaster(pred2, paste0("~//scratch//rasOut//",modelName,"futuredistro.tif"), overwrite=T)
        } else{
          writeRaster(pred2, paste0("~//scratch//rasOut//",modelName,"futuredistro.tif"), overwrite=T)
        }
        ## write raster with difference in climate projections
        diffRas <- pred2 - pred1
        writeRaster(diffRas, paste0("~//scratch//RasterDiff//",modelName,".tif"), overwrite=T)
        
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
  }
stopCluster(cl)

write.csv(pasmOut, "pasmAll.csv", row.names=FALSE)
