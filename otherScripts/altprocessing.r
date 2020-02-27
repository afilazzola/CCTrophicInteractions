## load packages 
library(raster)
library(rgdal)

## load dataset list
dempath <- read.csv("data//elevation//demlist.csv")

## download file
## run loop to download file
for(i in 1:nrow(dempath)){
## download file
download.file(as.character(dempath[i,"downloadURL"]), destfile="data//elevation//temp.zip")
outdir <- "data//elevation//unzipped" ## specify directory to unzip`
unzip("data//elevation//temp.zip", exdir=outdir) ## unzip directory

rasfile <- list.files("data//elevation//unzipped//", pattern="w001001x.adf", recursive=T) ## list raster file in directory
r1 <- raster(paste(outdir,rasfile, sep="/")) ## read raster
names(r1) <- dirname(rasfile) ## rename raster to folder directory name
writeRaster(r1, paste0("data//elevation//DEMtiff//",dirname(rasfile),".tif")) ## write raster
unlink("data//elevation//unzipped//*", recursive = T) ## delete all temporary files that were unzipped
Sys.sleep(3) ## wait 3 seconds to ensure files are deleted
gc() ## clear cache
print(i) ## print iteration
}
