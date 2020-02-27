## list climate models
modellist <- list.files("~//scratch//modelOut//", recursive=T, pattern="otherspp*", full.names=T) ## list all files with that climate model

mergedData <- do.call(rbind, lapply(modellist, read.csv))
write.csv(mergedData, "~/projects/def-sapna/afila/PASMmodels//othersppOut.csv", row.names=F)

modellist <- list.files("~//scratch//modelOut//", recursive=T, pattern="pasm*", full.names=T) ## list all files with that climate model


mergedData <- do.call(rbind, lapply(modellist, read.csv))
write.csv(mergedData, "~/projects/def-sapna/afila/PASMmodels//pasmOut.csv", row.names=F)



### new sedum
modellist <- list.files("E://PASMmodels//data//temp//", recursive=T, pattern=".csv", full.names=T) ## list all files with that climate model
mergedData <- do.call(rbind, lapply(modellist, read.csv))
write.csv(mergedData, "E://PASMmodels//data//temp//sedumRedAll.csv", row.names=F)
