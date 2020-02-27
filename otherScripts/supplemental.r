### Supplemental analyses

library(tidyverse)

## load dataset of different threshold
thresh <-  read.csv("data//thresholdResults.csv")

## Colourblind palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

ggplot(thresh, aes(x=as.factor(threshold), y=percentZero, fill=spp)) + geom_bar(position = "dodge", stat="identity", color="black") + scale_fill_manual(values=cbPalette) + 
  ylab("Percent of excluded observations") + xlab("Dispersal threshold (km)")  + theme(text = element_text(size=16),  
                                                                                      panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.title= element_blank()) 
  
