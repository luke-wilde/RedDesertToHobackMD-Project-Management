# Code to convert to residual forage
# ATTN: Need to run scaledNDVIfitToSpline on GEE first!!
# Luke Wilde
# 12.01.2024
library(terra)
library(tidyverse)
library(MerkleLabGIS)
library(MoveTools)
library(mapview)

#lyr <- bucket()

setwd("H:/My Drive/gee-export/VIIRS_NDVI_MeanSmooth/")

for(year in 2014:2023){
  
  #year = 2024

#collect the year Raster stack of mean-estimated daily scaled-NDVI 
ndvi <- rast(paste("Yearly_NDVI_",year,".tif",sep=""))

#collect and add forage biomass
afb <- crop(rast(paste("/vsicurl/https://pathfinder.arcc.uwyo.edu/merklelab/Landcover_RAP/RAP_",min(year,2023),"_Biomass_AnnualForbsGrasses.tif",sep="")),ext(ndvi))
pfb <- crop(rast(paste("/vsicurl/https://pathfinder.arcc.uwyo.edu/merklelab/Landcover_RAP/RAP_",min(year,2023),"_Biomass_PerennialForbsGrasses.tif",sep="")),ext(ndvi))

tot_fb <- afb + pfb

tot_fb1 <- extend(tot_fb, ndvi)
tot_fb2 <- resample(tot_fb1, ndvi)

resfor <- tot_fb2 * ndvi

rm(afb, pfb, tot_fb, ndvi)

writeRaster(resfor, filename = paste("C:/Users/lwilde2/Documents/PhD_Chapters/Datasets/ChapterOne_FremontFenceAnalysis/Forage Layers/YearlyResidualForage_",year,"_scaledNDVImeanInterpol.tif",sep=""))

rm(resfor)

print(paste("Finished with:", year))

}
