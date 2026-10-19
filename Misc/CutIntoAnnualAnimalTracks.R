# code to split into individual AID_yr
# Author Luke Wilde

# load library
 library(dplyr)
 library(sf)
 library(sp)
 
 #load data
 all_data <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllData.csv"); head(all_data); str(all_data)

 
 all_data$Year <- as.numeric(all_data$Year)

 #filter to just the year you want
 date.int <- as.POSIXct("2021-01-01 01:00:00")
 year.int <- 2021
 
 all_data_CurrentYr <- all_data %>% filter(POSIXct > date.int)
 range(all_data_CurrentYr$POSIXct) 
 
 all_data_CurrentYr_sf <- st_as_sf(all_data_CurrentYr, coords = c("Long", "Lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
 
 #loop through all animals to export a .shp file
 sub <- list(NA, length(unique(all_data_CurrentYr$AID))) #length(unique(all_data_CurrentYr$AID))

 setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/AnnualGPSCollarData/2021/GPSDataByAnimalNew/")
 
 #unique(all_data_CurrentYr$CllrSrN)
 for(i in 1:length(unique(all_data_CurrentYr$AID))) {
   sub[[i]] <- as.data.frame(all_data_CurrentYr_sf %>% filter(AID == as.character(unique(all_data_CurrentYr$AID)[i])))
   sub_sf <- st_as_sf(sub[[i]])
   tmp <- paste0("RD2H", sep="_", as.character((sub_sf$AID[1])))
   tmp2 <- paste0(tmp, sep="_", as.character(sub_sf$Year[1]))
   st_write(sub_sf, paste0(tmp2, sep="", ".shp"))
 }
 