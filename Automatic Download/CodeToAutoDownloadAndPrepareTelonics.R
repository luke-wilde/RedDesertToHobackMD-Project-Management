# Access TDC via R and download collar data to computer

library(processx)
library(dplyr)
library(sf)

#### Globalstar ####
processx::run("C:/Program Files (x86)/Telonics/Data Converter/TDC.exe",              "/batch:D:/RD2H_GPSCollarData/BatchSettings_RDH_GlobalStar.xml")

#unclear what this part does - seems to repeat
system(paste(shQuote("C:/Program Files (x86)/Telonics/Data Converter/TDC.exe"),shQuote("/batch:D:/RD2H_GPSCollarData/BatchSettings_RDH_GlobalStar.xml")))


# Import tracks
# Create a list of the data you will import
path = "D:/RD2H_GPSCollarData/RD2H_Telonics_Data/IndividualRawGlobalstarDataFromTDC/"
ListOfInd_GS = list.files(path, pattern= "*.csv")



RDH_mule_Globalstar.df = NULL
for(i in ListOfInd_GS){
  # The skip parameter is because there is some meta information above where the recordings begin
  Individual.df = read.csv(paste0(path,"/",i), skip = 22, header = TRUE)
  Serial_Number = substr(i, 1, 7)
  Individual.df$CollarSerialNumber = Serial_Number
  print(paste0("Data importanted for individual: ", Serial_Number))
  RDH_mule_Globalstar.df = rbind(RDH_mule_Globalstar.df, Individual.df)
}



#### Iridium ####

#
processx::run("C:/Program Files (x86)/Telonics/Data Converter/TDC.exe", "/batch:D:/RD2H_GPSCollarData/BatchSettings_RDH_Iridium.xml")

system(paste(shQuote("C:/Program Files (x86)/Telonics/Data Converter/TDC.exe"), shQuote("/batch:D:/RD2H_GPSCollarData/BatchSettings_RDH_Iridium.xml")))


path = "D:/RD2H_GPSCollarData/RD2H_Telonics_Data/testdir/" #IndividualRawIridiumDataFromTDC

stat <- as.list(list.files(path, pattern= "*Statistics.csv"))
for(i in 2:length(stat)){unlink(paste(path, stat[[i]], sep=""), recursive = T, force = T)}

ListOfInd_IR = list.files(path, pattern= "*Complete.csv")


# Commented by LRW from this line to end on 10/13/21 - don't need given Anna's other code
# 
# RDH_mule_Iridium.df = NULL
# for(i in ListOfInd_IR){
#   # The skip parameter is because there is some meta information above where the recordings begin
#   Individual.df = read.csv(paste0(path,"/",i), skip = 22, header = TRUE)
#   Serial_Number = substr(i, 1, 7)
#   Individual.df$CollarSerialNumber = Serial_Number
#   print(paste0("Data importanted for individual: ", Serial_Number))
#   RDH_mule_Iridium.df = rbind(RDH_mule_Iridium.df, Individual.df)
# }
# 
# 
# str(RDH_mule_Globalstar.df)
# str(RDH_mule_Iridium.df)
# 
# RDH_mule_Iridium_sub.df <- RDH_mule_Iridium.df[,which(names(RDH_mule_Iridium.df) %in% names(RDH_mule_Globalstar.df))]
# 
# 
# #combine
# RDH_mule.df <- rbind(RDH_mule_Globalstar.df,RDH_mule_Iridium_sub.df)
# 
# 
# #filter out NA
# RDH_mule.df = RDH_mule.df[which(!(is.na(RDH_mule.df$GPS.Latitude))),]
# 
# #remove unwanted data 
# #RDH_mule.df <- RDH_mule.df[c("CollarSerialNumber", "GPS.Fix.Time", "GPS.Latitude", "GPS.Longitude" )]
# 
# # Change date time to POSIXct
# RDH_mule.df$GPS.Fix.Time <- as.POSIXct(RDH_mule.df$GPS.Fix.Time, format= "%Y.%m.%d %H:%M:%S", tz=Sys.time())
# 
# LastLoc_RDH_mule.df <- RDH_mule.df %>% group_by(CollarSerialNumber) %>% summarise(
#   Max.DateTime = max(GPS.Fix.Time),
#   Last.Lat = GPS.Latitude[which(GPS.Fix.Time==Max.DateTime)],
#   Last.Lon = GPS.Longitude[which(GPS.Fix.Time==Max.DateTime)],
#   
# )
# 
# ThreshDate = as.POSIXct("2021-10-01 01:38:30", format= "%Y-%m-%d %H:%M:%S", TZ=Sys.time())
# 
# LastLoc_RDH_mule.df <- LastLoc_RDH_mule.df[which(LastLoc_RDH_mule.df$Max.DateTime> ThreshDate),]
# 
# 
# projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# # convert data to an sf object
# LastLocs_TelonicsRDH_mule_sf <- st_as_sf(x = LastLoc_RDH_mule.df,                         
#                             coords = c("Last.Lon", "Last.Lat"),
#                             crs = projcrs)
# 
# library(mapview)
# mapview(LastLocs_TelonicsRDH_mule_sf )


