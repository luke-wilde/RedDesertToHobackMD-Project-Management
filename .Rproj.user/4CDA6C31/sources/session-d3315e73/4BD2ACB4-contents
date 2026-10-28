#---------------------------#
# Code to Apply Speed and Scatter Filters ####
# Written by Luke Wilde #
#---------------------------#

rm(list = ls())

#packages
library(sf)
library(mapview)
library(tidyverse)
library(lubridate)
library(devtools)
#install_github("jmerkle1/MerkleLab-Code-Repository", subdir="MoveTools")
library(MoveTools)
library(plotly); #install.packages("plotly")
library(adehabitatHR)
library(slider) #install.packages("slider")

target.tz <- Sys.timezone()

if(target.tz == "America/Denver"){ message("correct TZ implimented")} else { target.tz = "America/Denver"; message("TZ changed")}

select <- dplyr::select
filter <- dplyr::filter

setwd("C:/Users/lwilde2/Documents/")


# #----------------------------#
# # Load up gps data (CSV)  ####
# #----------------------------#
# data <- read.csv("RD2H_AllData.csv", 
#                  header=T, row.names=NULL, stringsAsFactors=FALSE)
# head(data)
# str(data)
# #table(is.na(data))   # are there any NAs? If so, figure out what columns they are in
# 
# # load("RD2H_AllData.RData")
# # datasf <- st_as_sf(finalshape)
# 
# data <- data %>% rename(UTM_E = coords.x1, UTM_N = coords.x2)
# 
# # make spatially aware
# # what is the projection that your data are in?  You'll need to figure this out and specify below - use Step2 to check
# proj <- "+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs"
# projnew <- "+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs+towgs84=0,0,0"
# 
# # turn into sf object
# data <- st_as_sf(data, coords=c("UTM_E","UTM_N"), dim="XY", crs=proj) %>% mutate(POSIXct = as.POSIXct(POSIXct, "%Y-%m-%d %H:%M:%S", tz = target.tz))
# 
# 
# #transform
# data <- st_transform(data, crs = projnew)
# 
# #fix month, day, year issue
# data <- data %>% mutate(Year = year(POSIXct), Month = month(POSIXct), Day = mday(POSIXct), JDate = yday(POSIXct))
# 
# #create coords
# data[,c("UTM_E","UTM_N")] <- st_coordinates(data)
# 
# head(data)
# str(data)
# rm(proj, projnew)

load(paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RD2H_AllData.RData"))



data <- clean.Data2
rm(clean.Data2)

#check for NA by column
apply(data, 2, function(x){sum(is.na(x))}) # OPTIONAL: figure out how many NAs in each column, should be all 0s except HDOP for which some early collars did not record.




# calculate movement parameters and remove based on speed -- ACOrtega recommends 3 m/s ####
#----------------------------------#

class(data$POSIXct)

#Check datetime is working right
data$POSIXct2 <- as.POSIXct(data$POSIXct, format = "%Y-%m-%d %H:%M:%S", tz = target.tz)
class(data$POSIXct2)
range(na.omit(data$POSIXct2))
table(is.na(data$POSIXct2))

data$POSIXct2 <- as.POSIXct(ifelse(is.na(data$POSIXct2), as.POSIXct(paste(data$POSIXct,"00:00:01",sep=" "), format = "%Y-%m-%d %H:%M:%S", tz = target.tz), as.POSIXct(data$POSIXct2, format = "%Y-%m-%d %H:%M:%S", tz = target.tz)))


class(data$POSIXct2)
table(is.na(data$POSIXct2))

data$POSIXct <- data$POSIXct2


data <- data %>% dplyr::select(-POSIXct2)

tmp <- data %>% mutate(id_yr = paste(AID,"_",Year,sep="")) %>%  arrange(id_yr, POSIXct) %>% group_by(id_yr) %>% mutate(dist = st_distance(geometry, lag(geometry), by_element = TRUE),
                                                                                                                       diff = as.difftime(lead(POSIXct) - POSIXct, units = "secs"),
                                                                                                                       speed = as.numeric(dist / as.numeric(diff))) %>% ungroup()



tmp[which(as.numeric(tmp$speed) == "Inf"),"speed"] <- NA
range(na.omit(tmp$speed))

options(scipen = 999)
quantile(tmp$speed, probs = c(0,0.025,0.975,0.9995,1), na.rm =T)

# data.sf <- bind_rows(tmp %>% filter(diff < 3600*2.2) %>% filter(as.numeric(speed) < 3), tmp %>% filter(diff >= 3600*2.2))

data.sf <- tmp %>% filter(as.numeric(speed) <= pmax(3, quantile(tmp$speed, probs = c(0.9995), na.rm =T)))

message(paste("This code removed", nrow(data) - nrow(data.sf), "locations which exceeded the speed barrier of ",pmax(3, quantile(tmp$speed, probs = c(0.9995), na.rm =T)) ," m/s."))

# find outliers that lie beyond the 99% boundary ####
#----------------------------------------------#

footprint <- mcp(as(data.sf %>% dplyr::filter(Mgtry == "Long") %>% dplyr::select(geometry), "Spatial"), percent = 99.99)
foot_sf <- st_as_sf(footprint)
mapview(foot_sf)


inlier <- st_filter(data.sf, foot_sf)

outliers <- setdiff(data.sf, inlier)


#how many outliers in each id_yr
out <- outliers %>% group_by(id_yr) %>% summarise(n = n()) %>% arrange(n)


outliers <- outliers %>% mutate( dist2StudyArea = round(st_distance(outliers, foot_sf, by_element = F)/1000,0)) %>% arrange(dist2StudyArea)

mapview(foot_sf) + mapview(outliers, zcol = "dist2StudyArea")


#get rid of those > 100 km, as that is the appropriate cutoff
#changed to 100 to keep in 
outliers <- outliers %>% dplyr::filter(as.numeric(dist2StudyArea) > 100) # %>% st_drop_geometry() %>% as.data.frame()

keep959 <- outliers %>% filter(AID == 959)

clean.data.sf <- data.sf %>% dplyr::anti_join(., outliers %>% st_drop_geometry(), by = c("id_yr","POSIXct"))

clean.data.sf <- rbind(clean.data.sf, keep959 %>% select(names(clean.data.sf))) %>% arrange(AID, POSIXct)

paste("This code removed", nrow(data.sf)- nrow(clean.data.sf), "locations which exceeded 100 km from the study area.")


str(clean.data.sf)



# figure out fix rate ####
# -----------------------#

# ATTN
### NEW CHANGE to integrate ####
# ATTN

clean.data.sf <- clean.data.sf %>% arrange(id_yr, POSIXct)

x <- clean.data.sf %>% st_drop_geometry() %>% group_by(AID, id_yr, Date) %>% summarise(AvgFixRate = as.numeric(round(mean(diff, na.rm = T),0))/3600) %>% st_drop_geometry() %>% as.data.frame()  %>% group_by(AID, id_yr) %>% summarise(AvgFixRate = mean(AvgFixRate, na.rm = T), Complete = quantile(AvgFixRate,probs = .75, na.rm = T) < 6)
x %>% head()

# deem any collar with less than 25% missed fixes complete
clean.data.sf <- clean.data.sf %>% left_join(x, by=c("AID","id_yr") )









rdh_data.sf <- clean.data.sf %>% dplyr::select(-c(DayOfYear))

# rdh_data.sf %>% group_by(id_yr) %>% mutate(n = n()) %>% ungroup() %>% filter(diff < (4.2*3600)) %>% group_by(id_yr) %>% mutate(n1 = n(), prop.drop = (n-n1)/n) %>% ungroup() # %>% group_by(id_yr) %>% filter(prop.drop < .05) 




#change for camera collars and 10 min fixes

camcolls <- c("46524","46523","46521")
ten_minfix <- c("48626", "48631", "751713A")
rdh_data.sf <- rdh_data.sf %>% mutate(Complete = as.integer(ifelse(CllrSrN %in% c(camcolls,ten_minfix), 1, Complete)))

#check that these were kept as complete
rdh_data.sf %>% filter(CllrSrN %in% ten_minfix) %>% select(AvgFixRate, Complete) %>% summary()



table(rdh_data.sf$Complete)

# -------------------------#
# save cleaned database ####
# -------------------------#


# write out as rds object (more efficient). I suggest you do this, and then 
# you'll have a clean database ready for each lab in the class
save(rdh_data.sf, file=paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RD2H_CleanData.RData"))
write.csv(rdh_data.sf, paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RD2H_CleanData.csv"), row.names = F)

#write out as shapefile, if you want 
rdh_data.sf %>% 
  mutate(POSIXct=as.character(POSIXct)) %>% # when writing shapefile with st_write(), need to switch posix to character.
  st_write(paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RD2H_CleanAllData"), driver="ESRI Shapefile", 
           delete_layer = TRUE)  # this removes previously saved shapefile with the same name 


showConnections()


