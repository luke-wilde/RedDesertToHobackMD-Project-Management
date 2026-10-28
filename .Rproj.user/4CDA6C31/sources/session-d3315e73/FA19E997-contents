library(tidyverse); library(sf); library(mapview); library(slider)

rm(list = ls())

setwd("C:/Users/lwilde2/Documents/")

 load(paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RD2H_CleanData.RData"))
 target.tz <- Sys.timezone()
 
 if(target.tz == "MST"){ message("correct TZ implimented")} else { target.tz = "MST"; message("TZ changed")}
 
 select <- dplyr::select
 filter <- dplyr::filter
 


# path <- "G:/.shortcut-targets-by-id/11TbL7-Qp6K3dPrbVA2L0_2mZRgzuXS5V/RDH_Sawyer/Shapefiles/"
# 
# # List all .shp files in the directory
# shp_files <- list.files(path, pattern = "\\.shp$", full.names = TRUE)
# 
# # Read and append all .shp files
# all_data <- lapply(shp_files, st_read)
# x <- do.call(rbind, all_data)
# 
# 
# 
# 
# x <- x %>% mutate(Mgtry = NA, AID = animal_id, CllrSrN = NA, Frequency = NA, Date = mdy(date), POSIXct = as.POSIXct(mdy_h(paste(date, hour)), tz = target.tz), Month = month, Day = day, Year = year(Date), dt = as.character(POSIXct + second(.1)), Time = str_sub(dt, start = 12, end = 19), HDOP = horizontal, JDate = yday(Date), Lat = latitude, Long = longitude, id_yr = paste(AID, Year, sep="_"), UTM_E = easting, UTM_N = northing) %>% dplyr::select(-dt) %>%  arrange(id_yr, POSIXct) %>% group_by(id_yr) %>% mutate(dist = st_distance(geometry, lead(geometry), by_element = TRUE), diff = as.numeric(difftime(lead(POSIXct), POSIXct, units = "secs")), speed = as.numeric(dist / as.numeric(diff))) %>% ungroup(); x %>% as.data.frame() %>% head()
# 
# 
# xx <- x %>% st_drop_geometry() %>% group_by(AID, id_yr, Date) %>% summarise(AvgFixRate = as.numeric(round(mean(diff, na.rm = T),0))/3600) %>% st_drop_geometry() %>% as.data.frame()  %>% group_by(AID, id_yr) %>% summarise(AvgFixRate = mean(AvgFixRate, na.rm = T), Complete = quantile(AvgFixRate,probs = .75, na.rm = T) < 6)
# xx %>% head()
# 
# # deem any collar with less than 25% missed fixes complete
# RDH_sawyer <- x %>% left_join(xx, by=c("AID","id_yr") )
# 
# 
# RDH_sawyer <- RDH_sawyer[,names(rdh_data.sf)]
# 
# quantile(RDH_sawyer$speed, probs = c(0,0.025,0.975,0.9995,1), na.rm =T)
# 
# # RDH_sawyer <- RDH_sawyer %>% filter(as.numeric(speed) <= quantile(RDH_sawyer$speed, probs = c(0.9995), na.rm =T))
# 
# 
# rdh_data.sf_sawyer<-RDH_sawyer
# 
# save(rdh_data.sf_sawyer, file = paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RDH_Sawyer.RData"))

load(paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RDH_Sawyer.RData"))

rdh_data.sf_sawyer %>% head()

x <- rdh_data.sf_sawyer %>% filter(Month == "07") %>% group_by(AID) %>% sample_n(3); mapview(x, zcol = "AID")

mgtry <- data.frame(AID = as.character(c(33, 11, 17, 27, 3, 7, 19, 38, 22, 30, 44, 50, 35,
  46, 4, 31, 24, 45, 20, 26, 12, 14, 25, 40, 29, 59,
  41, 8, 53, 57, 49, 51, 52,
  54)), Mgtry = 
c(rep("Long", 13),
  rep("Medium", 13),
  rep("Short", 7),
  "Resident"))

rdh_data.sf_sawyer <- rdh_data.sf_sawyer %>% select(-Mgtry) %>% left_join(., mgtry, by = "AID")

rdh_data.sf_sawyer <- rdh_data.sf_sawyer %>% mutate(AID = as.integer(AID), Date = as.character(Date), Month = as.double(Month), Day = as.double(Day)) %>% st_transform(., crs = st_crs(rdh_data.sf))
rdh_data.sf <- rdh_data.sf %>% mutate(POSIXct = with_tz(POSIXct, target.tz)) %>% mutate(diff = as.double(diff))
all_rdh_data.sf <- bind_rows(rdh_data.sf_sawyer,rdh_data.sf) %>% arrange(POSIXct, AID)

save(all_rdh_data.sf, file = paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RDH_GPSdata_HS+AC+LW+KM+MK_10.27.2026.RData"))
