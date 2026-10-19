#Author: Luke Wilde
#Date: 10/13/2021
#code to automatically download vectronics data (camera collars)

library(devtools)
library(collar)
#devtools::install_github("huh/collar")


#set working directory
setwd("D:/RD2H_GPSCollarData/RD2H_Vectronics_Data/IndividualRawIridiumDataFromGPSPlusX/")

#locate access keys (E Reed has these)
path_keys <-   list.files("D:/RD2H_GPSCollarData/Vectronics Keys/", pattern = "*.keyx", full.names = T); path[1]

# 
# #create list that is filled
# collar <- list(NA, nrow(as.data.frame(path_keys)))
# 
# 
# for(i in 1:nrow(as.data.frame(path_keys))){
#   coll <- fetch_vectronics(
#   path_keys[i],
#   base_url = NULL,
#   after_data_id = NULL,
#   type = "gps",
#   count = FALSE,
#   start_date = NULL,
#   which_date = NULL,
#   rename_fun = adj_col_nms)
#   
#   assign(paste("Coll", i), path_keys[i]) <- coll}
# 
# 
# #create list
# collar <- list(NA, nrow(as.data.frame(path_keys)))
# 
# for(i in 1:nrow(as.data.frame(path_keys))){
#   mort_i <<- fetch_vectronics(
#     path_keys[i],
#     base_url = NULL,
#     after_data_id = NULL,
#     type = "gps",
#     count = FALSE,
#     start_date = NULL,
#     which_date = NULL,
#     rename_fun = adj_col_nms
#   )}




collar_46521 <- fetch_vectronics(
  path_keys[1],
  base_url = NULL,
  after_data_id = NULL,
  type = "gps",
  count = FALSE,
  start_date = NULL,
  which_date = NULL,
  rename_fun = adj_col_nms
)

collar_46523 <- fetch_vectronics(
  path_keys[2],
  base_url = NULL,
  after_data_id = NULL,
  type = "gps",
  count = FALSE,
  start_date = NULL,
  which_date = NULL,
  rename_fun = adj_col_nms
)

mort_46521 <- fetch_vectronics(
  path_keys[1],
  base_url = NULL,
  after_data_id = NULL,
  type = "mor",
  count = FALSE,
  start_date = NULL,
  which_date = NULL,
  rename_fun = adj_col_nms
)

mort_46523 <- fetch_vectronics(
  path_keys[2],
  base_url = NULL,
  after_data_id = NULL,
  type = "mor",
  count = FALSE,
  start_date = NULL,
  which_date = NULL,
  rename_fun = adj_col_nms
)



dt <- as.character(format(Sys.time(), format = "%Y%m%d%H%M%S"))


#### format collar gps data #### - for the formatting, check what information Anna keeps for the final dataset
#current format

head(as.data.frame(collar_46523))

#first collar
collar_46523$scts <- as.POSIXct(gsub("T"," ",collar_46523$scts))
collar_46523$acquisitiontime<- as.POSIXct(gsub("T"," ",collar_46523$acquisitiontime))


collar_46523 <- data.frame("CollarID" = collar_46523$idcollar, 
                           "UTC_Date" = format(as.Date(collar_46523$acquisitiontime),"%m/%d/%Y"),
                           "UTC_Time" = format(as.POSIXct(collar_46523$acquisitiontime), "%H:%M:%S"),
                           "LMT_Date"= format(as.Date(collar_46523$acquisitiontime),"%m/%d/%Y"),
                           "LMT_Time" = format(as.POSIXct(collar_46523$acquisitiontime), "%H:%M:%S"), 
                           "Origin" = rep("Iridium", nrow(collar_46523)),
                           "SCTS_Date" = format(as.Date(collar_46523$scts),"%m/%d/%Y"), 
                           "SCTS_Time" = format(as.POSIXct(collar_46523$scts),"%H:%M:%S"),
                           "ECEF_X [m]" = collar_46523$ecefx, 
                           "ECEF_Y [m]" = collar_46523$ecefy, 
                           "ECEF_Z [m]" = collar_46523$ecefz, 
                           "Latitude [°]" = collar_46523$latitude, 
                           "Longitude [°]" = collar_46523$longitude, 
                           "Height [m]" = collar_46523$height, 
                           "DOP"  = collar_46523$dop, 
                           "FixType" = collar_46523$idfixtype,
                           "3D_Error [m]" = NA,
                           "Sats" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Mort. Status"  = collar_46523$idmortalitystatus, 
                           "Activity" = collar_46523$activity, 
                           "Main [V]"  = collar_46523$mainvoltage, 
                           "Beacon [V]" = collar_46523$backupvoltage, 
                           "Temp [°C]" = collar_46523$temperature, 
                           "No"= seq(1,nrow(collar_46523),1), 
                           "No"= collar_46523$idposition)

#other collar
collar_46521$scts <- as.POSIXct(gsub("T"," ",collar_46521$scts))
collar_46521$acquisitiontime<- as.POSIXct(gsub("T"," ",collar_46521$acquisitiontime))



collar_46521 <- data.frame("CollarID" = collar_46521$idcollar, 
                           "UTC_Date" = format(as.Date(collar_46521$acquisitiontime),"%m/%d/%Y"),
                           "UTC_Time" = format(as.POSIXct(collar_46521$acquisitiontime), "%H:%M:%S"),
                           "LMT_Date"= format(as.Date(collar_46521$acquisitiontime),"%m/%d/%Y"),
                           "LMT_Time" = format(as.POSIXct(collar_46521$acquisitiontime), "%H:%M:%S"), 
                           "Origin" = rep("Iridium", nrow(collar_46521)),
                           "SCTS_Date" = format(as.Date(collar_46521$scts),"%m/%d/%Y"), 
                           "SCTS_Time" = format(as.POSIXct(collar_46521$scts),"%H:%M:%S"),
                           "ECEF_X [m]" = collar_46521$ecefx, 
                           "ECEF_Y [m]" = collar_46521$ecefy, 
                           "ECEF_Z [m]" = collar_46521$ecefz, 
                           "Latitude [°]" = collar_46521$latitude, 
                           "Longitude [°]" = collar_46521$longitude, 
                           "Height [m]" = collar_46521$height, 
                           "DOP"  = collar_46521$dop, 
                           "FixType" = collar_46521$idfixtype,
                           "3D_Error [m]" = NA,
                           "Sats" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Sat" = NA,
                           "C/N" = NA,
                           "Mort. Status"  = collar_46521$idmortalitystatus, 
                           "Activity" = collar_46521$activity, 
                           "Main [V]"  = collar_46521$mainvoltage, 
                           "Beacon [V]" = collar_46521$backupvoltage, 
                           "Temp [°C]" = collar_46521$temperature, 
                           "No"= seq(1,nrow(collar_46521),1), 
                           "No"= collar_46521$idposition)

positions <- as.list(list.files("D:/RD2H_GPSCollarData/RD2H_Vectronics_Data/IndividualRawIridiumDataFromGPSPlusX/", pattern= "*.csv"))
for(i in 1:length(positions)){unlink(paste("D:/RD2H_GPSCollarData/RD2H_Vectronics_Data/IndividualRawIridiumDataFromGPSPlusX/", positions[[i]], sep=""), recursive = T, force = T)}

write.csv(collar_46523, paste("./GPS_Collar46523_",".csv", sep=dt), append = F)
write.csv(collar_46521, paste("./GPS_Collar46521_",".csv", sep=dt), append = F)




#### format collar mort data ####

#current format
head(mort_46523)

#first collar
mort_46523$scts <- as.POSIXct(gsub("T"," ",mort_46523$scts))
mort_46523$acquisitiontime<- as.POSIXct(gsub("T"," ",mort_46523$acquisitiontime))

mort_46523 <- data.frame("CollarID" = mort_46523$idcollar, 
                         "UTC_Date" = format(as.Date(mort_46523$acquisitiontime),"%m/%d/%Y"),
                         "UTC_Time" = format(as.POSIXct(mort_46523$acquisitiontime), "%H:%M:%S"),
                         "LMT_Date"= format(as.Date(mort_46523$acquisitiontime),"%m/%d/%Y"),
                         "LMT_Time" = format(as.POSIXct(mort_46523$acquisitiontime), "%H:%M:%S"), 
                         "Origin" = rep("Iridium", nrow(mort_46523)), 
                         "SCTS_Date" = format(as.Date(mort_46523$scts),"%m/%d/%Y"), 
                         "SCTS_Time" = format(as.POSIXct(mort_46523$scts),"%H:%M:%S"),
                         "Kind" = rep("Mortality", nrow(mort_46523)),
                         "AnimalID" = NA, 
                         "GroupID" = NA)


#first collar
mort_46521$scts <- as.POSIXct(gsub("T"," ",mort_46521$scts))
mort_46521$acquisitiontime<- as.POSIXct(gsub("T"," ",mort_46521$acquisitiontime))

mort_46521 <- data.frame("CollarID" = mort_46521$idcollar, 
                         "UTC_Date" = format(as.Date(mort_46521$acquisitiontime),"%m/%d/%Y"),
                         "UTC_Time" = format(as.POSIXct(mort_46521$acquisitiontime), "%H:%M:%S"),
                         "LMT_Date"= format(as.Date(mort_46521$acquisitiontime),"%m/%d/%Y"),
                         "LMT_Time" = format(as.POSIXct(mort_46521$acquisitiontime), "%H:%M:%S"), 
                         "Origin" = rep("Iridium", nrow(mort_46521)), 
                         "SCTS_Date" = format(as.Date(mort_46521$scts),"%m/%d/%Y"), 
                         "SCTS_Time" = format(as.POSIXct(mort_46521$scts),"%H:%M:%S"),
                         "Kind" = rep("Mortality", nrow(mort_46521)),
                         "AnimalID" = NA, 
                         "GroupID" = NA)


mort <- as.list(list.files("D:/RD2H_GPSCollarData/Vectronics_mort_messages/", pattern= "*.csv"))
for(i in 1:length(mort)){unlink(paste("D:/RD2H_GPSCollarData/Vectronics_mort_messages/", mort[[i]], sep=""), recursive = T, force = T)}

write.csv(mort_46523, paste("D:/RD2H_GPSCollarData/Vectronics_mort_messages/MOR_Collar46523_",".csv",sep=dt), append = F)
write.csv(mort_46521, paste("D:/RD2H_GPSCollarData/Vectronics_mort_messages/MOR_Collar46521_",".csv",sep=dt), append = F)




















# 
# 
# require(collar)
# library(rvest)
# library(purrr)
# library(httr)
# library(pipeR)
# library(magrittr)
# 
# 
# 
# 
# url_login <- "https://webservice.lotek.com/"
# user = "aortega5"
# pass = "WyomingElk24!"
# 
# 
# #create a web session with the desired log in address
# pgsession <- session(url_login, tz="UTC")
# pgform<-html_form(pgsession)[[1]]
# filled_form<- html_form_set(pgform,UserName=user,Password=pass)
# results <- session_submit(pgsession, filled_form)
# 
# content(results$response, "text")
# 
# #pre allocate the final results dataframe.
# #results<-data.frame()  
# 
# lotek_form <- read_html(results) %>% 
#   html_nodes("form") %>% 
#   magrittr::extract2(1) %>% 
#   html_form()
# 
# html_element()
# 
# lot_val <- set_values(lotek_form,
#                       )
# 
# 
# 
# 
# map_df(1:4,function(i)){
#   page <- read_html(sprintf(url_login,i))
#   
#   data.frame(IssueID = html_text(html_nodes()))
# }