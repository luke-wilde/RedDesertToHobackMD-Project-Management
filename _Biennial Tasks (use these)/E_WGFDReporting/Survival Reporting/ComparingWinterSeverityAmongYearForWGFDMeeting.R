# For Monday Meeting on March Captures

gc()
rm(list=ls())


library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(patchwork)
library(dplyr)


# NEED INFO on:
# Number of mortalities this winter compared to past 5 winters
# Snowdepth and temperature this winter and last two weeks compared to past 5
# Capture mortalities in more severe winters
# 

# Method
# Use a subset of where the deer are to extract SNODAS and DAYMET info

load("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_CleanAllData.RData")
data <- findata 

attr(data$VAR, "ATT") <- NULL

winters <- data %>% dplyr::filter(Month %in% c(1,2)) %>% filter(Lat <= 42.0494039)
Prospectors <- data %>% dplyr::filter(Month %in% c(1,2)) %>% filter(Lat > 42.310816)


wint_sub <- winters %>% group_by(Year) %>% sample_n(2000, replace = T) 
Prosp_sub <- Prospectors  %>% group_by(Year) %>% sample_n(2000, replace = T) 

#-----------------------------------#
# mortalities in past winters ####
DeerID <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_10MARCH2023.csv") %>% filter(AID <= 699)
str(DeerID)
DeerID$StopDate <- as.POSIXct(DeerID$StopDate, format = "%m/%d/%Y", tz = "MST")
DeerID$StartDate <- as.POSIXct(DeerID$StartDate, format = "%m/%d/%Y", tz = "MST")
last <- DeerID %>% arrange(AID, desc(StartDate)) %>% group_by(AID) %>% top_n(1, StartDate)
Dead <- last %>% filter(!is.na(StopDate))
Alive <- last %>% filter(is.na(StopDate))
winter_morts <- Dead %>% mutate(Month = month(StopDate), Year = year(StopDate), J = yday(StopDate)) %>% filter(Month %in% c(12,1,2,3))

winter_morts <- winter_morts %>% arrange(Year, Month)

winter_morts$Year <- ifelse(winter_morts$J > 343, winter_morts$Year + 1, winter_morts$Year)

t <- winter_morts %>% filter(J >= 70 & J <= 343) #70 is March 9 or 10, depending on leap year

winter_mort_data <- winter_morts %>% filter(!AID %in% t$AID) %>% group_by(Year) %>% summarise(n = n()) 
winter_mort_data


#----------------------------#
# snowdepth ####
source("Z:/Snodas/code/extract.snodas.R")

wint_pre22 <- wint_sub %>% filter(Year < 2022) %>% mutate(Date = as.POSIXct(Date)) %>% as.data.frame() %>% st_as_sf(., coords = c("UTM_E", "UTM_N"), crs = "+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs")
Prosp_pre22 <- Prosp_sub %>% filter(Year < 2022) %>% mutate(Date = as.POSIXct(Date)) %>% as.data.frame() %>% st_as_sf(., coords = c("UTM_E", "UTM_N"), crs = "+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs")

wint_post22 <- wint_sub %>% filter(Year >= 2022) %>% mutate(Date = as.POSIXct(Date)) %>% as.data.frame() %>% st_as_sf(., coords = c("UTM_E", "UTM_N"), crs = "+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs")
Prosp_post22 <- Prosp_sub %>% filter(Year >= 2022) %>% mutate(Date = as.POSIXct(Date)) %>% as.data.frame() %>% st_as_sf(., coords = c("UTM_E", "UTM_N"), crs = "+proj=utm +zone=12 +datum=NAD83 +units=m +no_defs")

wint_pre22$Date <- wint_pre22$Date
wint_post22$Date <- wint_post22$Date

#------------------------#
# extract snodas function ####
#function starts 
# XYdata = wint_pre22 # this is a sf object with points
# SNODASfolder="Z:/Snodas/snowdepth/"  # this is the snodas folder with 1 tif file per julien day for the product in question
# datesname="Date"
# 
# 
# 
#   # prepare packages
#   if(all(c("raster","sf","parallel","stringr") %in% installed.packages()[,1])==FALSE)
#     stop("You must install the following packages: raster, sf, stringr, and parallel")
#   require("sf")
#   require("raster")
#   require("parallel")
#   require("stringr")
#   
#   # some checks and preparation
#   if(inherits(XYdata, "sf") == FALSE) 
#     stop("XYdata is not a sf object")
#   
#   xyorig <- XYdata  # save the original database as a separate file name (for later)
#   XYdata <- XYdata[,datesname]    # reduce the database down to save memory during parallel processing
#   names(XYdata)[1] <- "date"
#   
#   dates <- XYdata$date   # grab out the dates separately ######I changed this from dates <- st_set_geometry(XYdata, NULL)[,datesname] because it kept creating a tibble df and not a vector, which is what we needed
#   if(inherits(dates,"POSIXct") != TRUE) 
#     stop("Your datesname column is not POSIXct")
#   if(any(is.na(dates) == TRUE)) 
#     stop("You have NAs in your datesname column")
#   
#   drs <- list.files(SNODASfolder, ".tif$")
#   tz <- attr(dates,"tzone")
#   # update dates to format of snodas files
#   dates <- paste0(as.numeric(strftime(dates, format = "%Y", tz = tz)), 
#                   str_pad(as.numeric(strftime(dates, format = "%m", tz = tz)), 2, side="left",pad=0), 
#                   str_pad(as.numeric(strftime(dates, format = "%d", tz = tz)), 2, side="left",pad=0))   # this is the combos of years and juls
#   unique_dates <- unique(dates)
#   
#   #get and organize the name of the product
#   productName <- str_split_fixed(sub(".tif","",drs), "_",3)[,3]
#   if(length(table(productName))!= 1)
#     stop("You have extra files in your SNODASfolder. They should only be daily rasters (in tif format) of the same product.")
#   productName <- productName[1]
#   
#   fls <- paste0(SNODASfolder,"/SNODAS_",unique_dates, "_", productName, ".tif")
#   if(any(file.exists(fls)==FALSE))
#     print(paste0("Warning: You have ",length(which(file.exists(fls)==FALSE)), " dates outside of ", length(fls), " that are outside of the data in your snodas folder."))
#   
#   # reproject xy data to projection of snodas
#   r <- raster(paste0(SNODASfolder, "/", drs[1]))
#   XYdata <- st_transform(XYdata, crs=projection(r))
#   
#   # prepare parallel processing
#   no_cores <- detectCores()-1
#   print(paste0("Initiating parallel processing on ", no_cores, " cores."))
#   clust <- makeCluster(no_cores) 
#   parallel::clusterExport(clust, envir=environment(),
#                           varlist=c("dates","XYdata","unique_dates","fls"))
#   
#   # start parallel loop
#   vals <- do.call(c, clusterApplyLB(clust, 1:length(unique_dates), function(i){
#     require("raster")
#     require("sf")
#     
#     # bring in raster of the year/day of question
#     r <- try(raster(fls[i]),silent=TRUE)
#     
#     if(class(r)=="try-error"){
#       print(paste0("Warning: The following day was not found in the SNODAS folder: ", unique_dates[i]))
#       return(rep(NA, length(dates[dates == unique_dates[i]])))   # return NAs if the dates fall outside of snodas data
#     }else{
#       return(extract(r, XYdata[dates == unique_dates[i],]))   # extract the snodas data 
#     }
#     
#   }))   # end of loop over unique dates
#   stopCluster(clust)
#   
#   if(length(vals) != nrow(XYdata))
#     stop("There is a problem here. If you get this message, talk to Jerod.")
#   
#   # add column to origional database, then rename it with the product name
#   xyorig$temp12345 <- vals
#   names(xyorig)[names(xyorig)=="temp12345"] <- paste0("Snodas_",productName)
#   
# data_14t21_depth <- xyorig 
#function ends 

####
#function starts
# 
# XYdata = Prosp_pre22 # this is a sf object with points
# SNODASfolder="Z:/Snodas/snowdepth/"  # this is the snodas folder with 1 tif file per julien day for the product in question
# datesname="Date"
# 
# 
# 
# # prepare packages
# if(all(c("raster","sf","parallel","stringr") %in% installed.packages()[,1])==FALSE)
#   stop("You must install the following packages: raster, sf, stringr, and parallel")
# require("sf")
# require("raster")
# require("parallel")
# require("stringr")
# 
# # some checks and preparation
# if(inherits(XYdata, "sf") == FALSE) 
#   stop("XYdata is not a sf object")
# 
# xyorig <- XYdata  # save the original database as a separate file name (for later)
# XYdata <- XYdata[,datesname]    # reduce the database down to save memory during parallel processing
# names(XYdata)[1] <- "date"
# 
# dates <- XYdata$date   # grab out the dates separately ######I changed this from dates <- st_set_geometry(XYdata, NULL)[,datesname] because it kept creating a tibble df and not a vector, which is what we needed
# if(inherits(dates,"POSIXct") != TRUE) 
#   stop("Your datesname column is not POSIXct")
# if(any(is.na(dates) == TRUE)) 
#   stop("You have NAs in your datesname column")
# 
# drs <- list.files(SNODASfolder, ".tif$")
# tz <- attr(dates,"tzone")
# # update dates to format of snodas files
# dates <- paste0(as.numeric(strftime(dates, format = "%Y", tz = tz)), 
#                 str_pad(as.numeric(strftime(dates, format = "%m", tz = tz)), 2, side="left",pad=0), 
#                 str_pad(as.numeric(strftime(dates, format = "%d", tz = tz)), 2, side="left",pad=0))   # this is the combos of years and juls
# unique_dates <- unique(dates)
# 
# #get and organize the name of the product
# productName <- str_split_fixed(sub(".tif","",drs), "_",3)[,3]
# if(length(table(productName))!= 1)
#   stop("You have extra files in your SNODASfolder. They should only be daily rasters (in tif format) of the same product.")
# productName <- productName[1]
# 
# fls <- paste0(SNODASfolder,"/SNODAS_",unique_dates, "_", productName, ".tif")
# if(any(file.exists(fls)==FALSE))
#   print(paste0("Warning: You have ",length(which(file.exists(fls)==FALSE)), " dates outside of ", length(fls), " that are outside of the data in your snodas folder."))
# 
# # reproject xy data to projection of snodas
# r <- raster(paste0(SNODASfolder, "/", drs[1]))
# XYdata <- st_transform(XYdata, crs=projection(r))
# 
# # prepare parallel processing
# no_cores <- detectCores()-1
# print(paste0("Initiating parallel processing on ", no_cores, " cores."))
# clust <- makeCluster(no_cores) 
# parallel::clusterExport(clust, envir=environment(),
#                         varlist=c("dates","XYdata","unique_dates","fls"))
# 
# # start parallel loop
# vals <- do.call(c, clusterApplyLB(clust, 1:length(unique_dates), function(i){
#   require("raster")
#   require("sf")
#   
#   # bring in raster of the year/day of question
#   r <- try(raster(fls[i]),silent=TRUE)
#   
#   if(class(r)=="try-error"){
#     print(paste0("Warning: The following day was not found in the SNODAS folder: ", unique_dates[i]))
#     return(rep(NA, length(dates[dates == unique_dates[i]])))   # return NAs if the dates fall outside of snodas data
#   }else{
#     return(extract(r, XYdata[dates == unique_dates[i],]))   # extract the snodas data 
#   }
#   
# }))   # end of loop over unique dates
# stopCluster(clust)
# 
# if(length(vals) != nrow(XYdata))
#   stop("There is a problem here. If you get this message, talk to Jerod.")
# 
# # add column to origional database, then rename it with the product name
# xyorig$temp12345 <- vals
# names(xyorig)[names(xyorig)=="temp12345"] <- paste0("Snodas_",productName)
#
#prosp_14t21_depth <- xyorig 
#function ends


#-----------------------------------#

newSnodas <- "G:/Shared drives/wyo-coop-wilde/GIS/SNODAS/Finalized/"

#----------------------------------#
# same with daymet? ####
# #function starts 
# 
# XYdata = wint_post22 # this is a sf object with points
# SNODASfolder= newSnodas  # this is the snodas folder with 1 tif file per julien day for the product in question
# datesname="Date"
# 
# # prepare packages
# if(all(c("raster","sf","parallel","stringr") %in% installed.packages()[,1])==FALSE)
#   stop("You must install the following packages: raster, sf, stringr, and parallel")
# require("sf")
# require("raster")
# require("parallel")
# require("stringr")
# 
# # some checks and preparation
# if(inherits(XYdata, "sf") == FALSE) 
#   stop("XYdata is not a sf object")
# 
# xyorig <- XYdata  # save the original database as a separate file name (for later)
# XYdata <- XYdata[,datesname]    # reduce the database down to save memory during parallel processing
# names(XYdata)[1] <- "date"
# 
# dates <- XYdata$date   # grab out the dates separately ######I changed this from dates <- st_set_geometry(XYdata, NULL)[,datesname] because it kept creating a tibble df and not a vector, which is what we needed
# if(inherits(dates,"POSIXct") != TRUE) 
#   stop("Your datesname column is not POSIXct")
# if(any(is.na(dates) == TRUE)) 
#   stop("You have NAs in your datesname column")
# 
# drs <- list.files(SNODASfolder, ".tif$")
# tz <- attr(dates,"tzone")
# # update dates to format of snodas files
# dates <- paste0(as.numeric(strftime(dates, format = "%Y", tz = tz)), 
#                 str_pad(as.numeric(strftime(dates, format = "%m", tz = tz)), 2, side="left",pad=0), 
#                 str_pad(as.numeric(strftime(dates, format = "%d", tz = tz)), 2, side="left",pad=0))   # this is the combos of years and juls
# unique_dates <- unique(dates)
# 
# #get and organize the name of the product
# productName <- str_split_fixed(sub(".tif","",drs), "_",3)[,3]
# if(length(table(productName))!= 1)
#   stop("You have extra files in your SNODASfolder. They should only be daily rasters (in tif format) of the same product.")
# productName <- productName[1]
# 
# fls <- paste0(SNODASfolder,"/SNODAS_",unique_dates, "_", productName, ".tif")
# if(any(file.exists(fls)==FALSE))
#   print(paste0("Warning: You have ",length(which(file.exists(fls)==FALSE)), " dates outside of ", length(fls), " that are outside of the data in your snodas folder."))
# 
# # reproject xy data to projection of snodas
# r <- raster(paste0(SNODASfolder, "/", drs[1]))
# XYdata <- st_transform(XYdata, crs=projection(r))
# 
# # prepare parallel processing
# no_cores <- detectCores()-1
# print(paste0("Initiating parallel processing on ", no_cores, " cores."))
# clust <- makeCluster(no_cores) 
# parallel::clusterExport(clust, envir=environment(),
#                         varlist=c("dates","XYdata","unique_dates","fls"))
# 
# # start parallel loop
# vals <- do.call(c, clusterApplyLB(clust, 1:length(unique_dates), function(i){
#   require("raster")
#   require("sf")
#   
#   # bring in raster of the year/day of question
#   r <- try(raster(fls[i]),silent=TRUE)
#   
#   if(class(r)=="try-error"){
#     print(paste0("Warning: The following day was not found in the SNODAS folder: ", unique_dates[i]))
#     return(rep(NA, length(dates[dates == unique_dates[i]])))   # return NAs if the dates fall outside of snodas data
#   }else{
#     return(extract(r, XYdata[dates == unique_dates[i],]))   # extract the snodas data 
#   }
#   
# }))   # end of loop over unique dates
# stopCluster(clust)
# 
# if(length(vals) != nrow(XYdata))
#   stop("There is a problem here. If you get this message, talk to Jerod.")
# 
# # add column to origional database, then rename it with the product name
# xyorig$temp12345 <- vals
# names(xyorig)[names(xyorig)=="temp12345"] <- paste0("Snodas_",productName)
# 
# data_22t23_depth <- xyorig
# 
# #function ends 
# 
# 
# #function starts 
# XYdata = Prosp_post22 # this is a sf object with points
# SNODASfolder= newSnodas  # this is the snodas folder with 1 tif file per julien day for the product in question
# datesname="Date"
# 
# 
# # prepare packages
# if(all(c("raster","sf","parallel","stringr") %in% installed.packages()[,1])==FALSE)
#   stop("You must install the following packages: raster, sf, stringr, and parallel")
# require("sf")
# require("raster")
# require("parallel")
# require("stringr")
# 
# # some checks and preparation
# if(inherits(XYdata, "sf") == FALSE) 
#   stop("XYdata is not a sf object")
# 
# xyorig <- XYdata  # save the original database as a separate file name (for later)
# XYdata <- XYdata[,datesname]    # reduce the database down to save memory during parallel processing
# names(XYdata)[1] <- "date"
# 
# dates <- XYdata$date   # grab out the dates separately ######I changed this from dates <- st_set_geometry(XYdata, NULL)[,datesname] because it kept creating a tibble df and not a vector, which is what we needed
# if(inherits(dates,"POSIXct") != TRUE) 
#   stop("Your datesname column is not POSIXct")
# if(any(is.na(dates) == TRUE)) 
#   stop("You have NAs in your datesname column")
# 
# drs <- list.files(SNODASfolder, ".tif$")
# tz <- attr(dates,"tzone")
# # update dates to format of snodas files
# dates <- paste0(as.numeric(strftime(dates, format = "%Y", tz = tz)), 
#                 str_pad(as.numeric(strftime(dates, format = "%m", tz = tz)), 2, side="left",pad=0), 
#                 str_pad(as.numeric(strftime(dates, format = "%d", tz = tz)), 2, side="left",pad=0))   # this is the combos of years and juls
# unique_dates <- unique(dates)
# 
# #get and organize the name of the product
# productName <- str_split_fixed(sub(".tif","",drs), "_",3)[,3]
# if(length(table(productName))!= 1)
#   stop("You have extra files in your SNODASfolder. They should only be daily rasters (in tif format) of the same product.")
# productName <- productName[1]
# 
# fls <- paste0(SNODASfolder,"/SNODAS_",unique_dates, "_", productName, ".tif")
# if(any(file.exists(fls)==FALSE))
#   print(paste0("Warning: You have ",length(which(file.exists(fls)==FALSE)), " dates outside of ", length(fls), " that are outside of the data in your snodas folder."))
# 
# # reproject xy data to projection of snodas
# r <- raster(paste0(SNODASfolder, "/", drs[1]))
# XYdata <- st_transform(XYdata, crs=projection(r))
# 
# # prepare parallel processing
# no_cores <- detectCores()-1
# print(paste0("Initiating parallel processing on ", no_cores, " cores."))
# clust <- makeCluster(no_cores) 
# parallel::clusterExport(clust, envir=environment(),
#                         varlist=c("dates","XYdata","unique_dates","fls"))
# 
# # start parallel loop
# vals <- do.call(c, clusterApplyLB(clust, 1:length(unique_dates), function(i){
#   require("raster")
#   require("sf")
#   
#   # bring in raster of the year/day of question
#   r <- try(raster(fls[i]),silent=TRUE)
#   
#   if(class(r)=="try-error"){
#     print(paste0("Warning: The following day was not found in the SNODAS folder: ", unique_dates[i]))
#     return(rep(NA, length(dates[dates == unique_dates[i]])))   # return NAs if the dates fall outside of snodas data
#   }else{
#     return(extract(r, XYdata[dates == unique_dates[i],]))   # extract the snodas data 
#   }
#   
# }))   # end of loop over unique dates
# stopCluster(clust)
# 
# if(length(vals) != nrow(XYdata))
#   stop("There is a problem here. If you get this message, talk to Jerod.")
# 
# # add column to origional database, then rename it with the product name
# xyorig$temp12345 <- vals
# names(xyorig)[names(xyorig)=="temp12345"] <- paste0("Snodas_",productName)
# 
# prosp_22t23_depth <- xyorig
# #ends 
# 
# data_14t21_tmax
# prosp_14t21_tmax
# 
# data_22t23_depth$Snodas_SnowDepth <- data_22t23_depth$Snodas_SnowDepth/1000
# prosp_22t23_depth$Snodas_SnowDepth <- prosp_22t23_depth$Snodas_SnowDepth/1000
# 
# desert_snow <- rbind(data_14t21_depth, data_22t23_depth)
# prosp_snow <- rbind(prosp_14t21_depth, prosp_22t23_depth)
# 


#--------------------------@
# Pull tmax ####

#weather <- read.csv("C:/Users/lwilde2/Downloads/RockSprings_WeatherStationData_2014to2023.csv")
# 
# str(weather)
# weather <- weather %>% mutate(Date = as.POSIXct(DATE, format = "%m/%d/%Y"), Year = year(Date), Month = month(Date), J = yday(Date))
# x <- weather %>% filter(Month %in% c(12,1, 2)) %>% group_by(Year, J) %>% summarise(max = min(na.omit(TMIN)))
# x$J <- ifelse(x$J > 300, x$J - 365, x$J)
# 
# tmean_test <- weather %>% filter(Month %in% c(12,1,2)) %>% group_by(Year) %>% summarise(mean = mean(na.omit(TMIN)), sd = sd(na.omit(TMIN)))
# tmean_test
# wint_post22_tmax <- wint_post22 %>% left_join(weather %>% dplyr::select(Date, TMAX), by = "Date")
# names(wint_post22_tmax)[14] <- "DAYMET_tmax"
# 
# head(data_14t21_tmax)
# wint_post22

#--------------------------------------------------#
#determine sample size after Mar 10 each year ####
DeerID2 <- DeerID
DeerID2$StopDate <- ifelse(is.na(DeerID2$StopDate), as.POSIXct("2023-03-30"), DeerID2$StopDate)

range <- DeerID2 %>% group_by(AID) %>% summarise(min = min(as.POSIXct(StartDate, format = "%m/%d/%Y", origin = "1970-01-01")), max = max(as.POSIXct(StopDate, origin = "1970-01-01")))

range_Mar2Dec <- range %>% mutate(EndDay = mday(max), EndMonth = month(max)) %>% filter(!EndDay > 8 & !EndMonth > 11) 

sum <- range %>% mutate(minY = year(min), maxY = year(max)) %>% arrange(minY, maxY)

#add
year <- c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)

collarsYearly <- data.frame()
for(i in year){
  #i=2019
  xx <- data.frame(year = NA, collars = NA)
xx$year <- i
t <- sum %>% filter(minY < i & maxY >= i)
xx$collars <- nrow(t)
collarsYearly <- rbind(collarsYearly,xx)
}

#2014 fix
collarsYearly$collars[1] <- 14
winter_mort_data$n[1] <- 0 #fix that collars dropped, not dead


#morts in the winter
winter <- c("14-15","15-16","16-17","17-18","18-19","19-20","20-21","21-22","22-23")
txt <- paste("2023 data current as of:")
morts <- ggplot(winter_mort_data)  + geom_bar(aes(x = as.character(winter), y = n), stat = "identity", fill = "blue") + geom_hline(yintercept = mean(winter_mort_data$n), linewidth = 1.4, linetype = "dashed") + labs(x = "Winter (Dec 1 - Mar 10)", y = "Number of Deer", title = "Red Desert Mule Deer, Total Mortalities") + theme_classic() + coord_cartesian(ylim = c(-.5,10.5))+ scale_y_continuous(breaks = seq(0,10,2)) + theme(title = element_text(size = 10)) + geom_text(x=2.2, y=10, label=txt) + geom_text(x=2.2, y=9, label=Sys.Date());morts

winter_mort_data$perc <- winter_mort_data$n / collarsYearly$collars[2:10]
winter_mort_data$surv <- (1 - winter_mort_data$perc) * 100

perc.morts <- ggplot(winter_mort_data)  + geom_bar(aes(x = as.character(winter), y = perc), stat = "identity", fill = "blue") + geom_hline(yintercept = mean(winter_mort_data$perc ), linewidth = 1.4, linetype = "dashed") + labs(x = "Winter (Dec 1 - Mar 10)", y = "Percent of Sample", title = "Red Desert Mule Deer, % Sample Lost") + theme_classic() + coord_cartesian(ylim = c(-.01,.2))+ scale_y_continuous(breaks = seq(0,.2,.02)) + theme(title = element_text(size = 10)) + geom_text(x=2.2, y=.18, label=txt) + geom_text(x=2.2, y=.16, label=Sys.Date());perc.morts

perc.surv <- ggplot(winter_mort_data)  + geom_bar(aes(x = as.character(winter), y = surv), stat = "identity", fill = "blue") + geom_hline(yintercept = mean(winter_mort_data$surv ), linewidth = 1.4, linetype = "dashed") + labs(x = "Winter Survival since Dec 1", y = "Percent of Sample", title = "Red Desert Mule Deer, % Adult Survival") + theme_classic() + coord_cartesian(ylim = c(69,100))+ scale_y_continuous(breaks = seq(70,100,5)) + theme(title = element_text(size = 10)) + geom_text(x=2.2, y=.88, label=txt) + geom_text(x=2.2, y=.86, label=Sys.Date());perc.surv

# #snowdepth - winter range
# RedDes_Snodep <- ggplot(desert_snow %>% filter(Year != 2015)) + geom_jitter(aes(x = as.character(Year), y = Snodas_SnowDepth), color = "grey", alpha = .2) + geom_pointrange(data = desert_snow %>% filter(Year != 2015) %>% group_by(Year) %>% summarise(meandep = mean(Snodas_SnowDepth), sd = sd(Snodas_SnowDepth)),aes(x = as.character(Year), y = meandep, ymin = meandep - sd, ymax = meandep + sd), color = "black", size = 1.1) + theme_classic() + labs(x = "Year", y = "Snowdepth (m)", title = "Rock Springs, WY & Superior, WY")
# 
# #snowdepth - prospects and N of Hwy 28
# Prosp_Snodep <- ggplot(prosp_snow %>% filter(Year != 2015)) + geom_jitter(aes(x = as.character(Year), y = Snodas_SnowDepth), color = "grey", alpha = .2) + geom_pointrange(data = prosp_snow %>% filter(Year != 2015) %>% group_by(Year) %>% summarise(meandep = mean(Snodas_SnowDepth), sd = sd(Snodas_SnowDepth)),aes(x = as.character(Year), y = meandep, ymin = meandep - sd, ymax = meandep + sd), color = "black", size = 1.1) + theme_classic() + labs(x = "Year", y = "Snowdepth (m)", title = "North of Hwy 28")
# 
# #tmax - red des
# RedDes_Tmin <- ggplot(tmean_test) + geom_jitter(data = weather %>% filter(Month %in% c(12,1,2)), aes(x = as.character(Year), y = TMIN), color = "grey", alpha = .4) + geom_pointrange(aes(x = as.character(Year), y = mean, ymin = mean-sd, ymax = mean+sd), color = "black", size = 1.1) + theme_classic() + labs(x = "Year", y = "Daily Min Temp (c)", title = "Rock Springs, WY & Superior, WY"); RedDes_Tmin


#------------------------------#
# Export plots ####
ggsave(RedDes_Snodep + Prosp_Snodep, file = "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/WGFD_Mar2023_CaptureMeeting/RedDesert_snowdepth.png")
ggsave(RedDes_Tmin, file = "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/WGFD_Mar2023_CaptureMeeting/RedDesert_tmin.png")
ggsave(morts, file = "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/WGFD_Mar2023_CaptureMeeting/RedDesert_WinterMorts_v3_new.png")
ggsave(perc.morts, file = "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/WGFD_Mar2023_CaptureMeeting/RedDesert_WinterMortsPercentOfSample_v3_new.png")
