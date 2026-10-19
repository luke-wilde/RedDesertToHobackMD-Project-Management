##Code To Download Locations and Produce KML for upload to OnX ##
# Written by Luke Wilde, April 2022


rm(list=ls())

#load libraries

library(devtools)
#devtools::install_github("jmerkle1/MerkleLab-Code-Repository", subdir="MoveTools")
library(MoveTools)
library(sf)
library(tidyverse)
library(readxl)
library(mapview)
library(maptools)

source("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/RCode/other RCode/Misc/DownloadingIridium_FetchFunction.R")

master <- "RD2H_AllDeerIDs_21SEPTEMBER2024.csv"


# SETTINGS FOR ALL
days <- 3 #how many days "tail" are needed

HIGHCONF_FRS <- c(801, 723, 726) #list the collar serial numbers of the collars you have finished
MEDCONF_FRS <- c(000)#
FAWNCOLLARCHECK <- c(000)#c(seq(651,700,1))


# Open if using for Fawn captures ####
# fawndata <- read_excel("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/Fawn Recruitment/RD2H_MD_Fall2023_FawnRecruitment.xlsx")
# 
# 
# DoesWithFawns <- fawndata %>% filter(`NumOfOffspring (with Collar)` > 0 | !RecruitmentConfidence %in% c("HIGH", "MEDIUM-HIGH"))
# DoesWithFawns$AID <- DoesWithFawns$DeerID
# fawndata$AID <- fawndata$DeerID

#only if doing Fawn recruitmen surveys
FFR <- read.csv("G:/My Drive/RDH/Field Work/RDH_2024_FawnRecCheck.csv")
males <- read.csv("G:/My Drive/RDH/Field Work/RDH_2024_BuckCheck.csv")


#----------------------------------------------------------------#
#### Download ATS or Vectronics ####
#----------------------------------------------------------------#

# ATS Auto Download ####
library(devtools)
#install.packages("remotes")
#remotes::install_github("Huh/collar")

library(collar)
library(lubridate)
library(tidyverse)
library(stringr)

# Open if using Vectronix or ATS ####
# 
# y <- str_sub((Sys.Date() - days),1,4)
# m <- str_sub((Sys.Date() - days),6,7)
# d <- str_sub((Sys.Date() - days),9,10)
# after <- paste0(y,"-",m,"-",d," ","00:00:00", sub = "")
# 
# path <- "C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/VectronicsKeys/2023/Collar46523_Registration.keyx"
# path2 <- "C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/VectronicsKeys/2023/Collar46524_Registration.keyx"
# 
# x46523 <- fetch_vectronics(path, type = "gps")
# x46524 <- fetch_vectronics(path2, type = "gps")
# 
# v <- rbind(x46523, x46524)
# names(v)
# head(v)
# 
# v <- v %>% mutate(Date = str_sub(acquisitiontime, 1, 10), Time = str_sub(acquisitiontime, 12, 20), POSIXct = as.POSIXct(paste(Date," ",Time, sub = ""), format = "%Y-%m-%d %H:%M:%S", tz = "UTC"), Month = month(POSIXct), Day = day(POSIXct), Year = year(POSIXct), DayOfYear = yday(POSIXct), Date = ydm(POSIXct), Time = time(POSIXct)) %>% rename(CllrSrN= idcollar, Lat = latitude, Long = longitude, Altitude = height, HDOP = dop, NumSats = satcount) %>% dplyr::select(CllrSrN,POSIXct, Lat,Long,Altitude,HDOP,NumSats)
# 
# v
# head(v)
# 
# 
# #Reconvert date and time data to match MST time zone
# 
# v$Month <- month(v$POSIXct)
# v$Month<- sprintf(paste("%02d"), v$Month)
# v$Day <- day(v$POSIXct)
# v$Year <- year(v$POSIXct)
# v$Hour <- hour(v$POSIXct)
# v$Minute <- sprintf(paste("%02d"), 00)
# v$Sec <- sprintf(paste("%02d"), 00)
# v$Time <- paste(v$Hour, v$Minute, v$Sec, sep=":")
# v$Date <- paste(v$Year, v$Month, v$Day, sep="-")
# v$dt <- paste(as.character(v$Date), v$Time, sep=" ")
# v$POSIXct <- as.POSIXct(strptime(v$dt, format="%Y-%m-%d %H:%M:%S"), tz="UTC")
# v$POSIXct <- with_tz(v$POSIXct, tzone = "America/Denver")
# 
# v$DayOfYear <- yday(v$POSIXct)
# head(v)
# names(v)
# 
# vv <- v %>% filter(POSIXct >= as.POSIXct(after, format = "%Y-%m-%d %H:%M:%S", tz = "MST"))

# 
# # login to ATS site with Cody info
# ats_login(usr = "MA17201RE", pwd = "V0fE1F$E")
# 
# fetch_ats_devices()
# 
# # download position data for multiple collars at once
# RDH_ATS_collars <- c("048631", "048626")
# 
# # fetch them
# fixes_ATS_RDH <- fetch_ats_positions(device_id = RDH_ATS_collars)
# 
# # logout
# ats_logout()
# 
# 
# #clean and prep
# names(fixes_ATS_RDH)
# head(fixes_ATS_RDH)
# 
# #cut down to necessary columns
# gps_ATS <-
#   fixes_ATS_RDH %>% dplyr::select(CollarSerialNumber,
#                                   DateLocal,
#                                   JulianDay,
#                                   Latitude,
#                                   Longitude,
#                                   HDOP,
#                                   NumSats)
# head(gps_ATS)
# #
# gps_ATS <-
#   gps_ATS %>% mutate(
#     Year = year(DateLocal),
#     Month = month(DateLocal),
#     Day = day(DateLocal),
#     Date = date(DateLocal),
#     POSIXct = as.POSIXct(DateLocal, tx = "MST"),
#     Time = format(DateLocal, "%H:%M:%S"),
#     CllrSrN = CollarSerialNumber,
#     DayOfYear = JulianDay,
#     Lat = Latitude,
#     Long = Longitude
#   )
# 
# gps_ATS$Altitude <- "NA"
# gps_ATS$NumSats <- "NA"
# 
# 
# names(gps_ATS)
# 
# #columns to keep
# final.a <-
#   gps_ATS %>% dplyr::select(
#     "CllrSrN",
#     "POSIXct",
#     "Month",
#     "Day",
#     "Year",
#     "DayOfYear",
#     "Date",
#     "Time",
#     "Lat",
#     "Long",
#     "Altitude",
#     "HDOP",
#     "NumSats"
#   )
# 
# final.a <- final.a[final.a$Long != 0 & final.a$Lat != 0,]
# 
# final.a <- final.a %>% filter(!is.na(POSIXct)) %>% as.data.frame()
# head(final.a)
# str(final.a)
# names(final.a)
# 
# final.a <-
#   final.a %>% filter(POSIXct >= Sys.time() - (days * 24 * 3600))
# 
# final.a$CllrSrN <-
#   dplyr::recode(final.a$CllrSrN, "048626" = "48626", "048631" = "48631")
# rm(gps_ATS, fixes_ATS_RDH)


#----------------------------------------------------------------#
#### Settings for fetching locations on Telonics Collars ONLY ####
#----------------------------------------------------------------#

fldr_out = "C:\\Users\\lwilde2\\Desktop\\TelonicsTemp"## Where you want the temp files stored, this will be emptied after the data are brought into the R environment
days = days #the number of days worth of daily locations you want (will only affec thow long the "tail" following points is; think tadpoles)
username = "AnnaOrtega" #log in info for RDH
password = "n6rW2QSw" #log in info for RDH
TDC_path= "C:\\Program Files (x86)\\Telonics\\Data Converter\\TDC.exe" #the default area where apps are saved, go to https://www.telonics.com/software/tdc.php to download the TDC and work from there!





#now load the gps files from Telonics

if(file.exists(fldr_out)){print(length(list.files(fldr_out))==0)} else{dir.create(file.path(fldr_out))} # should be [1] TRUE or no output

fixes <- fetch_telonics_gstar_positions( username = username,password = password,
                                         fldr_out = fldr_out,  
                                         TDC_path = TDC_path,
                                         keep.reports = FALSE,   # if FALSE, will remove all temp files at the end
                                         start_date = NULL)  # if you put NULL here, you'll get ALL data, if you change the first number to 1, you get only todays data

#fixes will save to a directory, and you can store that location below as the dir.tdc object. Or you can integrate in the case that Jerod's code loads directly into the R environment

fixes.IR <- fetch_telonics_iridium_positions(username = username,password = password,
                                             fldr_out = fldr_out,  
                                             TDC_path = TDC_path,
                                             keep.reports = FALSE,   # if FALSE, will remove all temp files at the end
                                             start_date = NULL)

  # fetch_telonics_iridium_positions(username = username,password = password,
  #                                fldr_out = fldr_out,  
  #                                TDC_path = TDC_path,
  #                                keep.reports = FALSE,   # if FALSE, will remove all temp files at the end
  #                                start_date = Sys.time() - days * 24 * 3600)

#----------------------------------------------------------------#
#Import Telonics Globalstar data directly from TDC and clean the data ####
#----------------------------------------------------------------#

#just click through this section, no changes
gps <- fixes

gps2 <- fixes.IR

gps2 <- gps2 %>% dplyr::select(names(gps))

gps <- rbind(gps, gps2)

#Extract GPS fixes that have been successful
gps<-gps[gps$GPS.Fix.Attempt!="Failed", ]
gps<-gps[gps$GPS.Fix.Attempt!="Unresolved QFP", ]
gps<-gps[gps$GPS.Fix.Attempt!="", ]
table(gps$GPS.Fix.Attempt)

#Ensure that there are the correct number of unique Telonics collars (n = 65)
unique.cllr<-unique(gps$CllrSrN)

#Create new GPS dataframe
t<-gps

#Split date and time column into two separate columns
t$tempdate<- sapply(t$Acquisition.Time, FUN = function(x){unlist(strsplit(x, " "))[1]})
t$temptime<- sapply(t$Acquisition.Time, FUN = function(x){unlist(strsplit(x, " "))[2]})

#Split date column into year, month, and day columns
t$Year<-substr(t$tempdate, 1, 4) 
t$Month<-substr(t$tempdate, 6, 7) 
t$Day<-substr(t$tempdate, 9, 10) 

#Combine month, day, and year column into a single date column with correct format
t$date<- paste(t$Year, t$Month, t$Day,  sep="-")

#Clean up all date and time columns
t$Hour <- format(strptime(t$temptime,"%H:%M:%S"),'%H')
t$Minute <- sprintf(paste("%02d"), 00)
t$Sec <- sprintf(paste("%02d"), 00)
t$Time <- paste(t$Hour, t$Minute, t$Sec, sep=":")
t$Date <- paste(t$Year, t$Month, t$Day, sep="-")
t$dt <- paste(as.character(t$Date), t$Time, sep=" ")
t$POSIXct <- as.POSIXct(strptime(t$dt, format="%Y-%m-%d %H:%M:%S"), tz="UTC")
attr(t$POSIXct, "tzone") <- "GMT"
attr(t$POSIXct, "tzone") <- "MST"

#Reconvert date and time data to match MST time zone
t$Month <- month(t$POSIXct)
t$Month<- sprintf(paste("%02d"), t$Month)
t$Day <- day(t$POSIXct)
t$Year <- year(t$POSIXct)
t$Hour <- hour(t$POSIXct)
t$Minute <- sprintf(paste("%02d"), 00)
t$Sec <- sprintf(paste("%02d"), 00)
t$Time <- paste(t$Hour, t$Minute, t$Sec, sep=":")
t$Date <- paste(t$Year, t$Month, t$Day, sep="-")
t$dt <- paste(as.character(t$Date), t$Time, sep=" ")
t$POSIXct <- as.POSIXct(strptime(t$dt, format="%Y-%m-%d %H:%M:%S"), tz="MST")
t$DayOfYear <- yday(t$POSIXct)
head(t)
names(t)

t <- st_transform(t, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

tt <- as.data.frame(t %>% st_drop_geometry())
names(tt)

#Remove unneccessary columns, rename column headers, organize columns
tt <- tt[,c(6:7,8,16,19:21,26:27,29:30)]
colnames(tt)<-c("Lat","Long","HDOP","CllrSrN","Year","Month","Day","Time","Date","POSIXct","DayOfYear")
head(tt)
tt$Altitude<-"N/A"
tt$NumSats<-"N/A"
tt<-tt[order(tt$CllrSrN,tt$POSIXct),]
final.t <- tt[c("CllrSrN","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long","Altitude","HDOP","NumSats")]
head(final.t)

# Open if using Vectronix or ATS ####
# vv <- vv %>% select(names(final.t))
# 
# vvsf <- st_as_sf(vv %>% drop_na(Lat, Long), coords = c("Long", "Lat"), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
# 
# vvsf <- st_transform(vvsf, "+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs")
# 
# vv <- vvsf %>% mutate(Long = st_coordinates(.)[,1],Lat = st_coordinates(.)[,2]) %>% st_drop_geometry() %>% select(names(final.t))
# 
# #BIND VECTRONICS DATA WITH TELONICS
# final.t <- rbind(final.t, vv)

#Note, I did not remove any HDOP errors, because unlike LOTEK...I can't quite figure out what values of HDOP are truly errors

#Omit POSIXct values with null values
final.t<-final.t[!is.na(final.t[,2]),] 

#Remove any rows containing zero values for Latitude and Longitude
final.t<-final.t[final.t$Long!=0 & final.t$Lat!=0, ]
final.t <- final.t %>% drop_na(Long, Lat)

#make both spatial, same crs, and then bind
# final.a <- st_as_sf(final.a, 
#                     coords = c("Long", "Lat"), # note x goes first
#                     crs = "+proj=latlon +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs", # projection, this is NAD83
#                     remove = FALSE)

final.t <- st_as_sf(final.t, 
                      coords = c("Long", "Lat"), # note x goes first
                      crs = "+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs", # projection, this is NAD83
                      remove = FALSE)

#final.a <- final.a %>% st_transform(., "+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs") %>% mutate(Lat = st_coordinates(.)[,2], Long = st_coordinates(.)[,1])
  
#final.a
final.t
  

#bind
final.ta.sp <- final.t
final.ta <- final.ta.sp %>% st_drop_geometry() %>% as.data.frame()
head(final.ta)


ggplot(final.ta.sp) + geom_sf() # check that they are all where they should be!

rm(gps,t)

#----------------------------------------------------------------#
#Import master ID list ####
#----------------------------------------------------------------#
## Load where the deer ID metadata are stored
#Set working directory and import csv file
setwd("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/")
DeerID.Data <- read.table(master, header=TRUE, sep=",")
head(DeerID.Data)

#Merge DeerIDs based on unique CllrSrN and range of dates
#Format date and time columns
DeerID.Data$StartDate <- as.POSIXct(DeerID.Data$StartDate, format="%m/%d/%Y", origin = "1970-01-01")
DeerID.Data$StopDate <- as.POSIXct(DeerID.Data$StopDate,format="%m/%d/%Y", origin = "1970-01-01")

DeerID.Data$Startdt <- paste(as.character(DeerID.Data$StartDate), DeerID.Data$StartTime, sep=" ")
DeerID.Data$StartDateTime <- as.POSIXct(strptime(DeerID.Data$Startdt, format="%Y-%m-%d %H:%M:%S"), origin = "1970-01-01", tz="MST")

DeerID.Data$Stopdt <- paste(as.character(DeerID.Data$StopDate), DeerID.Data$StopTime, sep=" ")
DeerID.Data$StopDateTime <- as.POSIXct(strptime(DeerID.Data$Stopdt, format="%Y-%m-%d %H:%M:%S"), origin = "1970-01-01", tz="MST")
names(DeerID.Data)

#Sort unique collar serial numbers; make sure the serial numbers look correct
sort(unique(final.ta$CllrSrN))
sort(unique(DeerID.Data$CllrSrN))

#Merge all GPS collar data with Deer ID csv by CllrSrN
mydata <- final.ta %>% left_join(.,DeerID.Data, by="CllrSrN", relationship = "many-to-many")

#Organize dataframe by CllrSrN, POSIXct, and AID
mydata<-mydata[order(mydata$CllrSrN, mydata$POSIXct, mydata$AID),]

#Omit any deer IDs that are N/A
table(is.na(mydata[,11]))
mydata<-mydata[!is.na(mydata[,11]),] 

sort(unique(final.ta$CllrSrN))
sort(unique(mydata$CllrSrN))

#----------------------------------------------------------------#
#Identify unique IDs and create empty output dataframe for data in which StopDateTime contains NULL values ####
#----------------------------------------------------------------#
library(fuzzyjoin)
library(lubridate)


#Merge DeerIDs based on unique CllrSrN and range of dates
#Format date and time columns
DeerID.Data$StartDate <- as.POSIXct(DeerID.Data$StartDate)
DeerID.Data$StopDate <- as.POSIXct(DeerID.Data$StopDate)

names(DeerID.Data)

#Remove unnecessary columns
DeerID.Data<- DeerID.Data %>% dplyr::select(AID, Mgtry, TagColor, TagNum, Frequency, CllrSrN, StartDate, StopDate)
head(DeerID.Data)

#make sure the serial numbers look correct from both the gps and metadata
sort(unique(final.ta$CllrSrN))
sort(unique(DeerID.Data$CllrSrN))

str(final.ta)
str(DeerID.Data)

#make sure that the Date columns are both Date class -- this will make join work!
final.ta$Date <- as.Date(final.ta$POSIXct)
DeerID.Data$StartDate <- as.Date(DeerID.Data$StartDate)
DeerID.Data$StopDate <- as.Date(DeerID.Data$StopDate)

# ATTN: The join has problems with collars that do not have a StopDate, so split into active and inactive.
DeerID.Data.inactive <- DeerID.Data %>% filter(!is.na(StopDate))
DeerID.Data.active <- DeerID.Data %>% filter(is.na(StopDate))


final.ta <- final.ta %>% filter(POSIXct > Sys.Date() - days - 1)

#Merge all GPS collar data with Deer ID csv by CllrSrN and between dates FOR INDIV/COLLARS THAT ARE INACTIVE
# the dates need to be class "Date", be sure to use the backquotes in the match_fun, which tells R what operator to use in place of the "=" sign in the by command
mydata1 <- fuzzy_inner_join(final.ta, DeerID.Data.inactive, by= c("CllrSrN" = "CllrSrN",
                                                                 "Date" = "StartDate",
                                                                 "Date" = "StopDate"),
                            match_fun = list(`==`,`>=`,`<=`))

#Organize dataframe by CllrSrN, POSIXct, and AID
mydata1<-mydata1 %>% arrange(AID, POSIXct)

#Omit any deer IDs that are N/A
mydata1<-mydata1 %>% drop_na(AID)

#remove duplicates, should be none but you never know! also rename CllrSrN
mydata1 <- mydata1[!duplicated(mydata1[, c("AID", "POSIXct")]),]
mydata1 <- mydata1 %>% rename("CllrSrN" = CllrSrN.x)


# NOW DO THE SAME FOR ACTIVE COLLARS

mydata2 <- fuzzy_inner_join(final.ta, DeerID.Data.active, by= c("CllrSrN" = "CllrSrN",
                                                               "Date" = "StartDate"),
                            match_fun = list(`==`,`>=`))

#Organize dataframe by CllrSrN, POSIXct, and AID
mydata2<-mydata2 %>% arrange(AID, POSIXct)

#Omit any deer IDs that are N/A
mydata2<-mydata2 %>% drop_na(AID)

#remove duplicates, should be none but you never know! also rename CllrSrN
mydata2 <- mydata2[!duplicated(mydata2[, c("AID", "POSIXct")]),]
mydata2 <- mydata2 %>% rename("CllrSrN" = CllrSrN.x)

mydata <- rbind(mydata1, mydata2)

head(mydata)
str(mydata)

final.gps <- mydata %>% dplyr::select(AID, Mgtry, TagColor, TagNum, CllrSrN, Frequency, Lat, Long, POSIXct, Date, Time)


summary(final.gps)


#make spatial again
final.sf <- st_as_sf(final.gps, 
                     coords = c("Long", "Lat"), # note x goes first
                     crs = "+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs", # projection, this is NAD83
                     remove = FALSE) # don't remove lat/lon cols from dataframe

ggplot(final.sf) + geom_sf()

final.sf %>% mutate(Date = str_sub(as.character(POSIXct),1,10), Time = str_sub(as.character(POSIXct),11,19))



#keep only those for Fawn recruitment
check <- rbind(FFR %>% select(AID), males %>% select(AID))

final.sf <- final.sf %>% filter(AID %in% check$AID)







#----------------------------------------------------------------#
#### Check outputs ####
#----------------------------------------------------------------#



#check if there are any missing from the OnX data:
FFR %>% filter(!AID %in% final.sf$AID) %>% arrange(CllrSrN)

# CHECK TO MAKE SURE DATA IS UP TO DATE
as.data.frame(final.sf %>% filter(AID %in% DeerID.Data.active$AID) %>%  group_by(AID) %>% summarise(min = min(POSIXct), max = max(POSIXct)) %>% arrange(max))

#check which are missing
sort(unique(final.sf$CllrSrN))
sort(unique(final.ta$CllrSrN))
     
#help figure out if any deer is missing below:
# sort(unique(final.sf$AID))
# "701432A" %in% sort(unique(final.ta$CllrSrN))
# sort(unique(DeerID.Data.active$AID))
# 
# final.ta[which(sort(unique(final.ta$CllrSrN)) %in% sort(unique(final.sf$CllrSrN)) == F), "CllrSrN"]
# 
# final.ta[which(sort(unique(final.ta$CllrSrN)) %in% sort(unique(final.sf$CllrSrN)) == F), "CllrSrN"]
# DeerID.Data.active[which(DeerID.Data$CllrSrN == final.ta[which(sort(unique(final.ta$CllrSrN)) %in% sort(unique(final.sf$CllrSrN)) == F), "CllrSrN"]), ]
# 
# length(unique(final.ta$CllrSrN))
# length(unique(final.sf$CllrSrN))
# 
# final.ta %>% filter(!CllrSrN %in% sort(unique(final.sf$CllrSrN)))
# DeerID.Data.active %>% filter(!CllrSrN %in% sort(unique(final.sf$CllrSrN)) & is.na(StopDate))
# DeerID.Data.inactive %>% filter(!CllrSrN %in% sort(unique(final.sf$CllrSrN)) & is.na(StopDate))


#----------------------------------------------------------------#
#### Draw Lines and Keep point for only Last Day ####
#----------------------------------------------------------------#

#=================================USE THIS ONLY FOR FAWN CAPTURES
# 
# #bring in fawn info
# fawndata 
# 
# f <- as.numeric(fawndata$`NumOfOffspring (with Collar)`)
# 
# fawns <- data.frame()
# for(i in 1:nrow(fawndata)){
# 
#   m <- as.character(ifelse(fawndata$`NumOfOffspring (with Collar)`[i] > 0, rep("F", f[i]),""))
#   fawns <- rbind(fawns,m)
#   l <- as.character(ifelse(fawndata$RecruitmentConfidence[i] %in% c("HIGH", "MEDIUM-HIGH"), " ", fawndata$RecruitmentConfidence[i]))
#   fawns$conf[i] <- l
#                   
# }
# 
# fawns
# 
# fawns[c(which(f > 1)),1] <- "FF"
# 
# d <- data.frame("AID" = fawndata$DeerID, "num" = fawns$X.F., "conf" = fawns$conf)
# d <- d %>% group_by(AID) %>% slice(tail(row_number(), 1)) %>% ungroup() %>% filter(num %in% c("FF", "F") | conf %in% c("MED", 'MEDIUM', "LOW", "MEDIUM-LOW")) %>% arrange(AID) %>% as.data.frame(); d
# 
# 
# final.sf <- final.sf %>% left_join(d, by = "AID") %>% filter(AID %in% d$AID)
# 
# final.sf[which(is.na(final.sf$num)),"num"] <- ""
# 
# final.sf <- final.sf %>% mutate(Date = date(POSIXct), newID = paste(Frequency,"_",num," Deer",AID,"_",TagColor,TagNum,"_",Mgtry, sep=""))#,
# 
# 
# final.sf$AID <- as.numeric(final.sf$AID)
# #
# 
# #
# #final.sf <- final.sf %>% filter(AID %in% c(544, 606, 529, 608, 463,623,473,408,544,473,331))
# 
# #keep only those with fawns:
# final.sf <- final.sf %>% filter(AID %in% d$AID)
# 
# #final.sf <- final.sf %>% arrange(newID, POSIXct) %>% filter(AID < 650)


#---------------------#
#remove those already done ####
final.sf <- final.sf %>% filter(!AID %in% HIGHCONF_FRS)
final.sf <- final.sf %>% filter(!AID %in% MEDCONF_FRS)
final.sf <- final.sf %>% filter(!AID %in% FAWNCOLLARCHECK)

#---------------------#
# format cont ####

#final.sf <- final.sf %>% mutate(Tag = paste0(TagColor,"_", TagNum,sep="")) %>% filter(Tag %in% c("Green_25", "Orange_63", "Light Blue_66"))



final.point <- final.sf %>% 
  group_by(AID) %>%
  #arrange(rev(POSIXct)) %>%
  filter(POSIXct == max(POSIXct)) %>% dplyr::select(AID,POSIXct, Frequency, TagColor, TagNum, Mgtry)

as.data.frame(final.point %>% group_by(AID) %>% summarise(max = max(POSIXct)))

final.sf.ll <- st_transform(final.sf, "+proj=latlon +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs")

final.sf.ll <- final.sf.ll %>% arrange(POSIXct) %>% mutate(POSIXct == as.POSIXct(POSIXct))

final.lines <- Points2Lines(data=final.sf.ll, date_name="POSIXct", id_name="AID",byid=T)

unique(final.sf$AID)


# mapview(final.lines, zcol = "Tag", color = c("darkgreen", "lightblue", "darkorange")) + mapview(final.point, zcol = "Tag",  col.regions = c( "darkorange", "lightblue","darkgreen"), size = 10, alpha = .3)
 mapview(final.lines, color = "hotpink") + mapview(final.point, col.regions = "hotpink", legend = FALSE)


#----------------------------------------------------------------#
#### Prepare to Export ####
#----------------------------------------------------------------#

#final.point <- final.point %>% rename(Description = newID)
final.lines <- final.lines %>% dplyr::select(AID) 

final.lines.ll <- st_transform(final.lines, "+proj=latlon +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs")

final.point.ll <- st_transform(final.point, "+proj=latlon +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs")

#final.point.ll$Name <- paste0(final.point.ll$Frequency, "_", final.point.ll$TagColor, final.point.ll$TagNum, sep="")

#final.point.ll$Name <- ifelse(final.point.ll$AID %in% male$AID, paste(male$Sex,"_",final.point.ll$Name," ",str_sub(final.point.ll$Mgtry, 0,3), sep = ""), paste(final.point.ll$Name," ",str_sub(final.point.ll$Mgtry, 0,3),sep = ""))
#final.point.ll$Sex <- ifelse(final.point.ll$AID %in% male$AID, male$Sex, "Female")

final.point.sp <- as(final.point.ll, "Spatial")

final.point.sp$FFRid <- paste(final.point.sp$Frequency ,"_", final.point.sp$AID,"_", final.point.sp$TagColor, final.point.sp$TagNum, sep = "")

final.lines.ll <- final.lines.ll[which(npts(final.lines.ll, by_feature = T) >= 2),] # make sure to only add lines for deer with >1 point or will throw errors

mapview(final.point.ll, layer.name = "Loc", zcol = "Mgtry", map.types = c("OpenStreetMap","OpenTopoMap")) + mapview(final.lines.ll[,-1], layer.name = "Recent Movement", color = "grey") #+ mapview(stag, layer.name = "Staging Areas", col.regions = "red", col = "yellow")

#final.point.ll$newID



# st_write(final.point, "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/FieldSeasonPrep/FieldSeason2023/Fall/Points/RDH_new_LatestPoint.kml")

kmlPoints(obj=final.point.sp["AID"], kmlfile="G:/My Drive/RDH/Field Work/RDH_new_LatestPoint.kml", kmlname="daily points", name = final.point.sp$FFRid)

st_write(final.lines.ll, dsn="G:/My Drive/RDH/Field Work/RDH_new_lastFewDays.kml", driver = "kml", delete_dsn = TRUE, name = final.point.sp$Frequency)
# # 
# # 
# final.sf <- final.sf %>% filter(AID == 255)
# # mapview(final.sf)
# # 
# st_write(t, "C:/Users/lwilde2/Documents/Deer665_FallMigUpdates_11272023.shp")


#------------------------------------#
# staging areas ####

stag <- st_zm(st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/StagingAreas.kml"),drop = T)

CapturePlan %>% group_by(Mgtry) %>% summarise(n = n())

final.sf <- final.sf %>% filter(AID %in% CapturePlan$AID)

mapview(final.point.ll, layer.name = "Deer to Capture", zcol = "Mgtry", map.types = c("OpenStreetMap","OpenTopoMap","Esri.WorldImagery"), col.regions = c("purple", "orange", "green", "green"), col = "white") + mapview(stag, layer.name = "Staging Areas", col.regions = "black", col = "red")
# 
# 
# final <- final.ta.sp %>% filter(CllrSrN %in% c("727550A", "736474A"))


final.sf <- final %>% filter(POSIXct >= as.POSIXct(after, format = "%Y-%m-%d %H:%M:%S", tz = "MST"))

final.point <- final.sf %>%
  group_by(CllrSrN) %>%
  #arrange(rev(POSIXct)) %>%
  filter(POSIXct == max(POSIXct)) %>% dplyr::select(CllrSrN,POSIXct)

as.data.frame(final.point %>% group_by(AID) %>% summarise(max = max(POSIXct)))

final.sf.ll <- st_transform(final.sf, "+proj=latlon +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs")

final.sf.ll <- final.sf.ll %>% arrange(POSIXct) %>% mutate(POSIXct == as.POSIXct(POSIXct))

final.lines <- Points2Lines(data=final.sf.ll, date_name="POSIXct", id_name="CllrSrN",byid=T)

mapview(final.lines, zcol = "CllrSrN") + mapview(final.sf, zcol = "CllrSrN")



setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/")
M23 <- read_excel("Annual Capture Data/RD2H_MD_March2023 1 _CaptureData.xlsx")
male <- M23 %>% select(DeerID, Sex) %>% filter(Sex == "Male") %>% rename(AID = DeerID)

male

#----------------------------------------------------------------#
#### Prepare to Export ####
#----------------------------------------------------------------#

#final.point <- final.point %>% rename(Description = newID)
final.lines <- final.lines %>% dplyr::select(AID) 

final.lines.ll <- st_transform(final.lines, "+proj=latlon +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs")

final.point.ll <- st_transform(final.point, "+proj=latlon +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs")

final.point.ll$Name <- paste0(final.point.ll$Frequency, "_", final.point.ll$TagColor, final.point.ll$TagNum, sep="")

final.point.ll$Name <- ifelse(final.point.ll$AID %in% male$AID, paste(male$Sex,"_",final.point.ll$Name," ",str_sub(final.point.ll$Mgtry, 0,3), sep = ""), paste(final.point.ll$Name," ",str_sub(final.point.ll$Mgtry, 0,3),sep = ""))
final.point.ll$Sex <- ifelse(final.point.ll$AID %in% male$AID, male$Sex, "Female")

final.point.sp <- as(final.point.ll, "Spatial")



final.lines.ll <- final.lines.ll[which(npts(final.lines.ll, by_feature = T) >= 2),] # make sure to only add lines for deer with >1 point or will throw errors

mapview(final.point.ll, layer.name = "Loc", zcol = "Mgtry", map.types = c("OpenStreetMap","OpenTopoMap")) + mapview(final.lines.ll[,-1], layer.name = "Recent Movement", color = "grey") #+ mapview(stag, layer.name = "Staging Areas", col.regions = "red", col = "yellow")




#---------------------------------------#
# Fawn and mom ####

pair <- final.sf %>% filter(AID %in% c(255, 860)) %>% filter(Time == "17:00:00")
pair

pair.point <- pair %>%
  group_by(AID) %>%
  #arrange(rev(POSIXct)) %>%
  filter(POSIXct == max(POSIXct)) %>% dplyr::select(AID,POSIXct)

#as.data.frame(final.point %>% group_by(AID) %>% summarise(max = max(POSIXct)))

pair.ll <- st_transform(pair, "+proj=latlon +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs")

pair.ll <- pair.ll %>% arrange(POSIXct) %>% mutate(POSIXct == as.POSIXct(POSIXct))

pair.lines <- Points2Lines(data=pair.ll, date_name="POSIXct", id_name="AID",byid=T)

mapview(pair.lines, zcol = "AID", color = c("yellow3","purple")) + mapview(pair.point, zcol = "AID", col.regions = c("yellow3","purple"))


