##Code To Download Locations and Produce KML for upload to OnX ##
# Written by Luke Wilde, April 2022

#For captures

#========= LIST UPDATED ==============================#
#================== 11/20/2024 =======================#
#=====================================================#


rm(list=ls())

Capd <- c(000)
CheckAgain <- c(000)

#----------------------------------------------#
#from last capture (USE IN MARCH ONLY)
PrevFawnsCaught <- data.frame(AID = c(628,752,809,975,813,401,752,761,404,904,713), num = c(1,1,1,1,2,2,1,1,1,1,1))
#----------------------------------------------#

#install.packages("collar", repos = "https://nexus.spdgt.com/repository/spdgt-public/")

#load libraries
packages <- c("tidyverse","collar","stringr", "sf", "lubridate", "mapview", "MoveTools", "MerkleLabGIS", "readxl","devtools")


# Install missing packages
installed_packages <- installed.packages()
for (pkg in packages) {
  if (!(pkg %in% installed_packages[, "Package"])) {
    install.packages(pkg)
  }
}

# Load all packages
lapply(packages, library, character.only = T)

#correct select so that dplyr is preferred
select <- dplyr::select
summarise <- dplyr::summarise

#load function for iridium
source("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/RCode/other RCode/Misc/DownloadingIridium_FetchFunction.R")


#------------------------SETTINGS--------------------------#
master <- "RD2H_AllDeerIDs_15MARCH2026.csv"
gps <- "C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_CleanData.RData"
caplist <- "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CapturePlans/2026/March/RDH_CapList_March2026.csv"


#NEED TO UPDATE - this is from 12.6.2024
FRS <- "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/Fawn Recruitment/RD2H_MD_Fall2025_FawnRecruitment.xlsx" 


days <- 3 #how many days "tail" are needed


#anticipated capture date
capdate <- as.Date("2026-3-11")

#---------------------------------------------------------#




#load in the data
fawndata <- read_excel(FRS)
unique(fawndata$RecruitmentConfidence)

#only keep the most recent, HIGH confidence
fawndata <- fawndata %>% filter(RecruitmentConfidence %in% c("High")) %>% arrange(DeerID, Date) %>% group_by(DeerID) %>% slice_tail(n=1) %>% rename(AID = DeerID)

#seperate them out
DoesWithFawns <- fawndata %>% dplyr::filter(`NumOfOffspring (with Collar)` > 0)
DoesWithFawns
DoesWithoutFawns <- fawndata %>% filter(`NumOfOffspring (with Collar)` == 0)
DoesWithoutFawns

#load gps data
load(gps)
caplist <- read.csv(caplist)


#----------------------------------------------------------------#
#### Download ATS or Vectronics ####
#----------------------------------------------------------------#

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


#ggplot(final.ta.sp) + geom_sf() # check that they are all where they should be!

rm(gps,t)

#----------------------------------------------------------------#
#Import master ID list ####
#----------------------------------------------------------------#
## Load where the deer ID metadata are stored
#Set working directory and import csv file
setwd("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/")
DeerID.Data <- read.table(master, header=TRUE, sep=",")
males <- DeerID.Data %>% filter(Fawn.Sex == "M") %>% select(AID, Fawn.Sex)
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

#----------------------------#
# Now clip to only last 60 days ###

final.ta <- final.ta %>% arrange(CllrSrN, POSIXct) %>% group_by(CllrSrN) %>% slice_tail(n = 60)

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

#-------------------------#
#check if any living collars are missing from this - any malf you didnt know!
DeerID.Data.active %>% filter(!AID %in% final.gps$AID)
#-----------------------#

#make spatial again
final.sf <- st_as_sf(final.gps, 
                     coords = c("Long", "Lat"), # note x goes first
                     crs = "+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs", # projection, this is NAD83
                     remove = FALSE) # don't remove lat/lon cols from dataframe

#ggplot(final.sf) + geom_sf()

final.sf %>% mutate(Date = str_sub(as.character(POSIXct),1,10), Time = str_sub(as.character(POSIXct),11,19))



#keep only those for Fawn recruitment
final.sf <- final.sf %>% filter(AID %in% caplist$AID)

final.sf <- final.sf %>% filter(AID %in% DeerID.Data.active$AID) %>% group_by(AID) %>% slice_tail(n=days)

#----------------------------------------------------------------#
#### Check outputs ####
#----------------------------------------------------------------#

#check if any are older than 2 days
x <- final.sf %>% group_by(AID) %>% mutate(max = date(max(POSIXct))) %>% ungroup() 
xx <- x %>% group_by(AID) %>% slice_head(n=1) %>% summarise(daysOld = as.numeric(Sys.Date() - max))
table(xx$daysOld)


# which collars are missing
missing <- caplist %>% filter(!AID %in% final.sf$AID)
old <- xx %>% filter(daysOld > 3); old 

#replace the missing ones with their point last time
lastYearpoints <- rdh_data.sf %>% filter(AID %in% missing$AID) %>% filter(Date == as.Date(capdate - 365) | Date == as.Date(capdate - (2*365)) | Date == as.Date(capdate - (3*365))) %>% group_by(AID) %>% sample_n(1) %>% select(AID, geometry)

#find capture location of one with really bad data, no winter record
# t <- st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/March_2023/WY Superior doefawn 632023.GPX")
# tt <- t %>% filter(name == 4067) %>% mutate(AID = 710) %>% select(AID, geometry) ; rm(t)

badGPS <- rbind(lastYearpoints) #, tt %>% st_transform(., crs = st_crs(lastYearpoints)))

#join back in with the other data
badGPS <- badGPS %>% left_join(caplist %>% select(AID, Frequency, TagColor, TagNum, CllrSrN), by = "AID") %>% select(AID, Frequency, TagColor, TagNum, CllrSrN, geometry)
final.sf <- final.sf %>% select(AID, Frequency, TagColor, TagNum, CllrSrN, geometry)

tocap <- rbind(badGPS %>% st_transform(., crs = st_crs(final.sf)), final.sf)

badVHF = caplist %>% filter(Replace == "Check VHF")
caplist %>% filter(Replace == "Broken")
#----------------------------------------------------------------#
#### Draw Lines and Keep point for only Last Day ####
#----------------------------------------------------------------#

#=================================USE THIS ONLY FOR FAWN CAPTURES


tocap <- tocap %>% ungroup() %>% group_by(AID) %>% mutate(fawns = ifelse(AID %in% as.numeric(DoesWithFawns$AID), "F",ifelse(AID %in% as.numeric(DoesWithoutFawns$AID), "n","")), tagInfo = ifelse(AID %in% badVHF$AID | AID %in% badGPS$AID, paste(str_sub(TagColor,0,1),TagNum, sep =""),""), shortfreq = gsub("^1|\\.0+$", "", Frequency),num_offspring = ifelse(AID %in% as.numeric(DoesWithFawns$AID), DoesWithFawns$`NumOfOffspring (with Collar)`[match(AID, DoesWithFawns$AID)], 1))

tocap <- tocap %>% left_join(., PrevFawnsCaught, by = "AID") %>% mutate(num_offspring2 = num_offspring - num, num_offspring2 = ifelse(is.na(num_offspring2) | num_offspring2 <= 0, 1, num_offspring2))

for(i in 1:nrow(tocap)){
  #i = 6
  tocap$fawns[i] <- paste(rep(tocap$fawns[i],pmin(tocap$num_offspring[i],tocap$num_offspring2[i])), collapse = "")
}




tocap <- tocap %>% select(!c(num_offspring, TagColor, TagNum, CllrSrN)) %>% mutate(name = ifelse(tagInfo != "",paste(shortfreq,tagInfo,sep=""),shortfreq), name = paste(name,fawns,sep="")) %>% select(AID, fawns, Frequency, tagInfo, shortfreq, name, geometry)
tocap$name <- ifelse(tocap$AID %in% males$AID, paste(tocap$name,"M",sep=""),tocap$name)
tocap$name

#=================================OR USE THIS FOR NON-FAWN CAPTURES
# tocap <- tocap %>% mutate(tagInfo = ifelse(AID %in% badVHF$AID | AID %in% badGPS$AID, paste(str_sub(TagColor,0,1),TagNum, sep =""),""), shortfreq = gsub("^1|\\.0+$", "", Frequency)) %>% select(!c(TagColor, TagNum, CllrSrN)) %>% mutate(name = ifelse(tagInfo != "",paste(shortfreq,tagInfo,sep=""),shortfreq)) %>% select(AID, Frequency, tagInfo, shortfreq, name, geometry)
# 


#---------------------#
#remove those already done ####
tocap <- tocap %>% filter(!AID %in% Capd)
                            
                            

#tocap <- tocap %>% filter(!AID %in% CheckAgain)

 tocap$name <- ifelse(tocap$AID %in% old$AID | tocap$AID %in% badGPS$AID , paste(tocap$name, "?",sep=""),tocap$name)


#---------------------#
# format cont ####

#final.sf <- final.sf %>% mutate(Tag = paste0(TagColor,"_", TagNum,sep="")) %>% filter(Tag %in% c("Green_25", "Orange_63", "Light Blue_66"))

add <- data.frame("AID"=972, "fawns"="", "Frequency"=160.23, "tagInfo"=NA, "shortfreq"="60.23", "name"="60.23M?", "Lat"= 41.772640, "Long"= -108.981500) %>% st_as_sf(., coords = c("Long", "Lat"), crs = 4326) %>% st_transform(., crs = st_crs(tocap))

tocap <- bind_rows(tocap, add) 
 
final.point <- tocap %>% 
  group_by(AID) %>%
  #arrange(rev(POSIXct)) %>%
  slice_tail(n=1) %>% dplyr::select(name, AID, geometry)

mapview(final.point, col.regions = "hotpink", legend = FALSE)

#caplist %>% filter(!AID %in% tocap$AID)
#----------------------------------------------------------------#
#### Prepare to Export #### 
#----------------------------------------------------------------#
tocap$name


broken.point <- final.point %>% filter(AID %in% c(old$AID, badGPS$AID))
final.point <- final.point %>% filter(!AID %in% c(old$AID, badGPS$AID))

final.point.ll <- st_transform(final.point, "+proj=latlon +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs")
broken.point.ll <- st_transform(broken.point, "+proj=latlon +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs")

caplist %>% filter(!AID %in% tocap$AID)
tocap %>% filter(AID %in% Capd)
# final.point.sp <- as(final.point.ll, "Spatial")


## check the number of teeth to pull and where ####

# teeth <- as.numeric(c(caplist %>% filter(PullTooth != ""))$AID) 
# teeth <- final.point %>% filter(AID %in% teeth) %>% ungroup()
# mapview(teeth)


# kmlPoints(obj=final.point.sp["name"], kmlfile="C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CapturePlans/2025/March/Superior_Captures_03042025.kml", kmlname="daily points", name = final.point.sp$name)

st_write(final.point.ll, "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CapturePlans/2026/March/Superior_Capture_11MAR2026.kml", driver = "KML", delete_dsn = TRUE, name = final.point.ll$name)

st_write(broken.point.ll, "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CapturePlans/2026/March/Superior_BrokenCollars_11MAR2026.kml", driver = "KML", delete_dsn = TRUE, name = final.point.ll$name)


# #------------------------------------#
# # staging areas ####
# 
stag <- st_zm(st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/other/StagingAreas.kml"),drop = T)

Staging_Areas <- stag %>% filter(!Name %in% c("Steamboat Mtn N", "Point of Rocks", "Cabin Butte S")) %>% mutate(Lat =  st_coordinates(.)[,2], Long = st_coordinates(.)[,1])

mapview(final.point[,"geometry"], popup = NULL, label = "", col.regions = "yellow2", layer.name = "Collars", legend = FALSE, map.types = c("OpenTopoMap", "Esri.WorldImagery")) +
  mapview(Staging_Areas, popup = paste(Staging_Areas$Lat, Staging_Areas$Long, sep=", ") , label = Staging_Areas$Name, legend = TRUE, col.regions = "purple2", map.types = c("OpenTopoMap", "Esri.WorldImagery"))
# 
# #-----------------------#
# # Pairs to check the next day ####
# 
# PairsToCheck <- data.frame(pair = c(1,1,2,2,3,4,4,5,6,7,8,8,9,9,10), fawn = c(951,952,953,954,955,956,957,958,959,960,961,962,963,964,965), adult = c(537,537,509,509,801,360,360,725,610,901,902,902,362,362,631))
# 
# latest <- final.gps %>% arrange(POSIXct) %>% group_by(AID) %>% slice_tail(n=1)
# 
# latest <- latest %>% filter(AID %in% c(PairsToCheck$fawn, PairsToCheck$adult))
# 
# fawnlast <- latest %>% right_join(., PairsToCheck %>% select(fawn, pair), by = c("AID" = "fawn")) %>% st_as_sf(coords = c("Long", "Lat"), crs = st_crs(final.sf))
# adultlast <- latest %>% right_join(., PairsToCheck %>% select(adult, pair), by = c("AID" = "adult")) %>% group_by(AID) %>% slice_tail(n=1) %>% drop_na(Lat)  %>% st_as_sf(coords = c("Long", "Lat"), crs = st_crs(final.sf))
# 
# allast <- rbind(fawnlast %>% ungroup() %>% select(Frequency, pair), adultlast %>% ungroup() %>% select(Frequency, pair)) 
# 
# allast <- allast %>% st_transform(., crs = st_crs("EPSG: 4326")) %>% mutate(pairID = paste(Frequency, pair, sep = "fam")) %>% select(pairID)
# 
# 
# 
# allast.sp <- as(allast, "Spatial")

#kmlPoints(obj=allast.sp["pairID"], kmlfile="C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CapturePlans/2024/December/Superior_PairsToCheck.kml", kmlname="daily points", name = allast.sp$pairID)


