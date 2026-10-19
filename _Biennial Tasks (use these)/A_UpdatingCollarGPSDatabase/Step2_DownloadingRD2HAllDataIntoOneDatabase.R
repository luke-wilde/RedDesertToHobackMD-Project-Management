#last run: 04 11 2023
#reason: added in month day year julian columns where missing



#------------------------------------------------------------------------------------------#
##--WHEN RUNNING THIS CODE, DOWNLOAD FROM THE SERVICES DIRECTLY (DO NOT USE AUTO DOWNLOAD)---------------------------------##

##ATTN -- it is good practice to erase all the TDC downloads and replace them manually for Telonics Collars
##ATTN2 -- restart R before running this, or else the second loop can fail bc of stored functions...

#Code for downloading and cleaning up all collar data in the Sublette Mule Deer Herd
#Collars include ATS Globalstars, ATS Iridium, LOTEK Globalstars, Telonics Globalstars, Telonics Iridium, and Vectronics Iridium
#Written by Anna Ortega, Wyoming Cooperative Fish and Wildlife Research Unit, University of Wyoming
#September 29, 2021

rm(list = ls())

#Load required libraries
library(chron) #install.packages("chron")
library(date) #install.packages("date")
library(tidyverse)
library(lubridate)
#library(rgdal)
library(stringr)
library(sp)
library(sf)
library(beepr)


target.tz <- Sys.timezone()

if(target.tz == "MST"){ message("correct TZ implimented")} else { target.tz = "MST"; message("TZ changed")}

select <- dplyr::select
filter <- dplyr::filter

setwd("C:/Users/lwilde2/Documents/")

#-------------------------------#
#-------------------------------#
# UPDATE THESE ####
Telonics_new <- "/RDH Database/RedDesertToHobackMuleDeerResearchProject/GPS_Collar_Data/Telonics_AllGPSData_FromCollars/AllTelonicsRawDataFromCollars_15APRIL2026.csv"
# VEC_new <- "C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_ATS_Data/ATSIridiumAllDown_FinalGPSData_11DECEMBER2022.csv"
DEERID_new <- "/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_15MARCH2026.csv"
#-------------------------------#
#-------------------------------#


#Import ATS Globalstar data and organize into one single dataframe ####
#Set working directory

ATS_new <- paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RD2H_ATS_Data/ATSIridiumAllDown_FinalGPSData_11DECEMBER2022.csv")

#Import ATS Globalstar data
a<-read.table(paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RD2H_ATS_Data/ATSGlobalstarAllDown_FinalGPSData_10DEC2019.txt"),header=TRUE,sep=",")

#Convert two-digit year into four-digit year
a$Year<- as.numeric(paste("20", a$Year, sep=""))

#Subset each year into separate dataframes
a.Year.16<-subset(a, a$Year==2016)
a.Year.17<-subset(a, a$Year==2017)
a.Year.18<-subset(a, a$Year==2018)
a.Year.19<-subset(a, a$Year==2019)

#Convert day of year into separate month, day, and year columns
a.temp.16<- month.day.year(a.Year.16$Julianday, c(month=1, day =0, year= a.Year.16$Year))
a.temp.17<- month.day.year(a.Year.17$Julianday, c(month=1, day =0, year= a.Year.17$Year))
a.temp.18<- month.day.year(a.Year.18$Julianday, c(month=1, day =0, year= a.Year.18$Year))
a.temp.19<- month.day.year(a.Year.19$Julianday, c(month=1, day =0, year= a.Year.19$Year))

#Rbind the separate year dataframes and the dataframes with day of year conversions
a.d2016 <- data.frame(a.temp.16)
a.Year.16<- cbind(a.Year.16, a.d2016)  

a.d2017 <- data.frame(a.temp.17)
a.Year.17<- cbind(a.Year.17, a.d2017)  

a.d2018 <- data.frame(a.temp.18)
a.Year.18<- cbind(a.Year.18, a.d2018)  

a.d2019 <- data.frame(a.temp.19)
a.Year.19<- cbind(a.Year.19, a.d2019)  

a<-rbind(a.Year.16,a.Year.17,a.Year.18,a.Year.19)
head(a)

#Clean up all data and time columns
a$Hour <- sprintf(paste("%02d"), as.numeric(a$Hour))
a$Minute <- sprintf(paste("%02d"), 00)
a$Sec <- sprintf(paste("%02d"), 00)
a$Time <- paste(a$Hour, a$Minute, a$Sec, sep=":")
a$Month<- sprintf(paste("%02d"), a$month)
a$Date <- paste(a$Year, a$Month, a$day, sep="-")
a$dt <- paste(as.character(a$Date), a$Time, sep=" ")
a$POSIXct <- as.POSIXct(strptime(a$dt, format="%Y-%m-%d %H:%M:%S"), tz=target.tz)
names(a)

#Remove unneccessary columns, rename column headers, organize columns
a <- a[c(1:3,5:8,12,16:18,20)]
colnames(a)<-c("CllrSrN","Year","DayOfYear","Lat","Long","HDOP","NumSats","Day","Time","Month","Date","POSIXct")
a<-a[order(a$CllrSrN,a$POSIXct),]
a$Altitude<-"N/A"
final.a <- a[c("CllrSrN","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long","Altitude","HDOP","NumSats")]

#Omit POSIXct values with null values
final.a<-final.a[!is.na(final.a[,2]),] 

#Remove any rows containing zero values for Latitude and Longitude
final.a<-final.a[final.a$Long!=0 & final.a$Lat!=0, ]

#Remove data with high GPS errors
final.a<-subset(final.a,final.a$HDOP<20)

#Remove rows with probelmatic data and rbind to final dataframe
Cllr.37063<-subset(final.a,final.a$CllrSrN=="37063")
Cllr.37063$POSIXct<-as.character(Cllr.37063$POSIXct)
Cllr.37063<-Cllr.37063[Cllr.37063$POSIXct!="2016-04-29 21:00:00", ]
final.a<-final.a[final.a$CllrSrN!="37063",]
final.a<-rbind(final.a,Cllr.37063)
final.a<-final.a[order(final.a$CllrSrN,final.a$POSIXct),]

rm(a,a.d2016,a.d2017,a.d2018,a.d2019,a.temp.16,a.temp.17,a.temp.18,a.temp.19,
   a.Year.16,a.Year.17,a.Year.18,a.Year.19,Cllr.37063)

#Import ATS Iridium data and organize into one single dataframe ####


#Import ATS Iridium data
b<-read.table(ATS_new,header=TRUE,sep=",")

#Remove date column (won't bind with Jet; use julian day to calculate day)
b<-b[c(1:13)]

names(b)[10] <- "HDOP"

#Import ATS Iridium data from Jet ####
#Set working directory


#Import Jet data
Jet<-read.table(paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/Jet/2016/RawData_March032017/Jet_03032017.txt"),header=TRUE,sep=",")
Jet<-Jet[c(1:13)]

#Combine Jet data with all other ATS Iridium data
b<-rbind(b,Jet)

#Convert two-digit year into four-digit year
b$Year<- as.numeric(paste("20", b$Year, sep=""))

#Subset each year into separate dataframes
b.Year.16<-subset(b, b$Year==2016)
b.Year.17<-subset(b, b$Year==2017)
b.Year.18<-subset(b, b$Year==2018)
b.Year.19<-subset(b, b$Year==2019)
b.Year.22 <- subset(b, b$Year == 2022)

#Convert day of year into separate month, day, and year columns
b.temp.16<- month.day.year(b.Year.16$Julianday, c(month=1, day =0, year= b.Year.16$Year))
b.temp.17<- month.day.year(b.Year.17$Julianday, c(month=1, day =0, year= b.Year.17$Year))
b.temp.18<- month.day.year(b.Year.18$Julianday, c(month=1, day =0, year= b.Year.18$Year))
b.temp.19<- month.day.year(b.Year.19$Julianday, c(month=1, day =0, year= b.Year.19$Year))
b.temp.22<- month.day.year(b.Year.22$Julianday, c(month=1, day =0, year= b.Year.22$Year))

#Rbind the separate year dataframes and the dataframes with day of year conversions
b.d2016 <- data.frame(b.temp.16)
b.Year.16<- cbind(b.Year.16, b.d2016)  

b.d2017 <- data.frame(b.temp.17)
b.Year.17<- cbind(b.Year.17, b.d2017)  

b.d2018 <- data.frame(b.temp.18)
b.Year.18<- cbind(b.Year.18, b.d2018)  

b.d2019 <- data.frame(b.temp.19)
b.Year.19<- cbind(b.Year.19, b.d2019)  

b.d2022 <- data.frame(b.temp.22)
b.Year.22<- cbind(b.Year.22, b.d2022)

b<-rbind(b.Year.16,b.Year.17,b.Year.18,b.Year.19,b.Year.22)

#Clean up all data and time columns
b$Hour <- sprintf(paste("%02d"), as.numeric(b$Hour))
b$Minute <- sprintf(paste("%02d"), as.numeric(b$Minute))
b$Sec <- sprintf(paste("%02d"), 00)
b$Time <- paste(b$Hour, b$Minute, b$Sec, sep=":")
b$Month<- sprintf(paste("%02d"), b$month)
b$Date <- paste(b$Year, b$Month, b$day, sep="-")
b$dt <- paste(as.character(b$Date), b$Time, sep=" ")
b$POSIXct <- as.POSIXct(strptime(b$dt, format="%Y-%m-%d %H:%M:%S"), tz=target.tz)
names(b)

#Remove unneccessary columns, rename column headers, organize columns
b <- b[c(1:3,8:11,15,18:20,22)]
colnames(b)<-c("CllrSrN","Year","DayOfYear","Lat","Long","HDOP","NumSats","Day","Time","Month","Date","POSIXct")
b$Altitude<-"N/A"
b<-b[order(b$CllrSrN,b$POSIXct),]
final.b <- b[c("CllrSrN","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long","Altitude","HDOP","NumSats")]

#Omit POSIXct values with null values
final.b<-final.b[!is.na(final.b[,2]),] 

#Remove any rows containing zero values for Latitude and Longitude
final.b<-final.b[final.b$Long!=0 & final.b$Lat!=0, ]

#Remove points with high GPS error
final.b<-subset(final.b,final.b$HDOP<20)

#Remove rows with probelmatic data and rbind to final dataframe
Cllr.36830<-subset(final.b,final.b$CllrSrN=="36830")
Cllr.36830<-Cllr.36830[Cllr.36830$POSIXct!="2017-11-07 03:00:00", ]
Cllr.36830<-Cllr.36830[Cllr.36830$POSIXct!="2017-11-07 05:00:00", ]
Cllr.36830<-Cllr.36830[Cllr.36830$POSIXct!="2017-11-07 07:00:00", ]
final.b<-final.b[final.b$CllrSrN!="36830",]
final.b<-rbind(final.b,Cllr.36830)
final.b<-final.b[order(final.b$CllrSrN,final.b$POSIXct),]
head(final.b)

rm(b,b.d2016,b.d2017,b.d2018,b.d2019,b.temp.16,b.temp.17,b.temp.18,b.temp.19,
   b.Year.16,b.Year.17,b.Year.18,b.Year.19,Cllr.36830,Jet)

#Import LOTEK data directly from LOTEK Data Collector ####
#Set working directory and Obtain a list of all csv files for LOTEK collars

e<-read.csv(paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RD2H_LOTEK_Data/AllLOTEKFromWebService.csv"), header=TRUE, sep=",")
names(e)

#Remove GPS fixes with massive errors
e<-subset(e, e$DOP<20)
e<-subset(e, e$Altitude>1500)

#Reformat date and time columns
#Make sure date and time columns are in MST and NOT in GMT
e$POSIXct <- as.POSIXct(as.Date(e$Date...Time..GMT. ,origin = as.POSIXct("1899-12-30", tz = 'GMT')), tz = 'UTC') 
attr(e$POSIXct, "tzone") <- "GMT"
attr(e$POSIXct, "tzone") <- target.tz
e$Month <- month(e$POSIXct)
e$Month<- sprintf(paste("%02d"), e$Month)
e$Day <- day(e$POSIXct)
e$Year <- year(e$POSIXct)
e$Hour <- hour(e$POSIXct)
e$Minute <- sprintf(paste("%02d"), 00)
e$Sec <- sprintf(paste("%02d"), 00)
e$Time <- paste(e$Hour, e$Minute, e$Sec, sep=":")
e$Date <- paste(e$Year, e$Month, e$Day, sep="-")
e$dt <- paste(as.character(e$Date), e$Time, sep=" ")
e$POSIXct <- as.POSIXct(strptime(e$dt, format="%Y-%m-%d %H:%M:%S"), tz=target.tz)
e$DayOfYear <- yday(e$POSIXct)
head(e)
names(e)

#Remove unneccessary columns, rename column headers, organize columns
e <- e[c(1,4:6,8,12:15,19:20,22)]
colnames(e)<-c("CllrSrN","Lat","Long","Altitude","HDOP","POSIXct","Month","Day","Year","Time","Date","DayOfYear")
e$NumSats<-"N/A"
e<-e[order(e$CllrSrN,e$POSIXct),]
final.e <- e[c("CllrSrN","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long","Altitude","HDOP","NumSats")]

#Omit POSIXct values with null values
final.e<-final.e[!is.na(final.e[,2]),] 

#Remove any rows containing zero values for Latitude and Longitude
final.e<-final.e[final.e$Long!=0 & final.e$Lat!=0, ]
head(final.e)

rm(e)

#Import Telonics Globalstar data directly from TDC ####
#Set working directory
dir.tdc <- paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RD2H_Telonics_Data/IndividualRawGlobalstarDataFromTDC/")

rm(t)

#Loop through every CSV file, remove unncessary rows, create CllrSrN and ESN column
read.csv.fx <- function(x){
  tryCatch({
  t <- read.csv(x,header = TRUE, skip = 22, stringsAsFactors = FALSE) # Note! This function breaks if data in csv starts sooner or later than line 23
  CllrSrN <- read.csv(x,header = FALSE, skip = 6, nrows = 1, as.is = TRUE)
  t$CllrSrN <- CllrSrN[,2]
  return(t)}, error = function(e) {
    
    message("Failed for ", x," -- ", e$message)
    }
    )
}

head(t)

#Bind all csv files together and convert into a single dataframe
files <- list.files(dir.tdc,full.names = TRUE)
files <- files[grepl(".csv",files)]
gps <- lapply(files,read.csv.fx)
gps <- bind_rows(gps)

#Extract GPS fixes that have been successful
gps<-gps[gps$GPS.Fix.Attempt!="Failed", ]
gps<-gps[gps$GPS.Fix.Attempt!="Unresolved QFP", ]
gps<-gps[gps$GPS.Fix.Attempt!="", ]
table(gps$GPS.Fix.Attempt)

#Ensure that there are the correct number of unique Telonics collars (n = 65)
unique.cllr<-unique(gps$CllrSrN); unique.cllr

#Create new GPS dataframe
t<-gps

names(t)

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
attr(t$POSIXct, "tzone") <- target.tz

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
t$POSIXct <- as.POSIXct(strptime(t$dt, format="%Y-%m-%d %H:%M:%S"), tz=target.tz)
t$DayOfYear <- yday(t$POSIXct)
head(t)
names(t)

#Remove unneccessary columns, rename column headers, organize columns

t <- t[c(
  "GPS.Latitude",           # 5
  "GPS.Longitude",          # 6
  "GPS.Horizontal.Error",   # 10
  "CllrSrN",                # 18
  "Year",                   # 28
  "Month",                  # 29
  "Day",                    # 30
  "Time",                   # 35
  "Date",                   # 36
  "POSIXct",                # 38
  "DayOfYear"               # 39
)]


colnames(t)<-c("Lat","Long","HDOP","CllrSrN","Year","Month","Day","Time","Date","POSIXct","DayOfYear")
t$Altitude<-"N/A"
t$NumSats<-"N/A"
t<-t[order(t$CllrSrN,t$POSIXct),]
final.t <- t[c("CllrSrN","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long","Altitude","HDOP","NumSats")]

head(final.t %>% arrange(POSIXct))
tail(final.t %>% arrange(POSIXct))

#Note, I did not remove any HDOP errors, because unlike LOTEK...I can't quite figure out what values of HDOP are truly errors

#Omit POSIXct values with null values
final.t<-final.t[!is.na(final.t[,2]),] 

#Remove any rows containing zero values for Latitude and Longitude
final.t<-final.t[final.t$Long!=0 & final.t$Lat!=0, ]
head(final.t)
tail(final.t)

rm(gps,t)

#Import Telonics Iridium data directly from TDC ####
#Set working directory
dir.tdc.i <- paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RD2H_Telonics_Data/IndividualRawIridiumDataFromTDC")

#Loop through every CSV file, remove unncessary rows, create CllrSrN and ESN column
read.csv.fx <- function(x){
  m <- read.csv(x,header = TRUE, skip = 22, stringsAsFactors = FALSE) # Note! This function breaks if data in csv starts sooner or later than line 23
  CllrSrN <- read.csv(x,header = FALSE, skip = 6, nrows = 1, as.is = TRUE)
  m$CllrSrN <- CllrSrN[,2]
  return(m)
}

#Bind all csv files together and convert into a single dataframe
files.i <- list.files(dir.tdc.i,full.names = TRUE)
files.i <- files.i[grepl(".csv",files.i)]
gps.i <- lapply(files.i,read.csv.fx)
gps.i <- bind_rows(gps.i)
head(gps.i)

#Extract GPS fixes that have been successful
gps.i<-gps.i[gps.i$GPS.Fix.Attempt!="Failed", ]
gps.i<-gps.i[gps.i$GPS.Fix.Attempt!="Unresolved QFP", ]
gps.i<-gps.i[gps.i$GPS.Fix.Attempt!="", ]
table(gps.i$GPS.Fix.Attempt)

#Ensure that there are the correct number of unique Telonics collars (n = 2)
unique.cllr.i<-unique(gps.i$CllrSrN)

#Create new GPS dataframe
m<-gps.i

#Split date and time column into two separate columns
m$tempdate<- sapply(m$Acquisition.Time, FUN = function(x){unlist(strsplit(x, " "))[1]})
m$temptime<- sapply(m$Acquisition.Time, FUN = function(x){unlist(strsplit(x, " "))[2]})

#Split date column into year, month, and day columns
m$Year<-substr(m$tempdate, 1, 4) 
m$Month<-substr(m$tempdate, 6, 7) 
m$Day<-substr(m$tempdate, 9, 10) 

#Combine month, day, and year column into a single date column with correct format
m$date<- paste(m$Year, m$Month, m$Day,  sep="-")

#Clean up all date and time columns
m$Hour <- format(strptime(m$temptime,"%H:%M:%S"),'%H')
m$Minute <- sprintf(paste("%02d"), 00)
m$Sec <- sprintf(paste("%02d"), 00)
m$Time <- paste(m$Hour, m$Minute, m$Sec, sep=":")
m$Date <- paste(m$Year, m$Month, m$Day, sep="-")
m$dt <- paste(as.character(m$Date), m$Time, sep=" ")
m$POSIXct <- as.POSIXct(strptime(m$dt, format="%Y-%m-%d %H:%M:%S"), tz="UTC")
attr(m$POSIXct, "tzone") <- "GMT"
attr(m$POSIXct, "tzone") <- target.tz
head(m)

#Reconvert date and time data to match MST time zone
m$Month <- month(m$POSIXct)
m$Month<- sprintf(paste("%02d"), m$Month)
m$Day <- day(m$POSIXct)
m$Year <- year(m$POSIXct)
m$Hour <- hour(m$POSIXct)
m$Minute <- sprintf(paste("%02d"), 00)
m$Sec <- sprintf(paste("%02d"), 00)
m$Time <- paste(m$Hour, m$Minute, m$Sec, sep=":")
m$Date <- paste(m$Year, m$Month, m$Day, sep="-")
m$dt <- paste(as.character(m$Date), m$Time, sep=" ")
m$POSIXct <- as.POSIXct(strptime(m$dt, format="%Y-%m-%d %H:%M:%S"), tz=target.tz)
m$DayOfYear <- yday(m$POSIXct)
head(m)
names(m)

#Remove unneccessary columns, rename column headers, organize columns

m <- m[c(
  "GPS.Latitude",           # was 8
  "GPS.Longitude",          # was 9
  "GPS.Horizontal.Error",   # was 13  (use GPS.Horizontal.Dilution if that's your HDOP)
  "CllrSrN",                # was 24
  "Year",                   # was 27
  "Month",                  # was 28
  "Day",                    # was 29
  "Time",                   # was 34
  "Date",                   # was 35
  "POSIXct",                # was 37
  "DayOfYear"               # was 38
)]

colnames(m)<-c("Lat","Long","HDOP","CllrSrN","Year","Month","Day","Time","Date","POSIXct","DayOfYear")
m$Altitude<-"N/A"
m$NumSats<-"N/A"
m<-m[order(m$CllrSrN,m$POSIXct),]
final.m <- m[c("CllrSrN","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long","Altitude","HDOP","NumSats")]
head(final.m)

#Note, I did not remove any HDOP errors, because unlike LOTEK...I can't quite figure out what values of HDOP are truly errors

#Omit POSIXct values with null values
final.m<-final.m[!is.na(final.m[,2]),] 

#Remove any rows containing zero values for Latitude and Longitude
final.m<-final.m[final.m$Long!=0 & final.m$Lat!=0, ]
head(final.m)
tail(final.m %>% arrange(POSIXct))
unique(final.m$CllrSrN)

rm(gps.i,m,dir.tdc,dir.tdc.i,files.i,unique.cllr,unique.cllr.i)

#Import Iridium Vectronics data from GPS Plus X ####
dir.tdc.v <- paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RD2H_Vectronics_Data/IndividualRawIridiumDataFromGPSPlusX")

#Loop through every CSV file, remove unnecessary rows, create CllrSrN and ESN column
read.csv.fx <- function(x){
  t <- read.csv(x,header = TRUE, stringsAsFactors = FALSE, check.names = F) # Note! This function breaks if data in csv starts sooner or later than line 23
  CllrSrN <- read.csv(x,header = FALSE, skip = 6, nrows = 1, as.is = TRUE, check.names = F)
  t$CllrSrN <- CllrSrN[,2]
  return(t)
}

#Bind all csv files together and convert into a single dataframe
files <- list.files(dir.tdc.v,full.names = TRUE)
files <- files[grepl(".csv",files)]
gps <- lapply(files,read.csv.fx)
gps <- bind_rows(gps)


head(gps)

gps$UTC_Time <- format(strptime(gps$UTC_Time, format = "%I:%M:%S %p"), format = "%H:%M:%S")

#Ensure that there are the correct number of unique Telonics collars (n = 2)
unique.cllr<-unique(gps$CllrSrN)

#Create new GPS dataframe
v<-gps
names(v)

#Remove unnecessary columns
v<-v[c(2:4,13:16)]
colnames(v)<-c("CllrSrN","Date","Time","Lat","Long","Altitude","HDOP")

#Split date column into year, month, and day columns
v$Year <- str_split_fixed(v$Date, "/", 3)[,3]
v$Month <- str_split_fixed(v$Date, "/", 3)[,1]
v$Day <- str_split_fixed(v$Date, "/", 3)[,2]
head(v)
tail(v)
#Extract hour from time column
v$Hour <- str_split_fixed(v$Time, ":", 3)[,1]
head(v)
tail(v)

#Clean up all date and time columns
v$Minute <- sprintf(paste("%02d"), 00)
v$Sec <- sprintf(paste("%02d"), 00)
v$Time <- paste(v$Hour, v$Minute, v$Sec, sep=":")
v$Date<- paste(v$Year, v$Month, v$Day,  sep="-")
v$dt <- paste(as.character(v$Date), v$Time, sep=" ")
v$POSIXct <- as.POSIXct(strptime(v$dt, format="%Y-%m-%d %H:%M:%S"), tz="UTC")
attr(v$POSIXct, "tzone") <- "GMT"
attr(v$POSIXct, "tzone") <- target.tz

#Reconvert date and time data to match MST time zone
v$Month <- month(v$POSIXct)
v$Month<- sprintf(paste("%02d"), v$Month)
v$Day <- day(v$POSIXct)
v$Year <- year(v$POSIXct)
v$Hour <- hour(v$POSIXct)
v$Minute <- sprintf(paste("%02d"), 00)
v$Sec <- sprintf(paste("%02d"), 00)
v$Time <- paste(v$Hour, v$Minute, v$Sec, sep=":")
v$Date <- paste(v$Year, v$Month, v$Day, sep="-")
v$dt <- paste(as.character(v$Date), v$Time, sep=" ")
v$POSIXct <- as.POSIXct(strptime(v$dt, format="%Y-%m-%d %H:%M:%S"), tz=target.tz)
v$DayOfYear <- yday(v$POSIXct)
head(v)
names(v)

#Remove unneccessary columns, rename column headers, organize columns
v <- v[c(1:10,15:16)]
v$NumSats<-"N/A"
v<-v[order(v$CllrSrN,v$POSIXct),]
final.v <- v[c("CllrSrN","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long","Altitude","HDOP","NumSats")]
head(final.v)

#Note, I did not remove any HDOP errors, because unlike LOTEK...I can't quite figure out what values of HDOP are truly errors

#Omit POSIXct values with null values
final.v<-final.v[!is.na(final.v[,2]),] 

#Remove any rows containing zero values for Latitude and Longitude
final.v<-final.v[final.v$Long!=0 & final.v$Lat!=0, ]
head(final.v)

rm(gps,v,dir.tdc.v,files,unique.cllr)

#Import LOTEK data that has been downloaded directly from collars ####

f<-read.csv(paste0(getwd(),"/RDH Database/RedDesertToHobackMuleDeerResearchProject/GPS_Collar_Data/LOTEK_AllGPSData_FromCollars/AllLOTEKRawDataFromCollars_24JANUARY2022.csv"))

#Reformat date and time columns
f$Month<- sprintf(paste("%02d"), f$Month)
f$Date <- paste(f$Year, f$Month, f$Day, sep="-")
f$dt <- paste(as.character(f$Date), f$Time, sep=" ")
f$POSIXct <- as.POSIXct(strptime(f$dt, format="%Y-%m-%d %H:%M:%S"), origin = "1970-01-01", tz=target.tz)
names(f)

#Remove unncessary columns
final.f <- f[c(1:13)]
names(final.f)

final.f$POSIXct

rm(f)

#Import Telonics data that has been downloaded directly from collars ####

p<-read.csv(paste0(getwd(),Telonics_new)); head(p)

#Reformat date and time columns
p$Month<- sprintf(paste("%02d"), p$Month)
p$Date <- paste(p$Year, p$Month, p$Day, sep="-")
p$dt <- paste(as.character(p$Date), p$Time, sep=" ")
p$POSIXct <- as.POSIXct(strptime(p$dt, format="%Y-%m-%d %H:%M:%S"), origin = "1970-01-01", tz=target.tz)

#Remove unncessary columns
final.p <- p[c(1:13)]
names(final.p)

range(final.p$POSIXct)

unique(final.p$CllrSrN)
rm(p)


#Import Vectronic data that has been downloaded directly from collars ####

vv<-read.csv(paste0(getwd(),"/RDH Database/RedDesertToHobackMuleDeerResearchProject/GPS_Collar_Data/Vectronic_AllGPSData_FromCollars/CSV/GPS_Collar46523_20220401155515.csv"), check.names = F); str(vv); names(vv)

vv <- vv %>% dplyr::select(CollarID, UTC_Date, UTC_Time, names(vv)[13], names(vv)[14], names(vv)[15], DOP, Sats)

#Reformat date and time columns
vv$dt <- paste(as.character(vv$UTC_Date), vv$UTC_Time, sep=" ")
vv$POSIXct <- as.POSIXct(strptime(vv$dt, format="%m/%d/%Y %H"), origin = "1970-01-01", tz=target.tz)
vv$Day <- day(as.POSIXct(vv$POSIXct))
vv$Month <- month(as.POSIXct(vv$POSIXct))
vv$Year <- year(as.POSIXct(vv$POSIXct))
vv$DayOfYear <- yday(as.POSIXct(vv$POSIXct))
vv$Hour <- hour(vv$POSIXct)
vv$Minute <- sprintf(paste("%02d"), 00)
vv$Sec <- sprintf(paste("%02d"), 00)
vv$Time <- paste(vv$Hour, vv$Minute, vv$Sec, sep=":")
vv$Date <- paste(vv$Year, vv$Month, vv$Day, sep="-")

names(vv)[c(4,5,6)] <- c( "Latitude....", "Longitude....", "Height..m.")

#Remove unncessary columns
final.vv <- vv %>% dplyr::select(CollarID, POSIXct, Month, Day, Year, DayOfYear, Date, Time, Latitude...., Longitude...., Height..m., DOP, Sats)
names(final.vv)

names(final.vv) <- names(final.t)

head(final.vv)

rm(vv)

#Import collar data from homemade collars in 2014 and 2015 ####

g<-read.csv(paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RD2H_HomemadeCollar_Data/AllHomemadeCollarData_2014_2015.csv"))

#Reformat date and time columns
g$Month<- sprintf(paste("%02d"), g$Month)
g$Date <- paste(g$Year, g$Month, g$Day, sep="-")
g$dt <- paste(as.character(g$Date), g$Time, sep=" ")
g$POSIXct <- as.POSIXct(strptime(g$dt, format="%Y-%m-%d %H:%M:%S"), origin = "1970-01-01", tz=target.tz)
head(g)

#Remove unncessary columns
final.g <- g[c(1:10)]
final.g$Altitude<-"N/A"
final.g$HDOP<-"N/A"
final.g$NumSats<-"N/A"
head(final.g)

#Remove rows with probelmatic data and rbind to final dataframe
Cllr.2014115<-subset(final.g,final.g$CllrSrN=="2014115")
Cllr.2014115$POSIXct<-as.character(Cllr.2014115$POSIXct)
Cllr.2014115<-Cllr.2014115[Cllr.2014115$POSIXct!="2015-03-15 23:06:00", ]
final.g<-final.g[final.g$CllrSrN!="2014115",]
final.g<-rbind(final.g,Cllr.2014115)
final.g<-final.g[order(final.g$CllrSrN,final.g$POSIXct),]

rm(Cllr.2014115,g)

#Import ATS Globalstar data that has been downloaded directly from collars ####

h<-read.csv(paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RD2H_ATS_Data/AllATSDataFromCollars_21JAN2019.CSV")) # 

#Reformat date and time columns
h$Month<- sprintf(paste("%02d"), h$Month)
h$Date <- paste(h$Year, h$Month, h$Day, sep="-")
h$dt <- paste(as.character(h$Date), h$Time, sep=" ")
h$POSIXct <- as.POSIXct(strptime(h$dt, format="%Y-%m-%d %H:%M:%S"), origin = "1970-01-01", tz=target.tz)
names(h)

#Remove unncessary columns
final.h <- h[c(1:13)]

#Remove points with high GPS error
final.h<-subset(final.h,final.h$HDOP<20)

rm(h)

#Merge all data frames ### FINAL E HAS PROBLEM, CHARACTER STRING NOT IN UNAMBIG FORMAT
allData<-rbind(final.a,final.b,final.e,final.t,final.m,final.v,final.f,final.p,final.g,final.h, final.vv) #final.a,final.b,final.e,final.t,final.m,final.v,final.f,final.p,final.g,final.h
head(allData)

#Remove duplicated rows for every unique CllrSrN and POSIXct
finalData<-allData[!duplicated(allData[1:2]),]
#View(finalData)
head(finalData);tail(finalData)

#finalData %>% filter(CllrSrN %in% unique(finalData$CllrSrN)[c(362,363,364,372)])

range(na.omit(finalData$POSIXct))
range(na.omit(final.p$POSIXct))

#Merge DeerIDs based on unique CllrSrN and range of dates

DeerID.Data <- read.csv(paste0(getwd(),DEERID_new), header=TRUE, sep=",")

#Format date and time columns
DeerID.Data$StartDate <- as.POSIXct(DeerID.Data$StartDate, format="%m/%d/%Y", origin = "1970-01-01")
DeerID.Data$StopDate <- as.POSIXct(DeerID.Data$StopDate,format="%m/%d/%Y", origin = "1970-01-01")

DeerID.Data$Startdt <- paste(as.character(DeerID.Data$StartDate), DeerID.Data$StartTime, sep=" ")
DeerID.Data$StartDateTime <- as.POSIXct(strptime(DeerID.Data$Startdt, format="%Y-%m-%d %H:%M:%S"), origin = "1970-01-01", tz=target.tz)

DeerID.Data$Stopdt <- paste(as.character(DeerID.Data$StopDate), DeerID.Data$StopTime, sep=" ")
DeerID.Data$StopDateTime <- as.POSIXct(strptime(DeerID.Data$Stopdt, format="%Y-%m-%d %H:%M:%S"), origin = "1970-01-01", tz=target.tz)
names(DeerID.Data)

#Remove unncessary columns
DeerID.Data<- DeerID.Data[c(1:7,10)]
head(DeerID.Data)

#Sort unique collar serial numbers; make sure the serial numbers look correct
sort(unique(finalData$CllrSrN))
sort(unique(DeerID.Data$CllrSrN))

#Merge all GPS collar data with Deer ID csv by CllrSrN                
mydata <- merge(finalData, DeerID.Data, by=c("CllrSrN"), all=TRUE) 

#Organize dataframe by CllrSrN, POSIXct, and AID
mydata<-mydata[order(mydata$CllrSrN, mydata$POSIXct, mydata$AID),]

#Omit any deer IDs that are N/A
mydata<-mydata[!is.na(mydata[,11]),] 

#Convert date and time columns into character data type
mydata$StartDateTime<-as.character(mydata$StartDateTime)
mydata$StopDateTime<-as.character(mydata$StopDateTime)
mydata$POSIXct<-as.character(mydata$POSIXct)

#Subset data based on StopDateTime (NULL values present vs. NULL values not present)
my.data.Null<-subset(mydata, is.na(mydata$StopDateTime))
my.data.NoNull<-subset(mydata, !is.na(mydata$StopDateTime))

#Identify unique IDs and create empty output dataframe for data in which StopDateTime contains NULL values
Deer.ID.1 <- unique(my.data.Null$AID)
Deer.CllrSrN.1 <- unique(my.data.Null$CllrSrN) 

system.time(  
full.df.1 <- my.data.Null %>% filter(AID %in% Deer.ID.1, CllrSrN %in% Deer.CllrSrN.1) %>%
  group_by(AID, CllrSrN) %>%
  filter(POSIXct >= StartDateTime) %>%
  ungroup()
)


#then do it for the NoNull (ie deer with death or malfunction dates)
Deer.ID.2 <- unique(my.data.NoNull$AID)
Deer.CllrSrN.2 <- unique(my.data.NoNull$CllrSrN)

system.time(  
# Filter and bind data using dplyr
full.df.2 <- my.data.NoNull %>%
  filter(AID %in% Deer.ID.2, CllrSrN %in% Deer.CllrSrN.2) %>%
  group_by(AID, CllrSrN) %>%
  filter(POSIXct >= StartDateTime & POSIXct <= StopDateTime) %>%
  ungroup()
)

# Ortega version -- Slower version check for consistency
# 
# #Identify unique IDs and create empty output dataframe for data in which StopDateTime contains NULL values
# Deer.ID.1 <- unique(my.data.Null$AID)
# Deer.CllrSrN.1 <- unique(my.data.Null$CllrSrN) 
# df.1<- data.frame()
# df.1<-as.data.frame(my.data.Null)
# full.df.1<-df.1[0, ]
# 
# #check if CllrSrN is good now
# Deer.CllrSrN.1 
# 
# #old Ortega version
# system.time(
#   for (i in 1:length (Deer.ID.1)){
#     temp.2<-subset(my.data.Null, AID==Deer.ID.1[i])
#     for (j in 1:length (Deer.CllrSrN.1)){
#       temp.3<-subset(temp.2, CllrSrN==Deer.CllrSrN.1[j])
#       sub.1<-subset(temp.3, temp.3$POSIXct >= temp.3$StartDateTime)
#       full.df.1<-rbind(full.df.1,sub.1)
#     }
#   }
# )
# #Identify unique IDs and create empty output dataframe for data in which StopDateTime does not contain NULL values
# Deer.ID.2 <- unique(my.data.NoNull$AID)
# Deer.CllrSrN.2 <- unique(my.data.NoNull$CllrSrN)
# df.2<- data.frame()
# df.2<-as.data.frame(my.data.NoNull)
# full.df.2<-df.2[0, ]
# 
# #Create for loop that creates unique Deer IDs with associated CllrSrN based on range of dates
# for (i in 1:length (Deer.ID.2)){
#   temp.2<-subset(my.data.NoNull, AID==Deer.ID.2[i])
#   for (j in 1:length (Deer.CllrSrN.2)){
#     temp.3<-subset(temp.2, CllrSrN==Deer.CllrSrN.2[j])
#     sub.1<-subset(temp.3, temp.3$POSIXct >= temp.3$StartDateTime & temp.3$POSIXct<=temp.3$StopDateTime)
#     full.df.2<-rbind(full.df.2,sub.1)
#   }
# }

#Rbind both dataframes
clean.Data<-rbind(full.df.1,full.df.2) #clean.Data<-rbind(full.df.1,full.df.2_par)

#Ensure that frequency field as 3 decimal places
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
clean.Data$Frequency<-specify_decimal(clean.Data$Frequency, 3)
names(clean.Data)




#Remove unnessary columns and reorganize columns
#head(clean.Data[c(1:10,14:15,18)])
#clean.Data<-clean.Data[c(1:10,14:15,18)]
clean.Data<-clean.Data[order(clean.Data$AID, clean.Data$POSIXct),]
clean.Data <- clean.Data %>% dplyr::select(Mgtry,AID,CllrSrN,Frequency,POSIXct,Month,Day, Year, DayOfYear,Date,Time,Lat,Long,HDOP) %>% mutate(HDOP = as.numeric(HDOP))

clean.Data %>% dplyr::select(HDOP) %>% filter(is.na(HDOP)) %>% reframe(n()/nrow(clean.Data)) #missing HDOP for 3% of points

clean.Data2 <- clean.Data
str(clean.Data2)
table(is.na(clean.Data2))

#fix month, day, year issue
clean.Data2 <- clean.Data2 %>% mutate(POSIXct = ymd_hms(POSIXct, tz = target.tz), Year = year(POSIXct), Month = month(POSIXct), Day = mday(POSIXct), JDate = yday(POSIXct))

#check tz of POSIXct
tz(clean.Data2$POSIXct)

#make spatial
clean.Data2 <- clean.Data2 %>% st_as_sf(., coords = c("Long","Lat"), crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) %>% mutate(Lat = st_coordinates(.)[,2], Long = st_coordinates(.)[,1]) %>% st_transform(., CRS("EPSG:32612")) %>% mutate(UTM_N = st_coordinates(.)[,2], UTM_E = st_coordinates(.)[,1])

clean.Data2 <- clean.Data2 %>% filter(!is.na(POSIXct), !is_empty(geometry))




clean.Data2 %>% 
  group_by(AID) %>%
  summarise(max_time = max(.data[["POSIXct"]], na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(max_time))


# 
# st_write(clean.Data2, "C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllData.shp", append=F)
# 
# #Export data into csv file for future data analyses and backup
# write.csv(clean.Data2,"C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllData.csv", row.names = F)

#Save data as a .RData file
save(clean.Data2,file=paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RD2H_AllData.RData"))

load(paste0(getwd(),"/RDH Database/RD2H_GPSCollarData/RD2H_AllData.RData"))
