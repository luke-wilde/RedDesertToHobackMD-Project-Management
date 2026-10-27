Sys.setenv(TZ="America/Denver")

target.tz <- Sys.timezone()

if(target.tz == "America/Denver"){ message("correct TZ implimented")} else { target.tz = "America/Denver"; message("TZ changed")}

#Downloading and organizing all Telonics data that has been downloaded directly from the collars
#Code written by Anna Ortega, Wyoming Cooperative Fish and Wildlife Research Unit
#January 21, 2019

# DONT FORGET TO ERASE THE FIRST COLUMN or keep rownames false#

## ATTN:: !! Pay close attention to the format of the TDC downloads, the columns are sometimes not where they should be -- consider changing over to tidyverse and calling by name instead !! ##

# ENTER THE NEW NAME, BASED ON THE UPDATE TIME
UpdateName <- "27OCTOBER2026"

#Load required libraries
library(lubridate)
library(tidyverse)
#library(rgdal) #deprecated
library(sf)

#Import Telonics data directly from Globalstar collars ####
#Set working directory
dir.tdc <- "C:/Users/lwilde2/Documents/RDH Database/RedDesertToHobackMuleDeerResearchProject/GPS_Collar_Data/Telonics_AllGPSData_FromCollars/RawCSVFilesGlobalstarCollars"

#Loop through every CSV file, remove unnecessary rows, create CllrSrN and ESN column
read.csv.fx <- function(x){
  t <- read.csv(x,header = TRUE, skip = 22, stringsAsFactors = FALSE) # Note! This function breaks if data in csv starts sooner or later than line 23
  CllrSrN <- read.csv(x,header = FALSE, skip = 7, nrows = 1, as.is = TRUE)
  t$CllrSrN <- CllrSrN[,2]
  return(t)
}

#Bind all csv files together and convert into a single dataframe
files <- list.files(dir.tdc,full.names = TRUE)
files <- files[grepl(".csv",files)]
gps <- lapply(files,read.csv.fx)
gps <- bind_rows(gps)
head(gps); tail(gps)

str(gps)
unique(gps$CllrSrN)

table(is.na(gps$CllrSrN))


#Remove GPS fixes that have failed or have zero data
gps<-gps[gps$GPS.Fix.Attempt!="Failed", ]
gps<-gps[gps$GPS.Fix.Attempt!="Unresolved QFP", ]
gps<-gps[gps$GPS.Fix.Attempt!="", ]
table(gps$GPS.Fix.Attempt)

#Remove GPS fixes with large errors
gps<-subset(gps, gps$GPS.Horizontal.Dilution<20)
gps<-subset(gps, gps$GPS.Altitude>1500)

head(gps); tail(gps)

#Create new GPS dataframe
t<-gps
head(t)

#Split date and time column into two separate columns
t$tempdate<- sapply(t$Acquisition.Time, FUN = function(x){unlist(strsplit(x, " "))[1]})
t$temptime<- sapply(t$Acquisition.Time, FUN = function(x){unlist(strsplit(x, " "))[2]})

#Split date column into year, month, and day columns
t$Year<-substr(t$tempdate, 1, 4) 
t$Month<-substr(t$tempdate, 6, 7) 
t$Day<-substr(t$tempdate, 9, 10) 

#Combine month, day, and year column into a single date column with correct format
t$date<- paste(t$Year, t$Month, t$Day,  sep="-")
head(t)

#Clean up all date and time columns
t$Hour <- format(strptime(t$temptime,"%H:%M:%S"),'%H')
t$Minute <- sprintf(paste("%02d"), 00)
t$Sec <- sprintf(paste("%02d"), 00)
t$Time <- paste(t$Hour, t$Minute, t$Sec, sep=":")
t$Date <- paste(t$Year, t$Month, t$Day, sep="-")
t$dt <- paste(as.character(t$Date), t$Time, sep=" ")
t$POSIXct <- as.POSIXct(strptime(t$dt, format="%Y-%m-%d %H:%M:%S"), tz="UTC")
head(t)
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
t$Date <- lubridate::date(t$POSIXct)
t$Minute <- lubridate::minute(t$POSIXct)
t$Sec <- lubridate::second(t$POSIXct)
t$Hour <- lubridate::hour(t$POSIXct)
head(t); tail(t)
names(t)

unique(t$CllrSrN) # check for NAs
table(is.na(t$CllrSrN))


#check that there is not an issue with the ordering or structure of columns

# Assuming 'df' is your dataframe
sapply(names(t), function(col_name) {
  # Get the class of the first few entries
  first_few_entries_class <- class(t[1:5, col_name])
  
  # Check if all entries in the column are of the same class
  same_class <- all(sapply(t[, col_name], function(x) class(x) == first_few_entries_class))
  
  # Return the result
  if (same_class) {
    return(paste("All entries in column", col_name, "are of class", first_few_entries_class))
  } else {
    return(paste("Not all entries in column", col_name, "are of class", first_few_entries_class))
  }
})


#Remove unneccessary columns, rename column headers, organize columns
head(t[c(5:6,10,14,16,21,26:28,33:34,36:37)])
t <- t[c(5:6,10,14,16,21,26:28,33:34,36:37)]
colnames(t)<-c("Lat","Long","Altitude","HDOP","NumSats","CllrSrN","Year","Month","Day","Time","Date","POSIXct","DayOfYear")
t<-t[order(t$CllrSrN,t$POSIXct),]
final.t <- t[c("CllrSrN","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long","Altitude","HDOP","NumSats")]
head(final.t)
str(final.t)

#Omit POSIXct values with null values
final.t<-final.t[!is.na(final.t[,2]),] 

#Remove any rows containing zero values for Latitude and Longitude
final.t<-final.t[final.t$Long!=0 & final.t$Lat!=0, ]

#Remove rows with probelmatic data and rbind to final dataframe
Cllr.701421A<-subset(final.t,final.t$CllrSrN=="701421A")
Cllr.701421A$POSIXct<-as.character(Cllr.701421A$POSIXct)
Cllr.701421A<-Cllr.701421A[Cllr.701421A$POSIXct!="2019-06-20 11:00:00", ]

Cllr.701425A<-subset(final.t,final.t$CllrSrN=="701425A")
Cllr.701425A$POSIXct<-as.character(Cllr.701425A$POSIXct)
Cllr.701425A<-Cllr.701425A[Cllr.701425A$POSIXct!="2019-07-22 10:00:00", ]

Cllr.701432A<-subset(final.t,final.t$CllrSrN=="701432A")
Cllr.701432A$POSIXct<-as.character(Cllr.701432A$POSIXct)
Cllr.701432A<-Cllr.701432A[Cllr.701432A$POSIXct!="2018-11-25 04:00:00", ]

Cllr.701437A<-subset(final.t,final.t$CllrSrN=="701437A")
Cllr.701437A$POSIXct<-as.character(Cllr.701437A$POSIXct)
Cllr.701437A<-Cllr.701437A[Cllr.701437A$POSIXct!="2018-10-13 11:00:00", ]

Cllr.701439A<-subset(final.t,final.t$CllrSrN=="701439A")
Cllr.701439A$POSIXct<-as.character(Cllr.701439A$POSIXct)
Cllr.701439A<-Cllr.701439A[Cllr.701439A$POSIXct!="2019-03-17 13:00:00", ]

Cllr.701445A<-subset(final.t,final.t$CllrSrN=="701445A")
Cllr.701445A$POSIXct<-as.character(Cllr.701445A$POSIXct)
Cllr.701445A<-Cllr.701445A[Cllr.701445A$POSIXct!="2018-07-03 14:00:00", ]

Cllr.701449A<-subset(final.t,final.t$CllrSrN=="701449A")
Cllr.701449A$POSIXct<-as.character(Cllr.701449A$POSIXct)
Cllr.701449A<-Cllr.701449A[Cllr.701449A$POSIXct!="2019-07-20 13:00:00", ]

Cllr.706084A<-subset(final.t,final.t$CllrSrN=="706084A")
Cllr.706084A$POSIXct<-as.character(Cllr.706084A$POSIXct)
Cllr.706084A<-Cllr.706084A[Cllr.706084A$POSIXct!="2019-07-20 13:00:00", ]

Cllr.706094A<-subset(final.t,final.t$CllrSrN=="706094A")
Cllr.706094A$POSIXct<-as.character(Cllr.706094A$POSIXct)
Cllr.706094A<-Cllr.706094A[Cllr.706094A$POSIXct!="2020-12-03 12:00:00", ]

Cllr.706110A<-subset(final.t,final.t$CllrSrN=="706110A")
Cllr.706110A$POSIXct<-as.character(Cllr.706110A$POSIXct)
Cllr.706110A<-Cllr.706110A[Cllr.706110A$POSIXct!="2020-08-23 07:00:00", ]

Cllr.707228A<-subset(final.t,final.t$CllrSrN=="707228A")
Cllr.707228A$POSIXct<-as.character(Cllr.707228A$POSIXct)
Cllr.707228A<-Cllr.707228A[Cllr.707228A$POSIXct!="2019-09-13 09:00:00", ]

Cllr.715848A<-subset(final.t,final.t$CllrSrN=="715848A")
Cllr.715848A$POSIXct<-as.character(Cllr.715848A$POSIXct)
Cllr.715848A<-Cllr.715848A[Cllr.715848A$POSIXct!="2021-01-22 13:00:00", ]

Cllr.715851A<-subset(final.t,final.t$CllrSrN=="715851A")
Cllr.715851A$POSIXct<-as.character(Cllr.715851A$POSIXct)
Cllr.715851A<-Cllr.715851A[Cllr.715851A$POSIXct!="2020-10-11 12:00:00", ]

final.t<-final.t[final.t$CllrSrN!="701421A",]
final.t<-final.t[final.t$CllrSrN!="701425A",]
final.t<-final.t[final.t$CllrSrN!="701432A",]
final.t<-final.t[final.t$CllrSrN!="701437A",]
final.t<-final.t[final.t$CllrSrN!="701439A",]
final.t<-final.t[final.t$CllrSrN!="701445A",]
final.t<-final.t[final.t$CllrSrN!="701449A",]
final.t<-final.t[final.t$CllrSrN!="706084A",]
final.t<-final.t[final.t$CllrSrN!="706094A",]
final.t<-final.t[final.t$CllrSrN!="706110A",]
final.t<-final.t[final.t$CllrSrN!="707228A",]
final.t<-final.t[final.t$CllrSrN!="715848A",]
final.t<-final.t[final.t$CllrSrN!="715851A",]
final.t <- final.t %>% filter(!(CllrSrN == "706083A" & POSIXct >= "2022-03-01 12:00:00")) #removing most recent deployment on 706083A for now...

final.t<-rbind(final.t,Cllr.701421A,Cllr.701425A,Cllr.701432A,Cllr.701437A,Cllr.701439A,Cllr.701445A,Cllr.701449A,
               Cllr.706084A,Cllr.706094A,Cllr.706110A,Cllr.707228A,Cllr.715848A,Cllr.715851A)
final.t<-final.t[order(final.t$CllrSrN,final.t$POSIXct),]
head(final.t)
tail(final.t) ; unique(final.t$CllrSrN)

rm(Cllr.701421A,Cllr.701425A,Cllr.701432A,Cllr.701437A,Cllr.701439A,Cllr.701445A,Cllr.701449A,
   Cllr.706084A,Cllr.706094A,Cllr.706110A,Cllr.707228A,Cllr.715848A,Cllr.715851A,gps,t,dir.tdc,files)

#Import Telonics data directly from Iridium collars ####
#Set working directory
dir.tdc <- "C:/Users/lwilde2/Documents/RDH Database/RedDesertToHobackMuleDeerResearchProject/GPS_Collar_Data/Telonics_AllGPSData_FromCollars/RawCSVFilesIridiumCollars"

#Loop through every CSV file, remove unnecessary rows, create CllrSrN and ESN column
read.csv.fx <- function(x){
  t <- read.csv(x,header = TRUE, skip = 22, stringsAsFactors = FALSE) # Note! This function breaks if data in csv starts sooner or later than line 23
  CllrSrN <- read.csv(x,header = FALSE, skip = 7, nrows = 1, as.is = TRUE)
  t$CllrSrN <- CllrSrN[,2]
  return(t)
}

#Bind all csv files together and convert into a single dataframe
files <- list.files(dir.tdc,full.names = TRUE)
files <- files[grepl(".csv",files)]
gps <- lapply(files,read.csv.fx)
gps <- bind_rows(gps)
head(gps)

#Remove GPS fixes that have failed or have zero data
gps<-gps[gps$GPS.Fix.Attempt!="Failed", ]
gps<-gps[gps$GPS.Fix.Attempt!="Unresolved QFP", ]
gps<-gps[gps$GPS.Fix.Attempt!="", ]
table(gps$GPS.Fix.Attempt)
gps<-subset(gps, gps$GPS.Horizontal.Dilution<20)
gps<-subset(gps, gps$GPS.Altitude>1500)

head(gps)

#Create new GPS dataframe
t<-gps
head(t)

#Split date and time column into two separate columns
t$tempdate<- sapply(t$Acquisition.Time, FUN = function(x){unlist(strsplit(x, " "))[1]})
t$temptime<- sapply(t$Acquisition.Time, FUN = function(x){unlist(strsplit(x, " "))[2]})

#Split date column into year, month, and day columns
t$Year<-substr(t$tempdate, 1, 4) 
t$Month<-substr(t$tempdate, 6, 7) 
t$Day<-substr(t$tempdate, 9, 10) 

#Combine month, day, and year column into a single date column with correct format
t$date<- paste(t$Year, t$Month, t$Day,  sep="-")
head(t)

#Clean up all date and time columns
t$Hour <- format(strptime(t$temptime,"%H:%M:%S"),'%H')
t$Minute <- sprintf(paste("%02d"), 00)
t$Sec <- sprintf(paste("%02d"), 00)
t$Time <- paste(t$Hour, t$Minute, t$Sec, sep=":")
t$Date <- paste(t$Year, t$Month, t$Day, sep="-")
t$dt <- paste(as.character(t$Date), t$Time, sep=" ")
t$POSIXct <- as.POSIXct(strptime(t$dt, format="%Y-%m-%d %H:%M:%S"), tz="UTC")
head(t)
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
head(t); tail(t); unique(t$CllrSrN)
names(t)

#Remove unneccessary columns, rename column headers, organize columns
head(t[c(5:6,10,14,16,23,26:28,33:34,36:37)])
t <- t[c(5:6,10,14,16,23,26:28,33:34,36:37)]
colnames(t)<-c("Lat","Long","Altitude","HDOP","NumSats","CllrSrN","Year","Month","Day","Time","Date","POSIXct","DayOfYear")
t<-t[order(t$CllrSrN,t$POSIXct),]
final.i <- t[c("CllrSrN","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long","Altitude","HDOP","NumSats")]
head(final.i)

#Omit POSIXct values with null values
final.i<-final.i[!is.na(final.i[,2]),] 

#Remove any rows containing zero values for Latitude and Longitude
final.i<-final.i[final.i$Long!=0 & final.i$Lat!=0, ]

#Combine data from globalstar collars and iridium collars into one dataframe
all<-rbind(final.t,final.i)

head(all)
str(all)
unique(all$CllrSrN)


#tmp <- all %>% filter(CllrSrN == "706083A") # & POSIXct >= "2022-03-10 12:00:00")
#View(tmp)

#Write csv file
write.csv(all, paste("C:/Users/lwilde2/Documents/RDH Database/RedDesertToHobackMuleDeerResearchProject/GPS_Collar_Data/Telonics_AllGPSData_FromCollars/AllTelonicsRawDataFromCollars_",UpdateName,".csv",sep=""), row.names = FALSE)
# 
# #Convert into spatial dataframe
# UTM_E = all[,c('Long')]
# UTM_N = all[,c('Lat')]
# 
# library(sp)
# 
# UTM_E <- SpatialPointsDataFrame(cbind(UTM_E,UTM_N), all, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# 
# #Reproject shapefile into desired coordinate system
# finalshape <- spTransform(UTM_E, CRS("+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
# 
# finalshape_sf <- st_as_sf(finalshape ) %>% mutate(POSIXct = as.character(POSIXct))
# 
# #Designate the name of the shapefile
# fileSuffix=UpdateName
# outname<- paste("AllTelonicsRawDataFromCollars",sep="_", fileSuffix,"2")
# 
# #Export the shapefile 
# st_write(finalshape_sf, "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/GPS_Collar_Data/Telonics_AllGPSData_FromCollars/RawShapefiles", outname, driver="ESRI Shapefile",overwrite=TRUE)
# 
# str(all)
