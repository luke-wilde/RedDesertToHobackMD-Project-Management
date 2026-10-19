#Code for downloading new collar data and appending it to the existing GPS database
#Written by Anna Ortega, Wyoming Cooperative Fish and Wildlife Research Unit, University of Wyoming
#September 29, 2021


#Load required libraries
library(chron)
library(date)
library(dplyr)
library(lubridate)
library(rgdal)
library(stringr)
library(sp)
library(sf)

#Automatically Download current collars
#source("D:/RedDesertToHobackMuleDeerResearchProject/RCode/Automatic Download/CodeToAutoDownloadAndPrepareTelonics.R")
#source("D:/RedDesertToHobackMuleDeerResearchProject/RCode/Automatic Download/CodeToAutoDownloadAndPrepareVectronics.R")
#source("D:/RedDesertToHobackMuleDeerResearchProject/RCode/Automatic Download/CodeToAutoDownloadAndPrepareLotek.R")

#Import LOTEK data that was downloaded from LOTEK Webservice ####
#Set working directory and import list of all csv files for LOTEK collars
setwd("D:/RD2H_GPSCollarData/RD2H_LOTEK_Data/")
e<-read.csv("AllLOTEKFromWebService.csv", header=TRUE, sep=",")
# --------------------------> # WILL CAUSE ERROR IF YOU DOWNLOAD MANUALLY# <---------------------------------
e <- e[,-1] # only needed if you keep with the automated
# -------------------------->  <---------------------------------

names(e)

#Remove GPS fixes with massive errors
e<-subset(e, e$DOP<20)
e<-subset(e, e$Altitude>1500)

#Reformat date and time columns
#Make sure date and time columns are in MST and NOT in GMT
e$POSIXct <- as.POSIXct(as.Date(e$Date...Time..GMT. ,origin = as.POSIXct("1899-12-30", tz = 'GMT')), tz = 'UTC') 
attr(e$POSIXct, "tzone") <- "GMT"
attr(e$POSIXct, "tzone") <- "MST"
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
e$POSIXct <- as.POSIXct(strptime(e$dt, format="%Y-%m-%d %H:%M:%S"), tz="MST")
e$DayOfYear <- yday(e$POSIXct)
head(e)
names(e)

#Remove unnecessary columns, rename column headers, organize columns
e <- e[c(1,4:6,8,12:15,19:20,22)]
colnames(e)<-c("CllrSrN","Lat","Long","Altitude","HDOP","POSIXct","Month","Day","Year","Time","Date","DayOfYear")
e$NumSats<-"N/A"
e<-e[order(e$CllrSrN,e$POSIXct),]
final.e <- e[c("CllrSrN","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long","Altitude","HDOP","NumSats")]
head(final.e)

#Omit POSIXct values with null values
final.e<-final.e[!is.na(final.e[,2]),] 

#Remove any rows containing zero values for Latitude and Longitude
final.e<-final.e[final.e$Long!=0 & final.e$Lat!=0, ]
head(final.e)

#Clean up working environment
rm(e)

#Import Telonics Globalstar data that was downloaded directly from TDC ####
#Set working directory
dir.tdc <- "D:/RD2H_GPSCollarData/RD2H_Telonics_Data/IndividualRawGlobalstarDataFromTDC"

#Loop through every CSV file, remove unncessary rows, create CllrSrN and ESN column
read.csv.fx <- function(x){
  t <- read.csv(x,header = TRUE, skip = 22, stringsAsFactors = FALSE) # Note! This function breaks if data in csv starts sooner or later than line 23
  CllrSrN <- read.csv(x,header = FALSE, skip = 6, nrows = 1, as.is = TRUE)
  t$CllrSrN <- CllrSrN[,2]
  return(t)
}

#Bind all csv files together and convert into a single dataframe
files <- list.files(dir.tdc,full.names = TRUE)
files <- files[grepl(".csv",files)]
gps <- lapply(files,read.csv.fx)
gps <- dplyr::bind_rows(gps)

#Extract GPS fixes that have been successful
gps<-gps[gps$GPS.Fix.Attempt!="Failed", ]
gps<-gps[gps$GPS.Fix.Attempt!="Unresolved QFP", ]
gps<-gps[gps$GPS.Fix.Attempt!="", ]
table(gps$GPS.Fix.Attempt) #Ensure that all GPS fixes have been successful

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

#Remove unneccessary columns, rename column headers, organize columns
t <- t[c(5:6,10,18,21:23,28:29,31:32)]
head(t)
colnames(t)<-c("Lat","Long","HDOP","CllrSrN","Year","Month","Day","Time","Date","POSIXct","DayOfYear")
t$Altitude<-"N/A"
t$NumSats<-"N/A"
t<-t[order(t$CllrSrN,t$POSIXct),]
final.t <- t[c("CllrSrN","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long","Altitude","HDOP","NumSats")]
head(final.t)

#Note, I did not remove any HDOP errors, because unlike LOTEK...I can't quite figure out what values of HDOP are truly errors

#Omit POSIXct values with null values
final.t<-final.t[!is.na(final.t[,2]),] 

#Remove any rows containing zero values for Latitude and Longitude
final.t<-final.t[final.t$Long!=0 & final.t$Lat!=0, ]
head(final.t)

#Clean up working environment
rm(gps,t,dir.tdc,files)

#Import Telonics Iridium data that was downloaded directly from TDC ####
#Set working directory
dir.tdc.i <- "D:/RD2H_GPSCollarData/RD2H_Telonics_Data/IndividualRawIridiumDataFromTDC"

#Loop through every CSV file, remove unnecessary rows, create CllrSrN and ESN column
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

#Extract GPS fixes that have been successful
gps.i<-gps.i[gps.i$GPS.Fix.Attempt!="Failed", ]
gps.i<-gps.i[gps.i$GPS.Fix.Attempt!="Unresolved QFP", ]
gps.i<-gps.i[gps.i$GPS.Fix.Attempt!="", ]
table(gps.i$GPS.Fix.Attempt)#Ensure that all GPS fixes have been successful

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
head(m)
attr(m$POSIXct, "tzone") <- "GMT"
attr(m$POSIXct, "tzone") <- "MST"
head(m)

#Reconvert month, day, year, hour, minute, second columns to match MST time zone
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
m$POSIXct <- as.POSIXct(strptime(m$dt, format="%Y-%m-%d %H:%M:%S"), tz="MST")
m$DayOfYear <- yday(m$POSIXct)
head(m)
names(m)

#Remove unneccessary columns, rename column headers, organize columns
m <- m[c(8:9,13,24,27:29,34:35,37:38)]
colnames(m)<-c("Lat","Long","HDOP","CllrSrN","Year","Month","Day","Time","Date","POSIXct","DayOfYear")
m$Altitude<-"N/A" #No altitude column but need to add to be consistent with other collar formats
m$NumSats<-"N/A" #No NumSats column but need to add to be consistent with other collar formats
m<-m[order(m$CllrSrN,m$POSIXct),]
final.m <- m[c("CllrSrN","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long","Altitude","HDOP","NumSats")]

#Note, I did not remove any HDOP errors, because unlike LOTEK...I can't quite figure out what values of HDOP are truly errors

#Omit POSIXct values with null values
final.m<-final.m[!is.na(final.m[,2]),] 

#Remove any rows containing zero values for Latitude and Longitude
final.m<-final.m[final.m$Long!=0 & final.m$Lat!=0, ]
head(final.m)

#Clean up working envrionment
rm(gps.i,m,dir.tdc.i,files.i)

#Import Vectronics Iridium data that was downloaded directly from GPS Plus X ####
dir.tdc.v <- "D:/RD2H_GPSCollarData/RD2H_Vectronics_Data/IndividualRawIridiumDataFromGPSPlusX"

#Loop through every CSV file, remove unncessary rows, create CllrSrN and ESN column
read.csv.fx <- function(x){
  t <- read.csv(x,header = TRUE, stringsAsFactors = FALSE) #No lines to skip
  CllrSrN <- read.csv(x,header = FALSE, skip = 1, nrows = 1, as.is = TRUE)
  t$CllrSrN <- CllrSrN[,2]
  return(t)
}

#Bind all csv files together and convert into a single dataframe
files <- list.files(dir.tdc.v,full.names = TRUE)
files <- files[grepl(".csv",files)]
gps <- lapply(files,read.csv.fx)
gps <- bind_rows(gps)

head(gps)

#Create new GPS dataframe
v<-gps

#Remove unnecessary columns
# --------------------------> # WILL CAUSE ERROR IF YOU DOWNLOAD MANUALLY# <---------------------------------
v<-v[c(2:4,39:42)]
# --------------------------> # WILL CAUSE ERROR IF YOU DOWNLOAD MANUALLY# <---------------------------------

colnames(v)<-c("CllrSrN","Date","Time","Lat","Long","Altitude","HDOP")

#Split date column into year, month, and day columns
v$Year <- str_split_fixed(v$Date, "/", 3)[,3]
v$Month <- str_split_fixed(v$Date, "/", 3)[,1]
v$Day <- str_split_fixed(v$Date, "/", 3)[,2]
head(v)

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
attr(v$POSIXct, "tzone") <- "MST"

#Reconvert month, day, year, hour, minute, second columns to match MST time zone
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
v$POSIXct <- as.POSIXct(strptime(v$dt, format="%Y-%m-%d %H:%M:%S"), tz="MST")
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

#Clean up working environment
rm(gps,v,dir.tdc.v,files)

#Combine all collar data into one dataframe
all<-rbind(final.e,final.m,final.t,final.v)
names(all)

#Import existing GPS dataframe
load("D:/RD2H_GPSCollarData/RD2H_AllData.RData")

range(finalshape$POSIXct)

#Convert data to data frame (make not spatially aware)
data<-as.data.frame(finalshape)
rm(finalshape)

#Loop through existing dataset and extract last known location for all deer
#Identify different IDs and create empty output dataframe
IDlist <- unique(data$AID)
shape <- data.frame()

#Grabs the last known location of each deer
for (i in 1:length(IDlist)){
  temp<- subset(data, AID==IDlist[i])
  c <- tail(temp, n=1)
  shape<- rbind(shape, c)
}
head(shape)

#Clean working environment
rm(c,final.e,final.m,final.t,final.v,temp,i,IDlist)

#Import master ID list ####
setwd("~/")
getwd()
setwd("D:/RD2H_GPSCollarData/")
DeerID.Data <- read.csv("RD2H_AllDeerIDs_06OCTOBER2021.csv", header=TRUE, sep=",",na.strings=c(" ","NA"))
names(DeerID.Data)
head(DeerID.Data)

#Make sure StopoDateTime is in correct POSIXct format
DeerID.Data$StopDateTime<-as.POSIXct(DeerID.Data$StopDateTime,format="%m/%d/%Y %H:%M")
head(DeerID.Data)

#Subset active deer only (subset individuals with no stop time...collars still deployed)
d<-DeerID.Data[c(1:2,5:6,10)]
d<-d[is.na(d$StopDateTime),]
d<-d[order(d$AID),]
head(d)

#Merge all GPS collar data with Deer ID csv by CllrSrN                
#Only want to subset last locations for those individuals that are alive and actively in the dataset
lastloc <- merge(shape, d, by=c("AID","CllrSrN")) 
head(lastloc)

#Organize dataframe by AID
lastloc<-lastloc[order(lastloc$AID),]
head(lastloc)

#Reduce dataset to 'AID' and POSIXct'
lastloc<-lastloc[c(1,5)]
head(lastloc)
range(lastloc$POSIXct)

#Merge new GPS collar data with Deer ID csv by CllrSrN 
#Only want to subset data for those individuals that are alive and actively in the dataset
all.new <- merge(d, all, by=c("CllrSrN")) 

#Ensure that there are the correct number of AIDs in all.new
u<-unique(all.new$CllrSrN) #Should be the same number of rows as in 'd'

#Remove unnecessary columns from the new GPS collar data and existing dataframe so that the datasets match
all.new<-all.new[c(1:4,6:14)]
all.new <- all.new[c("Mgtry","AID","CllrSrN","Frequency","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long")]
data<-data[c(1:13)]

head(all.new)
head(data)

#Loop through each unique deer ID that is alive and with an active collar
#and stitch new data with data in existing dataset

#Must order the new GPS collar data and the last locations by AID !!!
#Otherwise, for loop won't run correctly
all.new<-all.new[order(all.new$AID,all.new$POSIXct),]
lastloc<-lastloc[order(lastloc$AID),]

all.new$POSIXct<-as.character(all.new$POSIXct)
lastloc$POSIXct<-as.character(lastloc$POSIXct)

l1<-unique(all.new$AID)
l2<-unique(lastloc$AID)

# #Export id lists if you want to 100% sure that the ID lists line up
# write.csv(l1,"C:/Users/aortega5/Desktop/RD2H_GPSCollarData/IDList1.csv")
# write.csv(l2,"C:/Users/aortega5/Desktop/RD2H_GPSCollarData/IDList2.csv")

#Loop through new GPS data and only subset data that is newer than the date of the last location
#Identify different IDs and create empty output dataframe
ID.list<-unique(all.new$AID)

df.1<-data
full.df.1<-df.1[0, ]

for(i in 1:length(ID.list)){
  a.1<-all.new[all.new$AID==ID.list[i],]
  b.1<-lastloc[lastloc$AID==ID.list[i],]
  d.1 <- subset(a.1,a.1$POSIXct > b.1$POSIXct)
  d.1<-d.1[order(d.1$AID,d.1$POSIXct),]
  full.df.1<-rbind(full.df.1,d.1)
}

l3<-unique(full.df.1$AID) #Might not have the same number of unique individuals as before if data hasn't uploaded to all individuals

#Append new data with existing dataframe
final.gps<-rbind(data,full.df.1)

#Ensure that there are no duplicated values
#If number of rows in dataframe doesn't change, then we stitched together the collar data correctly

#Convert AID to character format
#Convert date to POSIXct format
final.gps$AID<-as.character(final.gps$AID)
final.gps$POSIXct<-as.POSIXct(final.gps$POSIXct,format="%Y-%m-%d %H:%M:%S", tz="MST")

str(final.gps)
final.gps<-final.gps[order(final.gps$AID,final.gps$POSIXct),]

#The only was this duplicated function works is by readjusting the order of the columns
#The other way to remove duplicated values takes way too long
final.gps <- final.gps[c("Mgtry","CllrSrN","Frequency","AID","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long")]
head(final.gps)
final.gps<-final.gps[!duplicated(final.gps[4:5]),]

#Re-order database
final.gps <- final.gps[c("AID","Mgtry","CllrSrN","Frequency","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long")]
head(final.gps)

#Now, we have to append new data from recent mortalities, collar failures, or collars that we removed during captures
#Up until now, the code only works for those individuals that are alive and with active GPS collars
#But what about deer that died since the last time you append the data? Or deer that we replace their collar?
#We have to deal with them...otherwise the data right before they died may be cut too short

#First clean up the working environment
rm(a.1,b.1,d,d.1,df.1,full.df.1)

#Determine the last time you appended data
t<-as.data.frame(table(lastloc$POSIXct))
colnames(t)<-c("date","Freq")
t<-t[order(t$Freq),]
last.date<-tail(t,n=1)
head(last.date)

#Now find any mortalities that occurred after the last time you appended the data
str(DeerID.Data)
str(last.date)

#Have to convert date to character format when subsetting data based on dates
DeerID.Data$StopDateTime<-as.character(DeerID.Data$StopDateTime)
last.date$date<-as.character(last.date$date)
mort<-subset(DeerID.Data,DeerID.Data$StopDateTime>last.date$date) 
head(mort)

#Merge the new GPS data with the mortality data frame
#In other words, subset only the new GPS data for those deer that have died since you last appended the data
mort.new <- merge(mort, all, by=c("CllrSrN")) 
mort.new<-mort.new[order(mort.new$CllrSrN,mort.new$POSIXct),]

str(shape)
str(mort.new)

#Now figure out last date that the data for these particular deer were appended
shape$AID<-as.character(shape$AID)
mort.new$AID<-as.character(mort.new$AID)

mort.last <- shape[shape$AID %in% mort.new$AID, ]
head(mort.last)

#Identify unique IDs and create empty output dataframe for data in which StopDateTime does not contain NULL values
mort.ID<- unique(mort.new$AID)
df.2<- data.frame()
df.2<-as.data.frame(mort.new)
full.df.2<-df.2[0, ]

str(mort.new)
str(mort.last)

mort.new$POSIXct<-as.character(mort.new$POSIXct)

#Create for loop that creates unique Deer IDs with associated CllrSrN based on range of dates
for (i in 1:length (mort.ID)){
  temp.1<-mort.new[mort.new$AID==mort.ID[i],]
  temp.2<-mort.last[mort.last$AID==mort.ID[i],]
  head(temp.1)
  sub.1<-subset(temp.1, temp.1$POSIXct > temp.2$POSIXct & temp.1$POSIXct <= temp.1$StopDateTime)
  head(sub.1)  
  tail(sub.1)
  full.df.2<-rbind(full.df.2,sub.1)
}

#Re-convert date to POSIXct format
str(full.df.2)
full.df.2$POSIXct<-as.POSIXct(full.df.2$POSIXct,format="%Y-%m-%d %H:%M:%S", tz="MST")

#Fix the dataframe so it matches the main dataset
names(full.df.2)
full.df.2<-full.df.2[c(1:3,6,15:23)]
full.df.2 <- full.df.2[c("AID","Mgtry","CllrSrN","Frequency","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long")]

head(final.gps)
head(full.df.2)

#Merge new data for deer that died since the last time you appended data with the exisitng database
f<-rbind(final.gps,full.df.2)

#The only was this duplicated function works is by readjusting the order of the columns
#The other way to remove duplicated values takes way too long
f <- f[c("Mgtry","CllrSrN","Frequency","AID","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long")]
names(f)
f<-f[!duplicated(f[4:5]),]

#Re-order database
f <- f[c("AID","Mgtry","CllrSrN","Frequency","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long")]

#Re-order database by AID and then by POSIXct
f<-f[order(f$AID,f$POSIXct),]

#Subset an individual if you want to make sure the code worked
temp<-subset(f,f$AID=="108")
head(temp)
tail(temp)

#Clean working environment
rm(all,all.new,data,DeerID.Data,df.2,final.gps,full.df.2,last.date,lastloc,mort,mort.last,mort.new,shape,sub.1,t,temp,
   temp.1,temp.2,i,ID.list,l1,l2,l3,mort.ID,u)

#Export new appended and updated database
UTM_E = f[,c('Long')]
UTM_N = f[,c('Lat')]

UTM_E <- SpatialPointsDataFrame(cbind(UTM_E,UTM_N), f, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#Reproject shapefile into desired coordinate system
finalshape <- spTransform(UTM_E, CRS("+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#Designate the name of the shapefile
fileSuffix="AllData"
outname<- paste("RD2H",sep="_", fileSuffix)

# #change to sf
# finalshape_sf <- st_as_sf(finalshape)
# 
# #Export the shapefile 
# st_write(finalshape_sf, "D:/RD2H_GPSCollarData/RD2H_AllData.shp",append=TRUE)

#Export data into csv file for future data analyses and backup
write.csv(finalshape,"D:/RD2H_GPSCollarData/RD2H_AllData.csv")

#Save data as a .RData file
save(finalshape,file="D:/RD2H_GPSCollarData/RD2H_AllData.RData")

