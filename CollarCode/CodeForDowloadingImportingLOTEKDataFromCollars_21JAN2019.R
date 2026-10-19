#Downloading and organizing all LOTEK data that has been downloaded directly from the collars
#Code written by Anna Ortega, Wyoming Cooperative Fish and Wildlife Research Unit
#January 21, 2019

# DONT FORGET TO ERASE THE FIRST COLUMN #

#Load required libraries
library(lubridate)
library(dplyr)
library(rgdal)

#Import LOTEK data that was downloaded directly from LOTEK collars
#Set working directory and Obtain a list of all csv files for LOTEK collars
dir.lotek<-setwd("D:/RedDesertToHobackMuleDeerResearchProject/GPS_Collar_Data/LOTEK_AllGPSData_FromCollars/RawCSVFiles")

#Loop through every CSV file, remove unncessary rows, create CllrSrN and ESN column
read.csv.fx <- function(x){
  gps <- read.csv(x,header = TRUE, stringsAsFactors = FALSE) # Note! This function breaks if data in csv starts sooner or later than line 23
  return(gps)
}

#Bind all csv files together and convert into a single dataframe
files <- list.files(dir.lotek,full.names = TRUE)
files <- files[grepl(".csv",files)]
a <- lapply(files,read.csv.fx)
a <- bind_rows(a)

#Remove GPS fixes that have failed or have zero data
a<-a[a$Satellites!="0", ]
a<-subset(a, a$DOP<20)
a<-subset(a, a$Altitude>1500)

#Reformat date and time columns
#Make sure date and time columns are in MST and NOT in GMT
a$Hour <- sprintf(paste("%02d"), as.numeric(a$Hour))
a$Minute <- sprintf(paste("%02d"), 00)
a$Sec <- sprintf(paste("%02d"), 00)
a$Time <- paste(a$Hour, a$Minute, a$Sec, sep=":")
a$Date <- paste(a$Year, a$month, a$day, sep="-")
a$dt <- paste(as.character(a$Date), a$Time, sep=" ")
a$POSIXct <- as.POSIXct(strptime(a$dt, format="%Y-%m-%d %H:%M:%S"), tz="GMT")
attr(a$POSIXct, "tzone") <- "MST"
head(a)

#Convert date and time columns to MST time zone NOT in GMT time zone
a$month <- month(a$POSIXct)
a$day <- day(a$POSIXct)
a$Year <- year(a$POSIXct)
a$Hour <- hour(a$POSIXct)
a$Minute <- sprintf(paste("%02d"), 00)
a$Sec <- sprintf(paste("%02d"), 00)
a$Time <- paste(a$Hour, a$Minute, a$Sec, sep=":")
a$Date <- paste(a$Year, a$month, a$day, sep="-")
a$DayOfYear <- yday(a$POSIXct)
names(a)
head(a)

#Remove unneccessary columns, rename column headers, organize columns
a <- a[c(2:6,8:10,13:15,19:20)]
colnames(a)<-c("Date","Time","Month","Day","Year","Lat","Long","Altitude","HDOP","NumSats","CllrSrN","POSIXct","DayOfYear")
a<-a[order(a$CllrSrN,a$POSIXct),]
final.a <- a[c("CllrSrN","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long","Altitude","HDOP","NumSats")]

#Remove any rows containing zero values for Latitude and Longitude
final.a<-final.a[final.a$Long!=0 & final.a$Lat!=0, ]

#Remove rows with probelmatic data and rbind to final dataframe
Cllr.100138<-subset(final.a,final.a$CllrSrN=="100138")
Cllr.100138$POSIXct<-as.character(Cllr.100138$POSIXct)
Cllr.100138<-Cllr.100138[Cllr.100138$POSIXct!="2018-09-16 19:00:00", ]

final.a<-final.a[final.a$CllrSrN!="100138",]

final.a<-rbind(final.a,Cllr.100138)
final.a<-final.a[order(final.a$CllrSrN,final.a$POSIXct),]
head(final.a)

#Write txt file
write.csv(final.a, "D:/RedDesertToHobackMuleDeerResearchProject/GPS_Collar_Data/LOTEK_AllGPSData_FromCollars/AllLOTEKRawDataFromCollars_14OCTOBER2021.csv")

#Convert into spatial dataframe
UTM_E = final.a[,c('Long')]
UTM_N = final.a[,c('Lat')]

UTM_E <- SpatialPointsDataFrame(cbind(UTM_E,UTM_N), final.a, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#Reproject shapefile into desired coordinate system
finalshape <- spTransform(UTM_E, CRS("+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#Designate the name of the shapefile
fileSuffix="14OCTOBER2021"
outname<- paste("AllLOTEKRawDataFromCollars",sep="_", fileSuffix)

#Export the shapefile 
writeOGR(finalshape, "D:/RedDesertToHobackMuleDeerResearchProject/GPS_Collar_Data/LOTEK_AllGPSData_FromCollars/Shapefiles", outname, driver="ESRI Shapefile",overwrite=TRUE)

