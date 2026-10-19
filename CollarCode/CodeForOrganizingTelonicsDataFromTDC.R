#Code for downloading Telonics collar data
#Written by Ben Robb and Anna Ortega, Wyoming Cooperative Fish and Wildlife Research Unit
#March 31, 2018

#Load required libraries
library(dplyr)
library(rgdal)
library(lubridate)
library(stringr)

#Set working directory
dir.tdc <- "C:/Users/aortega5/Desktop/RD2H_GPSCollarData/RD2H_Telonics_Data/IndividualRawGlobalstarDataFromTDC"

#Loop through every CSV file, remove unncessary rows, create CllrSrN and ESN column
read.csv.fx <- function(x){
  t <- read.csv(x,header = TRUE, skip = 22, stringsAsFactors = FALSE) # Note! This function breaks if data in csv starts sooner or later than line 23
  CllrSrN <- read.csv(x,header = FALSE, skip = 6, nrows = 1, as.is = TRUE)
  ESN <- read.csv(x,header = FALSE, skip = 8, nrows = 1, as.is = TRUE)
  t$CllrSrN <- CllrSrN[,2]
  t$ESN <- ESN[,2]
  return(t)
}

#Bind all csv files together and convert into a single dataframe
files <- list.files(dir.tdc,full.names = TRUE)
files <- files[grepl(".csv",files)]
gps <- lapply(files,read.csv.fx)
gps <- bind_rows(gps)

#Extract GPS fixes that have been successful
gps<-subset(gps,gps$GPS.Fix.Attempt=="Succeeded")

#Ensure that there are the correct number of unique Telonics collars (n = 30)
unique.cllr<-unique(gps$CllrSrN)

#Create new GPS dataframe
a<-gps

#Split date and time column into two separate columns
a$tempdate<- sapply(a$Acquisition.Time, FUN = function(x){unlist(strsplit(x, " "))[1]})
a$temptime<- sapply(a$Acquisition.Time, FUN = function(x){unlist(strsplit(x, " "))[2]})

#Split date column into year, month, and day columns
a$Year<-substr(a$tempdate, 1, 4) 
a$Month<-substr(a$tempdate, 6, 7) 
a$Day<-substr(a$tempdate, 9, 10) 

#Combine month, day, and year column into a single date column with correct format
a$date<- paste(a$Year, a$Month, a$Day,  sep="-")

#Clean up all date and time columns
a$Hour <- format(strptime(a$temptime,"%H:%M:%S"),'%H')
a$Minute <- sprintf(paste("%02d"), 00)
a$Sec <- sprintf(paste("%02d"), 00)
a$Time <- paste(a$Hour, a$Minute, a$Sec, sep=":")
a$Date <- paste(a$Year, a$Month, a$Day, sep="-")
a$dt <- paste(as.character(a$Date), a$Time, sep=" ")
a$POSIXct <- as.POSIXct(strptime(a$dt, format="%Y-%m-%d %H:%M:%S"), tz="UTC")
attr(a$POSIXct, "tzone") <- "GMT"
head(a)
attr(a$POSIXct, "tzone") <- "MST"
a$DayOfYear <- yday(a$POSIXct)

#Reconvert hour and time data to match MST time zone
a$Hour <- hour(a$POSIXct)
a$Time <- paste(a$Hour, a$Minute, a$Sec, sep=":")
names(a)

#Remove unneccessary columns, rename column headers, organize columns
a <- a[c(5:6,18,29:31,36:37,39:40)]
colnames(a)<-c("Lat","Long","CllrSrN","Year","Month","Day","Time","Date","POSIXct","DayOfYear")
final.a <- a[c("CllrSrN","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long")]
a<-a[order(a$CllrSrN,a$POSIXct),]

#Omit POSIXct values with null values
final.a<-final.a[!is.na(final.a[,2]),] 

#Remove any rows containing zero values for Latitude and Longitude
final.a<-final.a[final.a$Long!=0 & final.a$Lat!=0, ]
head(final.a)

#Create spatial dataframe
UTM_E = final.a[,c('Long')]
UTM_N = final.a[,c('Lat')]

UTM_E <- SpatialPointsDataFrame(cbind(UTM_E,UTM_N), final.a, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#Reproject shapefile into desired coordinate system
finalshape <- spTransform(UTM_E, CRS("+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#Designate the name of the shapefile
outname<- paste("RD2H",sep="_","TelonicsData")
head(finalshape)
#Export the shapefile 
writeOGR(finalshape, "C:/Users/aortega5/Desktop/RD2H_GPSCollarData/RD2H_Telonics_Data", outname, driver="ESRI Shapefile",overwrite=TRUE)
