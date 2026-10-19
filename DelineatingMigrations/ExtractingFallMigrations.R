#Code to subset fall migration data based on migration start and end times
#Written by Anna Ortega, Wyoming Cooperative Fish and Wildlife Research Unit
#July 17, 2020

#Load required libraries
library(rgdal)

#Set working directory
setwd("D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/GPS_Collar_Data/Archived_RD2H_GPSCollarData/RD2H_GPSCollarData_29SEPT2021")

#Import GPS data
d<-read.csv("RD2H_AllData_29SEPT2021.csv", header=TRUE, sep=",")

#Merge migration data based on unique AID and range of dates
#Set working directory and import csv file with migration timing data
setwd("D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/Migrations/2020")
Mig.Data <- read.csv("2020SpringFallMigrationTiming.csv", header=TRUE, sep=",")
head(Mig.Data)

#Format date and time columns
Mig.Data$FallMS <- as.POSIXct(Mig.Data$FallMS, format="%m/%d/%Y")
Mig.Data$FallME <- as.POSIXct(Mig.Data$FallME,format="%m/%d/%Y")

Mig.Data$FalldtS <- paste(as.character(Mig.Data$FallMS), Mig.Data$FallMSTime, sep=" ")
Mig.Data$FallMS <- as.POSIXct(strptime(Mig.Data$FalldtS, format="%Y-%m-%d %H:%M:%S"), tz="MST")

Mig.Data$FalldtE <- paste(as.character(Mig.Data$FallME), Mig.Data$FallMETime, sep=" ")
Mig.Data$FallME <- as.POSIXct(strptime(Mig.Data$FalldtE, format="%Y-%m-%d %H:%M:%S"), tz="MST")
names(Mig.Data)
head(Mig.Data)

#Remove unncessary columns
Mig.Data<- Mig.Data[c(1:3,10,13)]

#Sort unique collar serial numbers; make sure the serial numbers look correct
sort(unique(d$AID))
sort(unique(Mig.Data$AID))

#Merge all GPS collar data with migration timing data by AID                
mydata <- merge(d, Mig.Data, by=c("AID"), all=TRUE)
names(mydata)

#Omit any deer IDs with unknown migratory strategies and any null values in the migration timing data
mydata<-mydata[mydata$Mgtry!="N/A", ]
mydata<-mydata[!is.na(mydata[,20]),] 
mydata<-mydata[!is.na(mydata[,21]),] 

#Organize dataframe by AID and POSIXct
mydata<-mydata[order(mydata$AID, mydata$POSIXct),]
head(mydata)

#Convert date and time columns into character data type
mydata$FallMS<-as.character(mydata$FallMS)
mydata$FallME<-as.character(mydata$FallME)
mydata$POSIXct<-as.character(mydata$POSIXct)

#Identify unique IDs and create empty output dataframe 
Deer.ID <- unique(mydata$AID)
df<- data.frame()

#Create for loop that subsets migration data based on migration timing and unique AID
for (i in 1:length (Deer.ID)){
  temp.2<-subset(mydata, AID==Deer.ID[i])
  sub.1<-subset(temp.2, temp.2$POSIXct >= temp.2$FallMS & temp.2$POSIXct<=temp.2$FallME)
  df<-rbind(df,sub.1)
}

#Create new dataframe
clean.Data<-as.data.frame(df)

#Subset an individual to ensure that data looks correct
# temp<-subset(df,df$AID=="108")
# head(temp)
# tail(temp)
# table(df$AID)

#Ensure that frequency field as 3 decimal places
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
#clean.Data$Frequency<-specify_decimal(clean.Data$Frequency, 3)
names(clean.Data)

#Remove unnessary columns and reorganize columns
clean.Data<-clean.Data[c(1,3:14)]
clean.Data<-clean.Data[order(clean.Data$AID, clean.Data$POSIXct),]
names(clean.Data)
clean.Data <- clean.Data[c("AID","Mgtry","CllrSrN","Frequency","POSIXct","Month","Day", "Year.x", "DayOfYear","Date","Time","Lat","Long")]
colnames(clean.Data)<-c("AID","Mgtry","CllrSrN","Frequency","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long")
head(clean.Data)

#Export data into shapefile for future data analyses
#Create spatial dataframe
UTM_E = clean.Data[,c('Long')]
UTM_N = clean.Data[,c('Lat')]

UTM_E <- SpatialPointsDataFrame(cbind(UTM_E,UTM_N), clean.Data, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#Reproject shapefile into desired coordinate system
finalshape <- spTransform(UTM_E, CRS("+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#Designate the name of the shapefile
outname<- paste("RD2H",sep="_","AutumnMig2020")

#Export the shapefile 
writeOGR(finalshape, "D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/Migrations/2020/AutumnMigration", outname, driver="ESRI Shapefile",overwrite=TRUE)

#Import shapefile and separate into different shapefiles based on unique Deer AID
data<-readOGR("D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/Migrations/2020/AutumnMigration","RD2H_AutumnMig2020")
outPath = ("D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/Migrations/2020/AutumnMigration/IndividualShapefiles")
fileSuffix='2020'

names(data)
unique <- unique(data@data$AID)
head(unique)

#Loop through and create separate shapefiles for each unique Deer AID
for (i in 1:length(unique)) {
  shape <- data[data$AID == unique[i], ] 
  outname<- paste("RD2H",unique[[i]],"AutumnMig",sep="_",fileSuffix)
  writeOGR(shape,outPath,as.character(outname), driver='ESRI Shapefile')
  
}

