#Code to subset spring migration data based on migration start and end times
#Written by Anna Ortega, Wyoming Cooperative Fish and Wildlife Research Unit
#January 31, 2011

#Load required libraries
library(rgdal)
library(dplyr)

#Set working directory
setwd("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData")

#Import GPS data
d<-read.csv("RD2H_AllData.csv", header=TRUE, sep=",")

#Merge migration data based on unique AID and range of dates
#Set working directory and import csv file with migration timing data
setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations")
Mig.Data <- read.csv("AllSpringFallMigrationTiming_2014_thru_2021.csv", header=TRUE, sep=",")

#Format date and time columns
Mig.Data$FallMS <- as.POSIXct(Mig.Data$FallMS, format="%m/%d/%Y")
Mig.Data$SpringME <- as.POSIXct(Mig.Data$SpringME,format="%m/%d/%Y")

Mig.Data$FalldtS <- paste(as.character(Mig.Data$FallMS), Mig.Data$FallMSTime, sep=" ")
Mig.Data$FallMS <- as.POSIXct(strptime(Mig.Data$FalldtS, format="%Y-%m-%d %H:%M:%S"), tz="MST")

Mig.Data$SpringdtE <- paste(as.character(Mig.Data$SpringME), Mig.Data$SpringMETime, sep=" ")
Mig.Data$SpringME <- as.POSIXct(strptime(Mig.Data$SpringdtE, format="%Y-%m-%d %H:%M:%S"), tz="MST")
names(Mig.Data)
head(Mig.Data); tail(Mig.Data)

#Remove unncessary columns
Mig.Data<- Mig.Data[c(1:3,7,10)]

#create animal year column
d$id_yr <- paste(d$AID, d$Year, sep = "_")
Mig.Data$id_yr <- paste(Mig.Data$AID, Mig.Data$Year, sep = "_")

#Sort unique collar serial numbers; make sure the serial numbers look correct
sort(unique(d$id_yr))
head(d)
sort(unique(Mig.Data$id_yr))
head(Mig.Data)

#Merge all GPS collar data with migration timing data by AID                
mydata <- merge(d, Mig.Data, by=c("id_yr"), all=TRUE)
names(mydata)
head(mydata)

#Omit any deer IDs with unknown migratory strategies and any null values in the migration timing data
mydata$Mgtry<-mydata$MigratoryStrategy
names(mydata)
mydata<-mydata[mydata$Mgtry!="N/A", ]
mydata<-mydata[!is.na(mydata[,19]),] 
mydata<-mydata[!is.na(mydata[,20]),] 
head(mydata)

#Organize dataframe by AID and POSIXct
mydata<-mydata[order(mydata$id_yr, mydata$POSIXct),]
head(mydata)

#Convert date and time columns into character data type
mydata$FallMS<-as.character(mydata$FallMS)
mydata$SpringME<-as.character(mydata$SpringME)
mydata$POSIXct<-as.character(mydata$POSIXct)

#Identify unique IDs and create empty output dataframe 
Deer.ID <- unique(mydata$id_yr)
df<- data.frame()

#Create for loop that subsets migration data based on migration timing and unique AID
for (i in 1:length (Deer.ID)){
  temp.2<-subset(mydata, id_yr==Deer.ID[i])
  sub.1<-subset(temp.2, temp.2$POSIXct <= temp.2$FallMS & temp.2$POSIXct>=temp.2$SpringME)
  df<-rbind(df,sub.1)
  }

#Create new dataframe
clean.Data<-as.data.frame(df)
head(clean.Data)

temp<-subset(clean.Data,clean.Data$id_yr=="108_2021")
tail(temp)

#Ensure that frequency field as 3 decimal places
specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
#clean.Data$Frequency<-specify_decimal(clean.Data$Frequency, 3)
names(clean.Data)

#Remove unnessary columns and reorganize columns
clean.Data<-clean.Data[c(3,4:15)]
clean.Data<-clean.Data[order(clean.Data$AID, clean.Data$POSIXct),]
names(clean.Data)
clean.Data <- clean.Data[c("AID.x","Mgtry","CllrSrN","Frequency","POSIXct","Month","Day", "Year.x", "DayOfYear","Date","Time","Lat","Long")]

names(clean.Data) <- c("AID","Mgtry","CllrSrN","Frequency","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long")

head(clean.Data); tail(clean.Data)

clean.Data <- clean.Data %>% filter(Mgtry == "Short")

write.csv(clean.Data, "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/AllYears/RD2H_Short_SummerRange_2014to2021.csv")

# 
# NOT RUN
#
# #Export data into shapefile for future data analyses
# #Create spatial dataframe
# UTM_E = clean.Data[,c('Long')]
# UTM_N = clean.Data[,c('Lat')]
# 
# UTM_E <- SpatialPointsDataFrame(cbind(UTM_E,UTM_N), clean.Data, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# 
# #Reproject shapefile into desired coordinate system
# finalshape <- spTransform(UTM_E, CRS("+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
# 
# #Designate the name of the shapefile
# outname<- paste("RD2H",sep="_","SpringMig2020")
# 
# #Export the shapefile 
# writeOGR(finalshape, "D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/Migrations/2020/SpringMigration", outname, driver="ESRI Shapefile",overwrite=TRUE)
# 
# #Import shapefile and separate into different shapefiles based on unique Deer AID
# data<-readOGR("D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/Migrations/2020/SpringMigration","RD2H_SpringMig2020")
# outPath = ("D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/Migrations/2020/SpringMigration/IndividualShapefiles")
# fileSuffix='2020'
# 
# unique <- unique(data@data$AID)
# head(unique)
# 
# #Loop through and create separate shapefiles for each unique Deer AID
# for (i in 1:length(unique)) {
#   shape <- data[data$AID == unique[i], ]
#   outname<- paste("RD2H",unique[[i]],"SpringMig",sep="_",fileSuffix)
#   writeOGR(shape,outPath,as.character(outname), driver='ESRI Shapefile')
# 
# }
# # 
