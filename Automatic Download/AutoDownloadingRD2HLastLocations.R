#Determining the last known locations for RD2H mule deer##########
#Code written by Anna Ortega
#January 15, 2018

#updated LRW: 10/14/2021

#Load required libraries
library(rgdal)
library(sf)
library(sp)
library(mapview)

#to make fully auto
#source("D:/RedDesertToHobackMuleDeerResearchProject/RCode/Automatic Download/AutoAppendingRD2HDataToExistingDatabase.R")

#Setting working directory and import temporary csv file
load("D:/RD2H_GPSCollarData/RD2H_AllData.RData")
data<-as.data.frame(finalshape)
rm(finalshape)

l<-unique(data$AID)

#Identify different IDs and create empty output dataframe
IDlist <- unique(data$AID)
shape <- data.frame()

#Grabs the last known location of each deer
for (i in 1:length(IDlist)){
  temp<- subset(data, AID==IDlist[i])
  c <- tail(temp, n=1) #Change to 2 if you want last 2 days
  shape<- rbind(shape, c)
}

#Remove all individuals that have died from dataframe
shape<-shape[shape$AID!="112",]
shape<-shape[shape$AID!="113",]
shape<-shape[shape$AID!="114",]
shape<-shape[shape$AID!="115",]
shape<-shape[shape$AID!="117",]
shape<-shape[shape$AID!="119",]
shape<-shape[shape$AID!="120",]
shape<-shape[shape$AID!="121",]
shape<-shape[shape$AID!="123",]
shape<-shape[shape$AID!="124",]
shape<-shape[shape$AID!="127",]
shape<-shape[shape$AID!="128",]
shape<-shape[shape$AID!="129",]
shape<-shape[shape$AID!="130",]
shape<-shape[shape$AID!="218",]
shape<-shape[shape$AID!="219",]
shape<-shape[shape$AID!="229",]
shape<-shape[shape$AID!="230",]
shape<-shape[shape$AID!="231",]
shape<-shape[shape$AID!="233",]
shape<-shape[shape$AID!="234",]
shape<-shape[shape$AID!="235",]
shape<-shape[shape$AID!="241",]
shape<-shape[shape$AID!="248",]
shape<-shape[shape$AID!="249",]
shape<-shape[shape$AID!="251",]
shape<-shape[shape$AID!="253",]
shape<-shape[shape$AID!="254",]
shape<-shape[shape$AID!="256",]
shape<-shape[shape$AID!="257",]
shape<-shape[shape$AID!="258",]
shape<-shape[shape$AID!="259",]
shape<-shape[shape$AID!="260",]
shape<-shape[shape$AID!="261",]
shape<-shape[shape$AID!="264",]
shape<-shape[shape$AID!="270",]
shape<-shape[shape$AID!="272",]
shape<-shape[shape$AID!="274",]
shape<-shape[shape$AID!="276",]
shape<-shape[shape$AID!="277",]
shape<-shape[shape$AID!="278",]
shape<-shape[shape$AID!="282",]
shape<-shape[shape$AID!="283",]
shape<-shape[shape$AID!="286",]
shape<-shape[shape$AID!="287",]
shape<-shape[shape$AID!="288",]
shape<-shape[shape$AID!="290",]
shape<-shape[shape$AID!="291",]
shape<-shape[shape$AID!="292",]
shape<-shape[shape$AID!="293",]
shape<-shape[shape$AID!="295",]
shape<-shape[shape$AID!="296",]
shape<-shape[shape$AID!="297",]
shape<-shape[shape$AID!="298",]
shape<-shape[shape$AID!="299",]
shape<-shape[shape$AID!="301",]
shape<-shape[shape$AID!="302",]
shape<-shape[shape$AID!="303",]
shape<-shape[shape$AID!="304",]
shape<-shape[shape$AID!="309",]
shape<-shape[shape$AID!="310",]
shape<-shape[shape$AID!="311",]
shape<-shape[shape$AID!="312",]
shape<-shape[shape$AID!="315",]
shape<-shape[shape$AID!="316",]
shape<-shape[shape$AID!="317",]
shape<-shape[shape$AID!="318",]
shape<-shape[shape$AID!="319",]
shape<-shape[shape$AID!="320",]
shape<-shape[shape$AID!="321",]
shape<-shape[shape$AID!="322",]
shape<-shape[shape$AID!="323",]
shape<-shape[shape$AID!="324",]
shape<-shape[shape$AID!="325",]
shape<-shape[shape$AID!="326",]
shape<-shape[shape$AID!="328",]
shape<-shape[shape$AID!="329",]
shape<-shape[shape$AID!="332",]
shape<-shape[shape$AID!="334",]
shape<-shape[shape$AID!="336",]
shape<-shape[shape$AID!="337",]
shape<-shape[shape$AID!="343",]
shape<-shape[shape$AID!="344",]
shape<-shape[shape$AID!="345",]
shape<-shape[shape$AID!="348",]
shape<-shape[shape$AID!="350",]
shape<-shape[shape$AID!="351",]
shape<-shape[shape$AID!="352",]
shape<-shape[shape$AID!="353",]
shape<-shape[shape$AID!="356",]
shape<-shape[shape$AID!="357",]
shape<-shape[shape$AID!="359",]
shape<-shape[shape$AID!="361",]
shape<-shape[shape$AID!="363",]
shape<-shape[shape$AID!="364",]
shape<-shape[shape$AID!="365",]
shape<-shape[shape$AID!="366",]
shape<-shape[shape$AID!="367",]
shape<-shape[shape$AID!="369",]
shape<-shape[shape$AID!="370",]
shape<-shape[shape$AID!="400",]
shape<-shape[shape$AID!="402",]
shape<-shape[shape$AID!="406",]
shape<-shape[shape$AID!="409",]
shape<-shape[shape$AID!="411",]
shape<-shape[shape$AID!="412",]
shape<-shape[shape$AID!="413",]
shape<-shape[shape$AID!="414",]
shape<-shape[shape$AID!="415",]
shape<-shape[shape$AID!="417",]
shape<-shape[shape$AID!="419",]
shape<-shape[shape$AID!="420",]
shape<-shape[shape$AID!="421",]
shape<-shape[shape$AID!="422",]
shape<-shape[shape$AID!="423",]
shape<-shape[shape$AID!="427",]
shape<-shape[shape$AID!="428",]
shape<-shape[shape$AID!="430",]
shape<-shape[shape$AID!="432",]
shape<-shape[shape$AID!="433",]
shape<-shape[shape$AID!="434",]
shape<-shape[shape$AID!="435",]
shape<-shape[shape$AID!="436",]
shape<-shape[shape$AID!="437",]
shape<-shape[shape$AID!="440",]
shape<-shape[shape$AID!="441",]
shape<-shape[shape$AID!="443",]
shape<-shape[shape$AID!="448",]
shape<-shape[shape$AID!="449",]
shape<-shape[shape$AID!="450",]
shape<-shape[shape$AID!="453",]
shape<-shape[shape$AID!="455",]
shape<-shape[shape$AID!="458",]
shape<-shape[shape$AID!="459",]
shape<-shape[shape$AID!="461",]
shape<-shape[shape$AID!="462",]
shape<-shape[shape$AID!="466",]
shape<-shape[shape$AID!="468",]
shape<-shape[shape$AID!="469",]
shape<-shape[shape$AID!="471",]
shape<-shape[shape$AID!="472",]
shape<-shape[shape$AID!="500",]
shape<-shape[shape$AID!="501",]
shape<-shape[shape$AID!="504",]
shape<-shape[shape$AID!="506",]
shape<-shape[shape$AID!="508",]
shape<-shape[shape$AID!="510",]
shape<-shape[shape$AID!="517",]
shape<-shape[shape$AID!="518",]
shape<-shape[shape$AID!="519",]
shape<-shape[shape$AID!="522",]
shape<-shape[shape$AID!="523",]
shape<-shape[shape$AID!="539",]
shape<-shape[shape$AID!="239",] # 10/07/21

#Remove male buck from dataset
shape<-shape[shape$AID!="456",] #Removed collar during December 2020 captures

#Remove all individuals with collar malfunctions from dataframe
shape<-shape[shape$AID!="116",]
shape<-shape[shape$AID!="221",]
shape<-shape[shape$AID!="227",]
shape<-shape[shape$AID!="240",]
shape<-shape[shape$AID!="244",]
shape<-shape[shape$AID!="262",]
shape<-shape[shape$AID!="263",]
shape<-shape[shape$AID!="266",]
shape<-shape[shape$AID!="268",]
shape<-shape[shape$AID!="281",]
shape<-shape[shape$AID!="285",]
shape<-shape[shape$AID!="289",]
shape<-shape[shape$AID!="305",]
shape<-shape[shape$AID!="308",]
shape<-shape[shape$AID!="333",]
shape<-shape[shape$AID!="335",]
shape<-shape[shape$AID!="342",]
shape<-shape[shape$AID!="346",]
shape<-shape[shape$AID!="354",]
shape<-shape[shape$AID!="425",]

#Remove individuals from fawn recruitment ####
shape<-shape[shape$AID!="431",] #Female that was not pregnant in March 2021

#Sort data by ID
ordered<-shape[order(shape$AID),]

#Create spatial dataframe
UTM_E = ordered[,c('Long')]
UTM_N = ordered[,c('Lat')]

UTM_E <- SpatialPointsDataFrame(cbind(UTM_E,UTM_N), ordered, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#Reproject shapefile into desired coordinate system
finalshape <- spTransform(UTM_E, CRS("+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))

#Export shapefile
outPath="D:/RD2H_GPSCollarData"
fileSuffix="AllLastLoc_05OCTOBER2021_FawnRecruitment"
outname<- paste("RD2H",sep="_", fileSuffix)

writeOGR(finalshape, "D:/RD2H_GPSCollarData", outname, driver="ESRI Shapefile",overwrite=TRUE)

finalshape_sf <- st_as_sf(finalshape)

mapview(finalshape_sf, legend = mapviewGetOption("legend"),  zcol = "Mgtry", label = "AID", col.regions = c("blue", "green", "orange", "red"))

#Combining capture info with last locations
#Import capture info
# cap.info<-read.csv("C:/Users/aortega5/Desktop/Dec2020Captures/Dec2020_CaptureList.csv",header=TRUE,sep=",")
# head(cap.info)
# cap.info$AID<-as.factor(cap.info$AID)
# 
# #Convert shapefile into data frame
# #Convert AID into factor data type
# last.loc<-as.data.frame(finalshape)
# last.loc$AID<-as.factor(last.loc$AID)
# head(last.loc)
# 
# #Remove unncessary columns
# names(last.loc)
# last.loc<-last.loc[c(2:6,13:14)]
# 
# #Merge last locations with capture info by AID
# final.locs<-merge(last.loc, cap.info, by = "AID")
# names(final.locs)
# 
# #Remove unncessary columns and rename column headers
# final.locs<-final.locs[c(1:7)]
# colnames(final.locs)<-c("AID","Mig","CllrSrN","Frequency","POSIXct","Lat","Long")
# head(final.locs)
# 
# #Sort data by frequency
# ordered2<-final.locs[order(final.locs$Frequency),]
# View(ordered2)
# 
# #Create spatial dataframe
# UTM_E = ordered2[,c('Long')]
# UTM_N = ordered2[,c('Lat')]
# 
# UTM_E <- SpatialPointsDataFrame(cbind(UTM_E,UTM_N), ordered2, proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# 
# #Reproject shapefile into desired coordinate system
# finalshape2 <- spTransform(UTM_E, CRS("+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"))
# head(finalshape2)
# all2<-finalshape2[c(4,6:7)]
# head(all2)
# str(all2)
# 
# #Export shapefile
# outPath="C:/Users/aortega5/Desktop/RD2H_GPSCollarData"
# fileSuffix="DecCaptures_LastLoc_06DEC2020"
# outname<- paste("RD2H",sep="_", fileSuffix)
# 
# writeOGR(all2, "C:/Users/aortega5/Desktop/RD2H_GPSCollarData", outname, driver="ESRI Shapefile",overwrite=TRUE)
