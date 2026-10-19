##Code To Download Locations and Produce KML for upload to OnX ##
# Written by Luke Wilde, April 2022

FawnRecruitment_OnXLayers <- function(DaysTail = 3, completed = c(""), DeerIDLoc = "RD2H_AllDeerIDs_02OCTOBER2022.csv"){

#load libraries

library(devtools)
#devtools::install_github("jmerkle1/MerkleLab-Code-Repository", subdir="MoveTools")
library(MoveTools)
library(sf)
library(tidyverse)

source("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/RCode/Misc/DownloadingIridium_FetchFunction.R")

# SETTINGS FOR ALL
days <- DaysTail #how many days "tail" are needed

#----------------------------------------------------------------#
#### Download ATS ####
#----------------------------------------------------------------#

# ATS Auto Download ####
library(devtools)
#remotes::install_github("huh/collar", build_vignettes = TRUE)
library(collar)
library(lubridate)
library(tidyverse)

# login to ATS site with Cody info
ats_login(usr = "MA17201RE", pwd = "V0fE1F$E")

fetch_ats_devices()

# download position data for multiple collars at once
RDH_ATS_collars <- c("048631", "048626")

# fetch them
fixes_ATS_RDH <- fetch_ats_positions(device_id = RDH_ATS_collars)

# logout
ats_logout()


#clean and prep
names(fixes_ATS_RDH)
head(fixes_ATS_RDH)

#cut down to necessary columns
gps_ATS <- fixes_ATS_RDH %>% dplyr::select(CollarSerialNumber, DateLocal, JulianDay, Latitude, Longitude, HDOP, NumSats)
head(gps_ATS)
#
gps_ATS <- gps_ATS %>% mutate(Year = year(DateLocal), Month = month(DateLocal), Day = day(DateLocal), Date = date(DateLocal), POSIXct = as.POSIXct(DateLocal, tx="MST"), Time = format(DateLocal, "%H:%M:%S"), CllrSrN = CollarSerialNumber, DayOfYear = JulianDay, Lat = Latitude, Long = Longitude)

gps_ATS$Altitude <- "NA"
gps_ATS$NumSats <- "NA"


names(gps_ATS)

#columns to keep 
final.a <- gps_ATS %>% dplyr::select("CllrSrN","POSIXct","Month","Day", "Year", "DayOfYear","Date","Time","Lat","Long","Altitude","HDOP","NumSats")

final.a<-final.a[final.a$Long!=0 & final.a$Lat!=0, ]

final.a<-final.a %>% filter(!is.na(POSIXct)) %>% as.data.frame()
head(final.a)
str(final.a)
names(final.a)

final.a <- final.a %>% filter(POSIXct >= Sys.time() - (days * 24 * 3600))

final.a$CllrSrN <- dplyr::recode(final.a$CllrSrN, "048626" = "48626", "048631" = "48631")
rm(gps_ATS, fixes_ATS_RDH)



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
                                         start_date = Sys.time() - days * 24 * 3600)  # if you put NULL here, you'll get ALL data, if you change the first number to 1, you get only todays data

#fixes will save to a directory, and you can store that location below as the dir.tdc object. Or you can integrate in the case that Jerod's code loads directly into the R environment

fixes.IR <- fetch_telonics_iridium_positions(username = username,password = password,
                                 fldr_out = fldr_out,  
                                 TDC_path = TDC_path,
                                 keep.reports = FALSE,   # if FALSE, will remove all temp files at the end
                                 start_date = Sys.time() - days * 24 * 3600)

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

#Note, I did not remove any HDOP errors, because unlike LOTEK...I can't quite figure out what values of HDOP are truly errors

#Omit POSIXct values with null values
final.t<-final.t[!is.na(final.t[,2]),] 

#Remove any rows containing zero values for Latitude and Longitude
final.t<-final.t[final.t$Long!=0 & final.t$Lat!=0, ]


#make both spatial, same crs, and then bind
final.a <- st_as_sf(final.a, 
                    coords = c("Long", "Lat"), # note x goes first
                    crs = "+proj=latlon +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs", # projection, this is NAD83
                    remove = FALSE)

final.t <- st_as_sf(final.t, 
                      coords = c("Long", "Lat"), # note x goes first
                      crs = "+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs", # projection, this is NAD83
                      remove = FALSE)

final.a <- final.a %>% st_transform(., "+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs") %>% mutate(Lat = st_coordinates(.)[,2], Long = st_coordinates(.)[,1])
  
final.a
final.t
  

#bind
final.ta.sp <- rbind(final.t, final.a)
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
DeerID.Data <- read.table(DeerIDLoc, header=TRUE, sep=",")


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
mydata <- final.ta %>% left_join(DeerID.Data, by=c("CllrSrN"), all=TRUE)

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

#----------------------------------------------------------------#
#### Check outputs ####
#----------------------------------------------------------------#

library(mapview)

# CHECK TO MAKE SURE DATA IS UP TO DATE
as.data.frame(final.sf %>% group_by(AID) %>% summarise(min = min(Date), max = max(Date)))

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

final.sf <- final.sf %>% mutate(newID = paste(AID,"_",Frequency,"_",TagColor,TagNum, sep=""))#"_",Mgtry,


#remove completed
final.sf <- final.sf %>% filter(AID %in% completed)

# final.sf <- final.sf %>% arrange(newID, POSIXct) %>% filter(AID < 650)

final.point <- final.sf %>%
  group_by(newID) %>%
  arrange(POSIXct) %>%
  filter(row_number()==1) %>% dplyr::select(newID)

final.sf.ll <- st_transform(final.sf, "+proj=latlon +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs")


final.lines <- Points2Lines(data=final.sf, date_name="POSIXct", id_name="newID",byid=T)

#----------------------------------------------------------------#
#### Prepare to Export ####
#----------------------------------------------------------------#

final.point <- final.point %>% rename(Description = newID)
final.lines <- final.lines %>% dplyr::select(newID) %>% rename(Description = newID)

final.lines.ll <- st_transform(final.lines, "+proj=latlon +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs")

final.point.ll <- st_transform(final.point, "+proj=latlon +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs")

final.point.sp <- as(final.point.ll, "Spatial")



final.lines.ll <- final.lines.ll[which(npts(final.lines.ll, by_feature = T) >= 2),] # make sure to only add lines for deer with >1 point or will throw errors


#----------------------------------------------------------------#
# Write KML ####
#----------------------------------------------------------------#
library(maptools)

kmlPoints(obj=final.point.sp["Description"], kmlfile="./RDH_Fall2022_points.kml", kmlname="daily points", name = final.point.sp$Description)
st_write(final.lines.ll, dsn="./RDH_Fall2022_lines.kml", driver = "kml", delete_dsn = TRUE)

options(scipen=999)
which(st_coordinates(final.sf$geometry)[,2] == as.numeric(st_bbox(final.sf)["ymin"]))

st_bbox(final.lines)["ymin"]


}

#------------------------#
### BELOW CODE NOT RUN ###
#------------------------#

#code to cut

# 
# #Remove unncessary columns
# DeerID.Data<- DeerID.Data[c(1:7,10)]
# head(DeerID.Data)
# 
# #Sort unique collar serial numbers; make sure the serial numbers look correct
# sort(unique(final.ta$CllrSrN))
# sort(unique(DeerID.Data$CllrSrN))
# 
# #Merge all GPS collar data with Deer ID csv by CllrSrN                
# mydata <- final.ta %>% left_join(DeerID.Data, by=c("CllrSrN"), all=TRUE) 
# 
# #Organize dataframe by CllrSrN, POSIXct, and AID
# mydata<-mydata[order(mydata$CllrSrN, mydata$POSIXct, mydata$AID),]
# 
# #Omit any deer IDs that are N/A
# table(is.na(mydata[,11]))
# mydata<-mydata[!is.na(mydata[,11]),] 
# 
# #Convert date and time columns into character data type
# mydata$StartDateTime<-as.character(mydata$StartDateTime)
# mydata$StopDateTime<-as.character(mydata$StopDateTime)
# mydata$POSIXct<-as.character(mydata$POSIXct)
# 
# #Subset data based on StopDateTime (NULL values present vs. NULL values not present)
# my.data.Null<-subset(mydata, is.na(mydata$StopDateTime))
# my.data.NoNull<-subset(mydata, !is.na(mydata$StopDateTime))
# 
# 
# 
# Deer.ID.1 <- unique(my.data.Null$AID)
# Deer.CllrSrN.1 <- unique(my.data.Null$CllrSrN) 
# df.1<- data.frame()
# df.1<-as.data.frame(my.data.Null)
# full.df.1<-df.1[0, ]
# 
# starttime <- as.POSIXct(Sys.time())
# 
# #Create for loop that creates unique Deer IDs with associated CllrSrN based on range of dates
# for (i in 1:length (Deer.ID.1)){
#   temp.2<-subset(my.data.Null, AID==Deer.ID.1[i])
#   for (j in 1:length (Deer.CllrSrN.1)){
#     temp.3<-subset(temp.2, CllrSrN==Deer.CllrSrN.1[j])
#     sub.1<-subset(temp.3, temp.3$POSIXct >= temp.3$StartDateTime)
#     full.df.1<-rbind(full.df.1,sub.1)
#   }
# }
# 
# final.gps <- full.df.1



#final.gps <- final.gps %>% dplyr::select(AID, Mgtry, TagColor, TagNum, CllrSrN, Frequency, Lat, Long, POSIXct, Date, Time)


# 
# #### Code to Send Emails ####
# 
# library(sendmailR)
# 
# 
# 
# SubjectText = paste0("Mule Deer (Red Desert and Boulder) Telonics collar update: ",Sys.Date())
# 
# BodyText = paste0("The latest GPS data for Mule Deer in Red Desert and Boulder have been downloaded from the Telonics site.
# 
#  
# 
# The first attachment is a general map of the last ",days2plot/24,  " days of data.
# 
#  
# 
# The second attachment is a figure depicting movement speed in last 2 days.
# 
#  
# 
# There are ", nrow(lns), " individuals on air right now. Of those, ", length(possible.morts), " (", paste(names(possible.morts), collapse = ", "), ") might be mortalities as their mean distance moved per location in the last 7 days is less than 20 meters!
# 
#  
# 
# If you have any questions/comments, please respond to Jerod Merkle (jmerkle@uwyo.edu). Thank you!")
# 
# 
# 
# 
# 
# To_Emails <- c(lwilde2@uwyo.edu,anne.scholle@uwyo.edu,
#                
#                khagan@uwyo.edu,gregory.nickerson@uwyo.edu,aortega5@uwyo.edu)
# 
# 
# 
# sendmail(
#   
#   from = sprintf("lwilde2@uwyo.edu"),
#   
#   to = sprintf(To_Emails),
#   
#   subject = SubjectText,
#   
#   msg = list(mime_part(BodyText),
#              
#              mime_part(paste0(fldr_out, "/telonics_updates.html")),
#              
#              mime_part(paste0(fldr_out, "/Speed_plot.png"))),
#   
#   control=list(smtpServer= "mail940.uwyo.edu")
#   
# )

