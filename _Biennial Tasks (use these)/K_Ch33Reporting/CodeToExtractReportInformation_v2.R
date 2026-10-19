# New code to prepare data and ch33 report materials
# Luke Wilde
# written 01/30/2024

#updated 01/22/2025

# load libraries ####
#------------------------------#
library(tidyverse)
library(sf)
library(lubridate)
library(readxl)


select <- dplyr::select

#ATTN: need to update each date!

#report year
year <- 2024
#cap data
meta <- "C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_10DECEMBER2024.csv"
#location of capture records
load("~/PhD_Chapters/Datasets/ChapterThree_Profiles/Data/CaptureData_2014to2024.RData")
#location of data
load("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_CleanData.RData")
# load age data
agedata <- read_csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/0_MainDataSets/RDH_KnownAgeByAID_Rolling.csv")
CBE1 <- read_csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/Chapter33Permit/2024/Wilde_RedDesertMuleDeer_Dec2024_CBE.csv")
CBE2 <- read_csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/Chapter33Permit/2024/Wilde_RedDesertMuleDeer_Mar2024_CBE.csv") 

#location of capture data
Dec <- read_excel(paste("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/Annual Capture Data/RD2H_MD_Dec",year,"_CaptureData.xlsx",sep=""))
Mar <- read_excel(paste("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/Annual Capture Data/RD2H_MD_March",year,"_CaptureData.xlsx",sep=""))

# Call data ####
#-----------------------------------#



#Mar
Mar <- Mar  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex, GeneralAgeClass, CTE)
Mar <- Mar %>% select(-MigrationBehavior) %>% left_join(read.csv(meta) %>% mutate(MigrationBehavior = Mgtry, DeerID = as.character(AID)) %>% select(DeerID, MigrationBehavior), by = "DeerID")


#Dec
Dec <- Dec %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex, GeneralAgeClass, CBETag) %>% mutate(Age = 0)
Dec <- Dec %>% select(-MigrationBehavior) %>% left_join(read.csv(meta) %>% mutate(MigrationBehavior = Mgtry, DeerID = as.character(AID)) %>% select(DeerID, MigrationBehavior), by = "DeerID", relationship = "many-to-many")
Dec <- Dec[!duplicated(Dec$DeerID),]; Dec


currentMar <- Mar
currentDec <- Dec




#------------------------------#
# gps data prep ####
DeerID <- read.csv(meta) 
str(DeerID) #check structure

#reformat Dates
DeerID$StopDate <- as.POSIXct(DeerID$StopDate, format = "%m/%d/%Y", tz = "MST")
DeerID$StartDate <- as.POSIXct(DeerID$StartDate, format = "%m/%d/%Y", tz = "MST")
DeerID$CollarMalfDate <- as.POSIXct(DeerID$CollarMalfDate, format = "%m/%d/%Y", tz = "MST")

#------------------------------#
# Extract relevant info ####

# identify the first time a deer was monitored
first <- DeerID %>% arrange(AID, StartDate) %>% group_by(AID) %>% slice(1) %>% mutate(FirstYear = year(StartDate))
# identify the last time a deer was monitored
last <- DeerID %>% arrange(AID, desc(StartDate)) %>% group_by(AID) %>% slice(1)


# find which deer are dead, all those without an NA for stopdate
Dead <- last %>% filter(!is.na(StopDate)) %>% filter(is.na(CollarMalfDate)) %>% mutate(Year = year(StopDate), Month = month(StopDate)) 
Alive <- last[which(is.na(last$StopDate)),] %>% mutate(Year = 2024) 


#------------------------------#
# clip gps data ####
currentData <- rdh_data.sf %>% filter(POSIXct >= as.POSIXct("2024-01-01 00:00:01", tz = "MST") & POSIXct <= as.POSIXct("2025-01-01 00:00:00", tz = "MST")) 


#export for csv share
write.csv(currentData %>% st_drop_geometry(), "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/Chapter33Permit/2024/Reporting/RDH_Kauffman_33-937_GPSDataset_2024.csv")


#------------------------------#
# other report information ####

nrow(first)
length(unique(rdh_data.sf$id_yr))
nrow(CAPall)
length(unique(currentMar$DeerID))
length(unique(currentDec$DeerID))




#------------------------------#
# survivability ####
sex = "Male"
sex2 = "Female"
age = "Fawn"
age2 = "Yearling"
age3 = "Adult"

marchID <- unique(currentMar %>% filter(GeneralAgeClass %in% c(age) & Sex %in% c(sex2)) %>% select(DeerID, Sex))
decID <- unique(currentDec %>% filter(GeneralAgeClass %in% c(age) & Sex %in% c(sex2) ) %>% select(DeerID, Sex))


survivabilityYoungF <- Dead %>% filter(AID %in% c(marchID$DeerID, decID$DeerID) & Year == 2024) %>% group_by(Month) %>% summarise(count = n()) %>% filter(Month > 2);survivabilityYoungF
currentData %>% filter(Month == 3) %>% filter(AID %in% rbind(marchID, decID)$DeerID) %>% reframe(n = length(unique(AID)))

marchID <- unique(currentMar %>% filter(GeneralAgeClass %in% c(age) & Sex %in% c(sex)) %>% select(DeerID, Sex))
decID <- unique(currentDec %>% filter(GeneralAgeClass %in% c(age) & Sex %in% c(sex) ) %>% select(DeerID, Sex))


survivabilityYoungM <- Dead %>% filter(AID %in% c(marchID$DeerID, decID$DeerID) & Year == 2024) %>% group_by(Month) %>% summarise(count = n()) %>% filter(Month > 2);survivabilityYoungM
currentData %>% filter(Month == 3) %>% filter(AID %in% rbind(marchID, decID)$DeerID) %>% reframe(n = length(unique(AID)))


marchID <- unique(currentMar %>% filter(GeneralAgeClass %in% c(age3, age2) & Sex %in% c(sex2)) %>% select(DeerID))
decID <- unique(currentDec %>% filter(GeneralAgeClass %in% c(age3, age2) & Sex %in% c(sex2)) %>% select(DeerID))

survivabilityAdultF <- Dead %>% filter(AID %in% c(marchID$DeerID, decID$DeerID) & Year == 2024) %>% group_by(Month) %>% summarise(count = n()) %>% filter(Month > 2);survivabilityAdultF
currentData %>% filter(Month == 3) %>% filter(AID %in% rbind(marchID, decID)$DeerID) %>% reframe(n = length(unique(AID)))

marchID <- unique(currentMar %>% filter(GeneralAgeClass %in% c(age2, age3) & Sex %in% c(sex)) %>% select(DeerID))
decID <- unique(currentDec %>% filter(GeneralAgeClass %in% c(age2, age3) & Sex %in% c(sex)) %>% select(DeerID))

survivabilityAdultM <- Dead %>% filter(AID %in% c(marchID$DeerID, decID$DeerID) & Year == 2024) %>% group_by(Month) %>% summarise(count = n()) %>% filter(Month > 2);survivabilityAdultM
currentData %>% filter(Month == 3) %>% filter(AID %in% rbind(marchID, decID)$DeerID) %>% reframe(n = length(unique(AID)))


DeerID %>% filter(CollarMalfDate > as.Date("2024-01-01")) %>% select(AID, TagColor, CollarMalfDate)


#OLD - do not use
#collarsMalf <- currentData %>% filter(AID %in% Alive$AID & AID %in% c(marchID$DeerID, decID$DeerID)) %>% arrange(AID, desc(POSIXct)) %>% group_by(AID) %>% slice(1) %>% select(AID, POSIXct) %>% mutate(malf = (POSIXct - as.POSIXct("2024-01-01 00:00:00", tz = "MST"))/(3600*24)) %>% arrange(malf); collarsMalf

collarsMalf <- DeerID %>% filter(!is.na(CollarMalfDate)); collarsMalf


#------------------------------#
# info for frequency list ####

#ready info
permit <- "33-937"
species <- "Odocoileus hemionus"
proj <- "March 2014-December 2024"
freq <- "Yes"

data <- data.frame("Movebank_ID" = unique(currentData$AID))

#OLD WAY - do not use - CAPall is done below!

first <- CAPall %>% mutate(AID = DeerID) %>% arrange(AID, CapDate) %>% group_by(AID) %>% slice(1)

data <- data %>% mutate(AID = as.character(Movebank_ID)) %>% left_join(first %>% select(CapDate, Sex, AID), by = "AID") %>% mutate(Date_Collar_Activated = 
CapDate) %>% select(AID, Movebank_ID, Sex, Date_Collar_Activated)

head(data)

data <- data %>% mutate(AID = as.integer(Movebank_ID)) %>% left_join(last %>% select(CllrSrN, Frequency, TagColor, TagNum, Mgtry, AID, ExpectedBatteryLife_AverageCase), by = "AID") %>% mutate(Collar_Serial_Number = CllrSrN, Transmitter_Frequency_MHz = Frequency, Eartag_Color_and_Number = paste(TagColor," ", TagNum, sep=""), Second_Eartag_Color_and_Number = "", Potential_Range_of_Movement_Miles = ifelse(Mgtry == "Long", "90-150", ifelse(Mgtry == "Medium", "50-90", ifelse(Mgtry == "Short", "<30", ifelse(Mgtry == "Resident", "<30",NA)))), Expected_Life_of_Transmitter = ExpectedBatteryLife_AverageCase) %>% select(AID, Movebank_ID, Sex, Date_Collar_Activated, Second_Eartag_Color_and_Number, Collar_Serial_Number, Transmitter_Frequency_MHz, Eartag_Color_and_Number, Potential_Range_of_Movement_Miles, Expected_Life_of_Transmitter)

data <- data %>% mutate(AID = as.integer(Movebank_ID)) %>% left_join(last %>% select(AID, StopDate), by = "AID") %>% mutate(Comments = ifelse(is.na(StopDate), "Alive", paste0("Died on ",StopDate," from unknown causes"))) %>% select(AID, Movebank_ID, Sex, Date_Collar_Activated, Second_Eartag_Color_and_Number, Collar_Serial_Number, Transmitter_Frequency_MHz, Eartag_Color_and_Number, Potential_Range_of_Movement_Miles, Expected_Life_of_Transmitter, Comments)

agedata <- agedata %>% arrange(AID, BirthYear) %>% group_by(AID) %>% slice(1)

data <- data %>% mutate(AID = as.integer(Movebank_ID)) %>% left_join(agedata %>% mutate(AID = as.numeric(AID)) %>% select(AID, BirthYear, Method), by = "AID") %>% mutate(Age = ifelse(is.na(BirthYear) | BirthYear == 9999, NA, 2025.5 - BirthYear)) %>% select(AID, Movebank_ID, Sex, Date_Collar_Activated, Second_Eartag_Color_and_Number, Collar_Serial_Number, Transmitter_Frequency_MHz, Eartag_Color_and_Number, Potential_Range_of_Movement_Miles, Expected_Life_of_Transmitter, Comments, Age, Method)

caploc <- rdh_data.sf %>% arrange(AID, POSIXct, UTM_E, UTM_N) %>% group_by(AID) %>% slice(1)

data <- data %>% mutate(AID = as.integer(Movebank_ID)) %>% left_join(caploc %>% select(AID, UTM_E, UTM_N), by = "AID") %>% mutate(Initial_Capture = paste(round(UTM_E,1), "E ", round(UTM_N,1), "N", sep="")) %>% select(AID, Movebank_ID, Sex, Date_Collar_Activated, Second_Eartag_Color_and_Number, Collar_Serial_Number, Transmitter_Frequency_MHz, Eartag_Color_and_Number, Potential_Range_of_Movement_Miles, Expected_Life_of_Transmitter, Comments, Age, Method, Initial_Capture)

data <- data %>% rename(Aging_Method = Method) %>% mutate(Count = 1:nrow(.), permit = "33-937", species = "Odocoileus hemionus", Nontelemetry_Collar_Color_and_Number = NA, Duration_of_Project = "March 2014-December 2024",  Will_Frequencies_be_Reused = "Yes") %>% select(Count, permit, species, Movebank_ID, Initial_Capture, Sex, Age, Aging_Method, Collar_Serial_Number, Transmitter_Frequency_MHz, Date_Collar_Activated, Eartag_Color_and_Number, Second_Eartag_Color_and_Number, Nontelemetry_Collar_Color_and_Number, Potential_Range_of_Movement_Miles, Expected_Life_of_Transmitter, Duration_of_Project, Will_Frequencies_be_Reused, Comments)


data %>% filter(Movebank_ID == 404)


#------------------------------#
# write to file ####

#ATTN: this is intermediate bc you go through and check for errors, as well as update the unkowns with those where we know what they died within 14 days of capture

write.csv(data, "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/Chapter33Permit/2024/Reporting/RDH_Kauffman_33-937_FreqList_Intermediate_2024.csv", row.names = F)

#-----------------------------#
# WYNDD ####


#---------------------------#
# ####
# 
# setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/")
# 
# 
# D14 <- read_excel("Annual Capture Data/RD2H_MD_Dec2014_CaptureData.xlsx")
# D15 <- read_excel("Annual Capture Data/RD2H_MD_Dec2015_CaptureData.xlsx")
# D16 <- read_excel("Annual Capture Data/RD2H_MD_Dec2016_CaptureData.xlsx")
# D17 <- read_excel("Annual Capture Data/RD2H_MD_Dec2017_CaptureData.xlsx")
# D18 <- read_excel("Annual Capture Data/RD2H_MD_Dec2018_CaptureData.xlsx")
# D19 <- read_excel("Annual Capture Data/RD2H_MD_Dec2019_CaptureData.xlsx")
# D20 <- read_excel("Annual Capture Data/RD2H_MD_Dec2020_CaptureData.xlsx")
# D21 <- read_excel("Annual Capture Data/RD2H_MD_Dec2021_CaptureData.xlsx")
# D22 <- read_excel("Annual Capture Data/RD2H_MD_Dec2022_CaptureData.xlsx")
# D23 <- read_excel("Annual Capture Data/RD2H_MD_Dec2023_CaptureData.xlsx")
# 
# M14 <- read_excel("Annual Capture Data/RD2H_MD_March2014_CaptureData.xlsx")
# M15 <- read_excel("Annual Capture Data/RD2H_MD_March2015_CaptureData.xlsx")
# M16 <- read_excel("Annual Capture Data/RD2H_MD_March2016_CaptureData.xlsx")
# M17 <- read_excel("Annual Capture Data/RD2H_MD_March2017_CaptureData.xlsx")
# M18 <- read_excel("Annual Capture Data/RD2H_MD_March2018_CaptureData.xlsx")
# M19 <- read_excel("Annual Capture Data/RD2H_MD_March2019_CaptureData.xlsx")
# M20 <- read_excel("Annual Capture Data/RD2H_MD_March2020_CaptureData.xlsx")
# M21 <- read_excel("Annual Capture Data/RD2H_MD_March2021_CaptureData.xlsx")
# M22 <- read_excel("Annual Capture Data/RD2H_MD_March2022_CaptureData.xlsx")
# M23 <- read_excel("Annual Capture Data/RD2H_MD_March2023 1 _CaptureData.xlsx")
# 
# M14 <- M14 %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# M15 <- M15 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# M16 <- M16 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# M17 <- M17 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# M18 <- M18 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# M19 <- M19 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# M20 <- M20 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# M21 <- M21 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# M22 <- M22 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# M23 <- M23 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# 
# # x <- data.frame("DeerID" = as.character(c(601, 603, 605,606,607,608,609,610,611,612,613,614,615,616,617,618, 619,620,621,622,623,624,625,626,627,628,629,630,631)), "MigrationStrategy2" = c("Long","Long", "Short","Medium","Medium","Medium","N/A","Short","Long","N/A","N/A","N/A","N/A","Short","Long","N/A","Medium","Long","Medium", "Medium","Long", "Long", "Long","Medium", "Long","Medium","Medium","Long","Long")
# # )
# 
# # tmp <- M22 %>% left_join(x, by = "DeerID")
# # M22$MigrationBehavior <- ifelse(tmp$MigrationBehavior =="N/A", tmp$MigrationStrategy2,tmp$MigrationBehavior)              
# 
# M22 <- M22 %>% select(-MigrationBehavior) %>% left_join(read.csv(meta) %>% mutate(MigrationBehavior = Mgtry, DeerID = as.character(AID)) %>% select(DeerID, MigrationBehavior), by = "DeerID")
# M23 <- M23 %>% select(-MigrationBehavior) %>% left_join(read.csv(meta) %>% mutate(MigrationBehavior = Mgtry, DeerID = as.character(AID)) %>% select(DeerID, MigrationBehavior), by = "DeerID")
# 
# 
# MARall <- rbind(M14, M15, M16, M17, M18, M19, M20, M21, M22, M23) %>% mutate(Year = year(CapDate)) 
# MARall$CapEvent <- paste0("Mar", " ", MARall$Year, sep = "")
# 
# #Dec
# D14 <- D14 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# D15 <- D15 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# D16 <- D16 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# D17 <- D17 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# D18 <- D18 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# D19 <- D19 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# D20 <- D20 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# D21 <- D21 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex)
# D22 <- D22 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex) %>% mutate(Age = 0)
# D23 <- D23 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex) %>% mutate(Age = 0)
# 
# D23 <- D23 %>% select(-MigrationBehavior) %>% left_join(read.csv(meta) %>% mutate(MigrationBehavior = Mgtry, DeerID = as.character(AID)) %>% select(DeerID, MigrationBehavior), by = "DeerID", relationship = "many-to-many")
# D23 <- D23[!duplicated(D23$DeerID),]; D23
# 
# DECall <- rbind(D14, D15, D16, D17, D18, D19, D20, D21,  D22, D23) %>% mutate(Year = year(CapDate))
# DECall$CapEvent <- paste0("Dec", " ", DECall$Year, sep = "")
# 
# CAPall <- rbind(DECall, MARall)
# 
# 
# CAPall %>% filter(!is.na(scaledIFBFat)) %>% summarise(n = n())
# CAPall %>% filter(!is.na(scaledIFBFat)) %>% summarise(n = length(unique(DeerID)))
# 
# CAPall$MigrationBehavior <- ifelse(CAPall$MigrationBehavior == "Resident", "Short", CAPall$MigrationBehavior)
# 
# 
# 
# 
