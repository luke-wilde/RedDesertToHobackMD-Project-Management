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

#------------------------------------------#
#ATTN: need to check that each file is correct to reporting year!
#------------------------------------------#

#manually change these

#report year
year <- 2025
outfolder <- "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/Chapter33Permit/2025/Reporting/"
#cap data
meta <- "C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_11DECEMBER2025.csv"

#these should all load correctly, if save properly

#location of capture records
caplocs <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/0_MainDataSets/RDH_AllCaptureDataWithLocs_Rolling.csv")
#location of data
load("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_CleanData.RData")
# load age data
agedata <- read_csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/0_MainDataSets/RDH_KnownAgeByAID_Rolling.csv")
CBE1 <- read_xlsx(paste0("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/Chapter33Permit/2025/Wilde_RedDesertMuleDeer_Dec",year,"_CBE.xlsx") )
CBE2 <- read_xlsx(paste0("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/Chapter33Permit/2025/Wilde_RedDesertMuleDeer_March",year,"_CBE.xlsx") )
#location of capture data
Dec <- read_excel(paste("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/Annual Capture Data/RD2H_MD_Dec",year,"_CaptureData.xlsx",sep=""))
Mar <- read_excel(paste("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/Annual Capture Data/RD2H_MD_March",year,"_CaptureData.xlsx",sep=""))
#load mort causes
mortdata <- read_csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/0_MainDataSets/AllMalfunctionsAndMortCauses_Rolling.csv")

# Process data ####
#-----------------------------------#

#Mar
Mar <- Mar  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex, GeneralAgeClass, CTE)
Mar <- Mar %>% select(-MigrationBehavior) %>% left_join(read.csv(meta) %>% mutate(MigrationBehavior = Mgtry, DeerID = as.character(AID)) %>% select(DeerID, MigrationBehavior), by = "DeerID")


#Dec
Dec <- Dec %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, Sex, GeneralAgeClass, CBETag) %>% mutate(Age = 0)
Dec <- Dec %>% select(-MigrationBehavior) %>% left_join(read.csv(meta) %>% mutate(MigrationBehavior = Mgtry, DeerID = as.numeric(AID)) %>% select(DeerID, MigrationBehavior), by = "DeerID", relationship = "many-to-many")
Dec <- Dec[!duplicated(Dec$DeerID),]; Dec


currentMar <- Mar
currentDec <- Dec


## gps data prep ####
DeerID <- read.csv(meta) 
str(DeerID) #check structure

#reformat Dates
DeerID$StopDate <- as.POSIXct(DeerID$StopDate, format = "%m/%d/%Y", tz = "MST")
DeerID$StartDate <- as.POSIXct(DeerID$StartDate, format = "%m/%d/%Y", tz = "MST")
DeerID$CollarMalfDate <- as.POSIXct(DeerID$CollarMalfDate, format = "%m/%d/%Y", tz = "MST")

## Extract relevant info ####

# identify the first time a deer was monitored
first <- DeerID %>% arrange(AID, StartDate) %>% group_by(AID) %>% slice(1) %>% mutate(FirstYear = year(StartDate))
# identify the last time a deer was monitored
last <- DeerID %>% arrange(AID, desc(StartDate)) %>% group_by(AID) %>% slice(1)


# find which deer are dead, all those without an NA for stopdate
Dead <- last %>% filter(!is.na(StopDate)) %>% filter(is.na(CollarMalfDate)) %>% mutate(Year = year(StopDate), Month = month(StopDate)) 
Alive <- last[which(is.na(last$StopDate)),] %>% mutate(Year = year) 



## clip gps data ####
currentData <- rdh_data.sf %>% filter(POSIXct >= as.POSIXct(paste0(year,"-01-01 00:00:00"), tz = "MST") & POSIXct <= as.POSIXct(paste0(year+1,"-01-01 00:00:00"), tz = "MST")) 


#export for csv share
write.csv(currentData %>% st_drop_geometry(), paste0(outfolder, "RDH_Kauffman_33-937_GPSDataset_",year,".csv"))


#------------------------------#
# other report information ####

nrow(first)
length(unique(rdh_data.sf$id_yr))
nrow(caplocs)
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

#how many started the period
currentData %>% filter(Month == 3) %>% filter(AID %in% rbind(marchID, decID)$DeerID) %>% reframe(n = length(unique(AID))) 

#how many died
survivabilityYoungF <- Dead %>% filter(AID %in% c(marchID$DeerID, decID$DeerID) & Year == year) 
survivabilityYoungF %>% group_by(Month) %>% summarise(count = n()) %>% filter(Month > 2)

#causes
mortdata %>% filter(AID %in% survivabilityYoungF$AID)


marchID <- unique(currentMar %>% filter(GeneralAgeClass %in% c(age) & Sex %in% c(sex)) %>% select(DeerID, Sex))
decID <- unique(currentDec %>% filter(GeneralAgeClass %in% c(age) & Sex %in% c(sex) ) %>% select(DeerID, Sex))

currentData %>% filter(Month == 3) %>% filter(AID %in% rbind(marchID, decID)$DeerID) %>% reframe(n = length(unique(AID)))


survivabilityYoungM <- Dead %>% filter(AID %in% c(marchID$DeerID, decID$DeerID) & Year == year) 
survivabilityYoungM %>% group_by(Month) %>% summarise(count = n()) %>% filter(Month > 2)

#causes
mortdata %>% filter(AID %in% survivabilityYoungM$AID)


marchID <- unique(currentMar %>% filter(GeneralAgeClass %in% c(age3, age2) & Sex %in% c(sex2)) %>% select(DeerID))
decID <- unique(currentDec %>% filter(GeneralAgeClass %in% c(age3, age2) & Sex %in% c(sex2)) %>% select(DeerID))

currentData %>% filter(Month == 3) %>% filter(AID %in% rbind(marchID, decID)$DeerID) %>% reframe(n = length(unique(AID)))
survivabilityAdultF <- Dead %>% filter(AID %in% c(marchID$DeerID, decID$DeerID) & Year == year) 
survivabilityAdultF %>% group_by(Month) %>% summarise(count = n()) %>% filter(Month > 2)

#causes
mortdata %>% filter(AID %in% survivabilityAdultF$AID)



marchID <- unique(currentMar %>% filter(GeneralAgeClass %in% c(age2, age3) & Sex %in% c(sex)) %>% select(DeerID))
decID <- unique(currentDec %>% filter(GeneralAgeClass %in% c(age2, age3) & Sex %in% c(sex)) %>% select(DeerID))

currentData %>% filter(Month == 3) %>% filter(AID %in% rbind(marchID, decID)$DeerID) %>% reframe(n = length(unique(AID)))
survivabilityAdultM <- Dead %>% filter(AID %in% c(marchID$DeerID, decID$DeerID) & Year == year)
survivabilityAdultM %>% group_by(Month) %>% summarise(count = n()) %>% filter(Month > 2)

#causes
mortdata %>% filter(AID %in% survivabilityAdultM$AID)


DeerID %>% filter(CollarMalfDate > as.Date("2025-01-01")) %>% select(AID, TagColor, CollarMalfDate)


#OLD - do not use
#collarsMalf <- currentData %>% filter(AID %in% Alive$AID & AID %in% c(marchID$DeerID, decID$DeerID)) %>% arrange(AID, desc(POSIXct)) %>% group_by(AID) %>% slice(1) %>% select(AID, POSIXct) %>% mutate(malf = (POSIXct - as.POSIXct("2024-01-01 00:00:00", tz = "MST"))/(3600*24)) %>% arrange(malf); collarsMalf

collarsMalf <- DeerID %>% filter(!is.na(CollarMalfDate)); collarsMalf


#------------------------------#
# info for frequency list ####

#ready info
permit <- "33-937"
species <- "Odocoileus hemionus"
proj <- paste0("March 2014-December ", year)
freq <- "Yes"

data <- data.frame("Movebank_ID" = unique(currentData$AID))

#OLD WAY - do not use - CAPall is done below!

first <- caplocs %>% arrange(AID, CapDate) %>% group_by(AID) %>% slice(1)

data <- data %>% mutate(AID = as.character(Movebank_ID)) %>% left_join(first %>% select(CapDate, Sex, AID), by = "AID") %>% mutate(Date_Collar_Activated = 
CapDate) %>% select(AID, Movebank_ID, Sex, Date_Collar_Activated)

head(data)

data <- data %>% mutate(AID = as.integer(Movebank_ID)) %>% left_join(last %>% select(CllrSrN, Frequency, TagColor, TagNum, Mgtry, AID, ExpectedBatteryLife_AverageCase), by = "AID") %>% mutate(Collar_Serial_Number = CllrSrN, Transmitter_Frequency_MHz = Frequency, Eartag_Color_and_Number = paste(TagColor," ", TagNum, sep=""), Second_Eartag_Color_and_Number = "", Potential_Range_of_Movement_Miles = ifelse(Mgtry == "Long", "90-150", ifelse(Mgtry == "Medium", "50-90", ifelse(Mgtry == "Short", "<30", ifelse(Mgtry == "Resident", "<30",NA)))), Expected_Life_of_Transmitter = ExpectedBatteryLife_AverageCase) %>% select(AID, Movebank_ID, Sex, Date_Collar_Activated, Second_Eartag_Color_and_Number, Collar_Serial_Number, Transmitter_Frequency_MHz, Eartag_Color_and_Number, Potential_Range_of_Movement_Miles, Expected_Life_of_Transmitter)

data <- data %>% mutate(AID = as.integer(Movebank_ID)) %>% left_join(last %>% select(AID, StopDate), by = "AID") %>% mutate(Comments = ifelse(is.na(StopDate), "Alive", paste0("Died on ",StopDate," from unknown causes"))) %>% select(AID, Movebank_ID, Sex, Date_Collar_Activated, Second_Eartag_Color_and_Number, Collar_Serial_Number, Transmitter_Frequency_MHz, Eartag_Color_and_Number, Potential_Range_of_Movement_Miles, Expected_Life_of_Transmitter, Comments)

agedata <- agedata %>% arrange(AID, BirthYear) %>% group_by(AID) %>% slice(1)

data <- data %>% mutate(AID = as.integer(Movebank_ID)) %>% left_join(agedata %>% mutate(AID = as.numeric(AID)) %>% select(AID, BirthYear, Method), by = "AID") %>% mutate(Age = ifelse(is.na(BirthYear) | BirthYear == 9999, NA, 2026.5 - BirthYear)) %>% select(AID, Movebank_ID, Sex, Date_Collar_Activated, Second_Eartag_Color_and_Number, Collar_Serial_Number, Transmitter_Frequency_MHz, Eartag_Color_and_Number, Potential_Range_of_Movement_Miles, Expected_Life_of_Transmitter, Comments, Age, Method)

caploc <- rdh_data.sf %>% arrange(AID, POSIXct, UTM_E, UTM_N) %>% group_by(AID) %>% slice(1)

data <- data %>% mutate(AID = as.integer(Movebank_ID)) %>% left_join(caploc %>% select(AID, UTM_E, UTM_N), by = "AID") %>% mutate(Initial_Capture = paste(round(UTM_E,1), "E ", round(UTM_N,1), "N", sep="")) %>% select(AID, Movebank_ID, Sex, Date_Collar_Activated, Second_Eartag_Color_and_Number, Collar_Serial_Number, Transmitter_Frequency_MHz, Eartag_Color_and_Number, Potential_Range_of_Movement_Miles, Expected_Life_of_Transmitter, Comments, Age, Method, Initial_Capture)

data <- data %>% rename(Aging_Method = Method) %>% mutate(Count = 1:nrow(.), permit = "33-937", species = "Odocoileus hemionus", Nontelemetry_Collar_Color_and_Number = NA, Duration_of_Project = paste0("March 2014-December ", year),  Will_Frequencies_be_Reused = "Yes") %>% select(Count, permit, species, Movebank_ID, Initial_Capture, Sex, Age, Aging_Method, Collar_Serial_Number, Transmitter_Frequency_MHz, Date_Collar_Activated, Eartag_Color_and_Number, Second_Eartag_Color_and_Number, Nontelemetry_Collar_Color_and_Number, Potential_Range_of_Movement_Miles, Expected_Life_of_Transmitter, Duration_of_Project, Will_Frequencies_be_Reused, Comments)


#data %>% filter(Movebank_ID == 404)


#------------------------------#
# write to file ####

#------------------------------------#
#------------------------------------#
#ATTN: this is intermediate bc you go through and check for errors, as well as update the unknowns with those where we know what they died within 14 days of capture
#------------------------------------#
#------------------------------------#


write.csv(data, paste0(outfolder,"RDH_Kauffman_33-937_FreqList_Intermediate_",year,".csv"), row.names = F)

#-----------------------------#
# WYNDD ####


