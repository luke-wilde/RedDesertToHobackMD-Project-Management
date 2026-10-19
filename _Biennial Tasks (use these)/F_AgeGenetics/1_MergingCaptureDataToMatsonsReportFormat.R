# Code to correctly compile the age-by-cementum data and maintain an up-to-date database of parentage and relation
# Author: Luke Wilde
# 09.18.2024


#ATTN: This code DOES NOT need to be run each year. It was done in Sep. 2024 to produce the age dataset, which should be maintained manually thereafter, or appended to.


#----------------------------#
# Load libraries ####
library(readxl)
library(tidyverse)
library(lubridate)


#----------------------------#
# bring in all captures ####

#load IDs
setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/")

D14 <- read_excel("Annual Capture Data/RD2H_MD_Dec2014_CaptureData.xlsx")
D15 <- read_excel("Annual Capture Data/RD2H_MD_Dec2015_CaptureData.xlsx")
D16 <- read_excel("Annual Capture Data/RD2H_MD_Dec2016_CaptureData.xlsx")
D17 <- read_excel("Annual Capture Data/RD2H_MD_Dec2017_CaptureData.xlsx")
D18 <- read_excel("Annual Capture Data/RD2H_MD_Dec2018_CaptureData.xlsx")
D19 <- read_excel("Annual Capture Data/RD2H_MD_Dec2019_CaptureData.xlsx")
D20 <- read_excel("Annual Capture Data/RD2H_MD_Dec2020_CaptureData.xlsx")
D21 <- read_excel("Annual Capture Data/RD2H_MD_Dec2021_CaptureData.xlsx")
D22 <- read_excel("Annual Capture Data/RD2H_MD_Dec2022_CaptureData.xlsx")
D23 <- read_excel("Annual Capture Data/RD2H_MD_Dec2023_CaptureData.xlsx")

M14 <- read_excel("Annual Capture Data/RD2H_MD_March2014_CaptureData.xlsx")
M15 <- read_excel("Annual Capture Data/RD2H_MD_March2015_CaptureData.xlsx")
M16 <- read_excel("Annual Capture Data/RD2H_MD_March2016_CaptureData.xlsx")
M17 <- read_excel("Annual Capture Data/RD2H_MD_March2017_CaptureData.xlsx")
M18 <- read_excel("Annual Capture Data/RD2H_MD_March2018_CaptureData.xlsx")
M19 <- read_excel("Annual Capture Data/RD2H_MD_March2019_CaptureData.xlsx")
M20 <- read_excel("Annual Capture Data/RD2H_MD_March2020_CaptureData.xlsx")
M21 <- read_excel("Annual Capture Data/RD2H_MD_March2021_CaptureData.xlsx")
M22 <- read_excel("Annual Capture Data/RD2H_MD_March2022_CaptureData.xlsx")
M23 <- read_excel("Annual Capture Data/RD2H_MD_March2023 1 _CaptureData.xlsx")
M24 <- read_excel("Annual Capture Data/RD2H_MD_March2024_CaptureData.xlsx")

# Format datasets ####
#-----------------------------------#
#Mar
M14 <- M14  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
M15 <- M15  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
M16 <- M16  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
M17 <- M17  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
M18 <- M18  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
M19 <- M19  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
M20 <- M20  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
M21 <- M21  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
M22 <- M22  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
M23 <- M23  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
M24 <- M24  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)


MARall <- rbind(M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24) %>% mutate(Year = year(CapDate)) 
MARall$CapEvent <- paste0("Mar", " ", MARall$Year, sep = "")

#Dec
D14 <- D14  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
D15 <- D15  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
D16 <- D16  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
D17 <- D17  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
D18 <- D18  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
D19 <- D19  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
D20 <- D20  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
D21 <- D21  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate)
D22 <- D22  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate) %>% mutate(Age = 0)
D23 <- D23  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age, IncisiformCanine, `Lidocaine/bupivicaine_mL`, LabAccess, BirthDate) %>% mutate(Age = 0)


DECall <- rbind(D14, D15, D16, D17, D18, D19, D20, D21,  D22, D23) %>% mutate(Year = year(CapDate))
DECall$CapEvent <- paste0("Dec", " ", DECall$Year, sep = "")

CAPall <- rbind(DECall, MARall)


#----------------------------#
# identify all who have had tooth pulled ####
pullevents <- CAPall %>% filter(IncisiformCanine == TRUE) %>% select(CapDate, LabAccess, DeerID); head(pullevents)

#----------------------------#
# Bring in compiled lab report and merge ####

#update these as new results come in! The latest can be found at:
labResults <- read_excel("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Data/RDH_AgeByCementum_AllDeer_2014to2024_18SEPTEMBER2024.xlsx")

merge <- pullevents %>% left_join(labResults %>% filter(Method == "Cemementum Annulli") %>% rename(Date.on.submission = Date), by = "LabAccess"); head(merge)
#unique(merge$LabAccess)


#----------------------------#
# Look into why some are missing ####
# on September 18, 2024 LRW went through and manually checked for both of these types

#two deer where we know we pulled teeth, are missing: 547 (shown as sent but no result?) and 607 (not received)
missingRecords <- pullevents %>% anti_join(labResults %>% rename(Date.on.submission = Date), by = "LabAccess"); missingRecords

#
missingEntries <- labResults %>% anti_join(pullevents , by = "LabAccess"); missingEntries



#----------------------------#
# also retain known ages ####

knownAge <- CAPall %>% select(CapDate, LabAccess, DeerID) %>% right_join(labResults %>% filter(Method != "Cemementum Annulli") %>% rename(Date.on.submission = Date), by = "LabAccess"); head(knownAge)



#----------------------------#
# Prepare the database ####

RDH_AgeData <- rbind(merge, knownAge) %>% mutate(month = month(CapDate), year = year(CapDate), Age = as.numeric(Age), BirthYear = ifelse(month == 3, year - Age - 1,ifelse(month == 12, year - Age, NA))) %>% rename(LabAccess.At.Submission = LabAccess, AID = DeerID) %>% select(AID, BirthYear, LabAccess.At.Submission, Date.on.submission, Method); RDH_AgeData


missingEntries <- labResults %>% anti_join(RDH_AgeData %>% rename(LabAccess = LabAccess.At.Submission), by = "LabAccess"); missingEntries

RDH_AgeData <- RDH_AgeData %>% arrange(AID, BirthYear, Method) 
  
table(RDH_AgeData$Method)

#-------------------------------#
# Manual corrections!!! ####

#correct buck released
RDH_AgeData[which(RDH_AgeData$LabAccess.At.Submission == "20-buck"),"AID"] <- "456"
RDH_AgeData[which(RDH_AgeData$LabAccess.At.Submission == "20-buck"),"BirthYear"] <- 2019

#inconguencies
RDH_AgeData[50,"BirthYear" ] <- 2013
RDH_AgeData[115,"BirthYear" ] <- 2008

#----------------------------#
# grab manual entries by Anna ####
AOrtega_rec <- CAPall %>% filter(CapDate < mdy("10/20/2021")) %>% rename(AID = DeerID) %>% filter(!is.na(BirthDate)) %>% select(AID, CapEvent, CapDate, BirthDate) %>% group_by(AID) %>% arrange(AID, CapDate) %>% mutate(BirthYear = year(BirthDate)); head(AOrtega_rec)
  
# Check if all BirthYears within each AID are the same
check_birth_years <- AOrtega_rec %>%
  group_by(AID) %>%
  summarize(unique_birth_years = n_distinct(BirthYear)) %>%
  filter(unique_birth_years > 1)

# Display the result
check_birth_years 

#definitive list
AOrtega_rec <- AOrtega_rec %>% filter(!AID %in% check_birth_years$AID) %>% group_by(AID) %>% arrange(AID, CapDate)  %>% slice(1)

#----------------------------#
# ####
RDH_AgeData <- RDH_AgeData %>% left_join(AOrtega_rec %>% select(AID, BirthYear) %>% rename(Ortega_BirthYear = BirthYear), by = "AID")


#manually went through and looked for duplicates and those with disagreement to Ortega records
RDH_AgeData <- RDH_AgeData[-c(8,13,23,24,30,39,40,43,58,68,73,79,85,91,92,98,99,107,108,113,119,126,138,140,146,151,165,166,182,185,243,272,274,277,279,281,284,285,291,299,301,323,325,332,337,339,342,345,347,354,356,357,359,363,366,368,384,391,400),]

#write.csv(RDH_AgeData, "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Data/Known Age/RDH_KnownAgeByAID_2014to2024.csv", row.names = F)

#----------------------------#
# ####