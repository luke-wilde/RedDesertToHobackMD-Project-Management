# Code to monitor who died since last December captures and the number to replace in each strategy
#Luke Wilde
#Last update: 11/03/24

rm(list = ls())
# load packages####
library(tidyverse)
library(sf)
library(mapview)
library(stringr)
library(lubridate)
library(readxl)


load("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_CleanData.RData")
final.sf <- rdh_data.sf


#-----------------------#
# load master and gps data ####
master <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_11DECEMBER2025.csv")
master$StopDate <- as.Date(master$StopDate, format = "%m/%d/%Y")
alive <- master[which(is.na(master$StopDate)),]
malf <-  master[which(!master$CollarMalfDate == "" & master$Release.Date == ""),] 

#females only!
aliveF <- alive %>% filter(Fawn.Sex != "M") %>% filter(!AID %in% malf$AID)

#from FRS script
gps <- final.sf %>% filter(POSIXct > Sys.Date()-720 & POSIXct <= Sys.Date())

#-----------------------#
# load last three captures ####

#load("~/PhD_Chapters/Datasets/ChapterThree_Profiles/Data/CaptureData_BodyMorph_2014to2024.RData")


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
D24 <- read_excel("Annual Capture Data/RD2H_MD_Dec2024_CaptureData.xlsx")
D25 <- read_excel("Annual Capture Data/RD2H_MD_Dec2025_CaptureData.xlsx")

M14 <- read_excel("Annual Capture Data/RD2H_MD_March2014_CaptureData.xlsx")
M15 <- read_excel("Annual Capture Data/RD2H_MD_March2015_CaptureData.xlsx")
M16 <- read_excel("Annual Capture Data/RD2H_MD_March2016_CaptureData.xlsx")
M17 <- read_excel("Annual Capture Data/RD2H_MD_March2017_CaptureData.xlsx")
M18 <- read_excel("Annual Capture Data/RD2H_MD_March2018_CaptureData.xlsx")
M19 <- read_excel("Annual Capture Data/RD2H_MD_March2019_CaptureData.xlsx")
M20 <- read_excel("Annual Capture Data/RD2H_MD_March2020_CaptureData.xlsx")
M21 <- read_excel("Annual Capture Data/RD2H_MD_March2021_CaptureData.xlsx")
M22 <- read_excel("Annual Capture Data/RD2H_MD_March2022_CaptureData.xlsx")
M23 <- read_excel("Annual Capture Data/RD2H_MD_March2023_CaptureData.xlsx")
M24 <- read_excel("Annual Capture Data/RD2H_MD_March2024_CaptureData.xlsx")
M25 <- read_excel("Annual Capture Data/RD2H_MD_March2025_CaptureData.xlsx")

# Format datasets ##


#Mar
M14 <- M14 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M15 <- M15 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M16 <- M16 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M17 <- M17 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M18 <- M18 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M19 <- M19 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M20 <- M20 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M21 <- M21 %>% filter(GeneralAgeClass %in% c("Adult","Fawn","Yearling")) %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M22 <- M22 %>% filter(GeneralAgeClass %in% c("Adult","Fawn","Yearling")) %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M23 <- M23 %>% filter(GeneralAgeClass %in% c("Adult","Fawn","Yearling")) %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M24 <- M24 %>% filter(GeneralAgeClass %in% c("Adult","Fawn","Yearling")) %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M25 <- M25 %>% filter(GeneralAgeClass %in% c("Adult","Fawn","Yearling")) %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# x <- data.frame("DeerID" = as.character(c(601, 603, 605,606,607,608,609,610,611,612,613,614,615,616,617,618, 619,620,621,622,623,624,625,626,627,628,629,630,631)), "MigrationStrategy2" = c("Long","Long", "Short","Medium","Medium","Medium","N/A","Short","Long","N/A","N/A","N/A","N/A","Short","Long","N/A","Medium","Long","Medium", "Medium","Long", "Long", "Long","Medium", "Long","Medium","Medium","Long","Long")
# )

#tmp <- M22 %>% left_join(x, by = "DeerID")
#M22$MigrationBehavior <- ifelse(tmp$MigrationBehavior =="N/A", tmp$MigrationStrategy2,tmp$MigrationBehavior)

MARall <- rbind(M14, M15, M16, M17, M18, M19, M20, M21, M22, M23,M24,M25) %>% mutate(Year = year(CapDate))
MARall$CapEvent <- paste0("Mar", " ", MARall$Year, sep = "")

#Dec
D14 <- D14 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
D15 <- D15 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
D16 <- D16 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
D17 <- D17 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
D18 <- D18 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
D19 <- D19 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
D20 <- D20 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
D21 <- D21 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
D22 <- D22 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age) %>% mutate(Age = 0)
D23 <- D23 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age) %>% mutate(Age = 0)
D24 <- D24 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age) %>% mutate(Age = 0)
D25 <- D25 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age) %>% mutate(Age = 0)

DECall <- rbind(D14, D15, D16, D17, D18, D19, D20, D21,  D22,D23,D24,D25) %>% mutate(Year = year(CapDate))
DECall$CapEvent <- paste0("Dec", " ", DECall$Year, sep = "")

CAPall <- rbind(DECall, MARall)
unique(CAPall$CapEvent)

CAPall %>% dplyr::filter(!is.na(scaledIFBFat)) %>% dplyr::summarise(n = n())
CAPall %>% dplyr::filter(!is.na(scaledIFBFat)) %>% dplyr::summarise(n = length(unique(DeerID)))

#-----------------------#
# ID longitudinal ####

rec <- CAPall %>% mutate(DeerID = as.numeric(DeerID)) %>% filter(!is.na(DeerID)) %>% dplyr::rename(AID = DeerID) %>% dplyr::mutate(AID = as.numeric(AID)) %>% dplyr::filter(!is.na(AID)) %>% mutate( mY = paste(month(CapDate),"_", year(CapDate), sep = "")) %>% dplyr::select(AID, mY)

recent <- rec %>% filter(mY %in% c("12_2024","3_2025","12_2025"))
mostrecent <- rec %>% filter(mY %in% c("3_2025","12_2025"))

record <- recent %>% dplyr::group_by(AID) %>% dplyr::summarise(n = length(unique(mY))) %>% dplyr::filter(AID %in% aliveF$AID) %>% dplyr::arrange(desc(n))

#females only
available <- aliveF %>% dplyr::select(AID, Mgtry) %>% left_join(record, by = "AID") %>% drop_na(AID) %>% arrange(Mgtry, desc(n), AID) %>% filter(Mgtry != "TBD")

available

#make sure they have been caught at least once in the last 3 capture events

priority <- available %>% filter(AID %in% c(mostrecent$AID))

longitudinal <- priority %>% filter(n > 1) #those with >1 and recent captures
longitudinal %>% dplyr::group_by(Mgtry) %>% dplyr::summarise(n = n())

#add ons
addons <- priority %>% filter(!AID %in% longitudinal$AID & AID %in% (rec %>% filter(mY == "12_2025") %>% select(AID))$AID) %>% filter(Mgtry != "TBD")
addons %>% dplyr::group_by(Mgtry) %>% dplyr::summarise(n = n())

#potentials
potentials <- priority %>% filter(!AID %in% longitudinal$AID & AID %in% (rec %>% filter(mY == "3_2025") %>% select(AID))$AID)
potentials %>% dplyr::group_by(Mgtry) %>% dplyr::summarise(n = n())


#-----------------------#
# prepare lists - additions to catch ####
BrokenCollars <- rbind(alive,malf) %>% filter(AID %in% c(710, 424, 460,515,529,624,710,529, 737, 747)) %>% select(AID, Mgtry, Fawn.Sex) %>% filter(!AID %in% mostrecent$AID)
LowVolts <- rbind(alive,malf) %>% filter(AID %in% c(733,741,742,682,626,701,360,752,446)) %>% select(AID, Mgtry, Fawn.Sex) #were none in November 2024
BadVHF <- rbind(alive,malf) %>% filter(AID %in% c(724)) %>% select(AID, Mgtry, Fawn.Sex)  

longitudinal <- longitudinal %>% filter(!AID %in% c(BrokenCollars$AID,BadVHF$AID)) 
addons <- addons %>% filter(!AID %in% BrokenCollars$AID)
potentials <- potentials %>% filter(!AID %in% BrokenCollars$AID)


REPLACE <- alive %>% filter(AID %in% c(723,622,669,735,724,717,713,761,447,708,682,742,741,722,707,727,626,360,733,628,748,744,973,541,974,1056,446,725,811,972,970,701,752,904)) %>% select(AID, Mgtry, Fawn.Sex)
REFIT_DoNotDrop <-  alive %>% filter(AID %in% c(855, 859, 765, 781 )) %>% select(AID, Mgtry, Fawn.Sex) 
ADULTMALE <- alive %>% filter(AID %in% c(972,974)) %>% select(AID, Mgtry, Fawn.Sex) 

longcap <- rbind(longitudinal,addons,priority) %>% mutate(Fawn.Sex = "") %>% select(AID, Mgtry, Fawn.Sex)

CapThese <- rbind(longcap, BrokenCollars, REFIT_DoNotDrop, REPLACE, ADULTMALE, LowVolts, BadVHF)  
CapThese <- CapThese %>% distinct(AID, Mgtry, Fawn.Sex)


CapTheseF <- CapThese %>% arrange(AID) %>% dplyr::filter(!AID %in% BrokenCollars$AID & !AID %in% LowVolts$AID & !Fawn.Sex %in% c("M"))

CapTheseF %>% dplyr::distinct(AID, Mgtry) %>% filter(!AID %in% c(malf$AID)) %>% dplyr::group_by(Mgtry) %>% dplyr::summarise(n = n())  

#================================ Needs =========================================#
needLong <- 0
needMedium <- 0
needShort <- 3
#================================================================================#

#-----------------------#
# Choose new animals ####

gps1 <- gps %>% filter(AID %in% aliveF$AID) %>%   mutate(class = ifelse(AID %in% CapTheseF$AID, "Cap'd", "Option"), AID = as.character(AID)) %>% filter(Month %in% c(7,8,9)) %>% group_by(AID, Date)  %>% sample_n(1) %>% select(Mgtry, AID, class, Date)
  
x <- gps1 %>% filter(Mgtry  %in% c("Long") & class == "Cap'd")
xx <- gps1 %>% filter(Mgtry  %in% c("Long") & class == "Option")

#View(gps %>% filter(class == "Option") %>%  dplyr::select(AID, Mgtry) %>% distinct(AID, Mgtry) %>% arrange(Mgtry, AID))
mapview(xx, col.regions = "grey") +
mapview(x, zcol = "AID") #& gps1$AID == "702"

addLong <- c(000)
addMedium <- c(000)
addShort <-  c(280, 742,682)

additions <- c(addLong, addMedium, addShort)

#check if any are already set to be caught
CapTheseF %>% filter(AID %in% additions)

CapturePlan <- data.frame(AID = c(CapThese$AID, additions)) %>% left_join(rbind(alive,malf) %>% dplyr::select(AID, Mgtry, TagColor, TagNum, CllrSrN, Frequency), by = c("AID"), relationship = "many-to-many") %>% distinct(AID, Mgtry, TagColor, TagNum, CllrSrN, Frequency)

CapturePlan %>% arrange(AID)

#-------------------------------------#
#Prep data sheet####
#DNA collected info
dna_filed <- read_xlsx(path = "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/0_MainDataSets/RDH_GeneticsAlleleData_Rolling.xlsx",  "All")
dna_filed <- dna_filed %>% select(AID) %>% mutate(AID = as.numeric(AID))

#TEMPORARY CODE TO fix (11.29.2024) -- 331 and 514 are mixed up, and 659 is the offspring of 514 only
dna_filed <- dna_filed %>% filter(!AID %in% c(514,659))

# teeth 
allcaptures <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/0_MainDataSets/RDH_KnownAgeByAID_Rolling.csv")
haveTooth <- allcaptures %>% select(AID) %>% mutate(AID = as.numeric(AID))

CapturePlan <- CapturePlan %>% mutate(Replace = ifelse(AID %in% REPLACE$AID & !AID %in% BrokenCollars$AID & !AID %in% LowVolts$AID & !AID %in% BadVHF$AID, "Replace", ifelse(AID %in% BrokenCollars$AID & !AID %in% LowVolts$AID & !AID %in% BadVHF$AID, "Broken", ifelse(AID %in% LowVolts$AID & !AID %in% BadVHF$AID, "Low Batt", ifelse(AID %in% BadVHF$AID, "Check VHF", "")))), Refit = ifelse(AID %in% REFIT_DoNotDrop$AID, "Refit/DoNotDrop", ifelse(AID %in% ADULTMALE$AID, "Add Drop Device", "")), CollectPurp = ifelse(AID %in% dna_filed$AID, "", "Collect DNA"), PullTooth = ifelse(!AID %in% haveTooth$AID, "CollectTooth", "")) %>% arrange(AID) %>% filter(AID > 0)

table(CapturePlan$PullTooth)

CapturePlan %>% dplyr::filter(!AID %in% BrokenCollars$AID) %>% dplyr::filter(!AID %in% alive[alive$Fawn.Sex == "M", "AID"]) %>% dplyr::group_by(Mgtry) %>% dplyr::summarise(n = n()) 
CapturePlan <- CapturePlan %>% mutate(Date = character(n()), Load = character(n()), Position = character(n()))

CapturePlan %>% arrange(Replace, AID)


#------------------------#
#------------------------#
## OPTIONAL -- ADDING OTHER COLLAR TYPES ####

CapturePlan <- CapturePlan %>% mutate(Replace = case_when(AID %in% c(665,970,717) ~ "*Iridium*", .default = Replace))

#------------------------#
#------------------------#


#-----------------------------#
# If needing to shorten, for second round...
# keep <- c(424, 460, 515, 624, 737, 747, 404, 446, 541, 610, 622, 669, 681, 682, 701, 712, 713, 717, 721 ,723, 724, 742, 752,761, 782, 811, 854, 858, 903, 904, 951 ,970 ,973)
# 
# CapturePlan <- CapturePlan %>% filter(AID %in% keep)
# 
# CapturePlan <- CapturePlan %>% filter(Replace !="" | AID %in% DoesWithFawns$AID)
#-----------------------------#


write.csv(CapturePlan %>% arrange(Frequency), "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CapturePlans/2026/March/RDH_CapList_March2026.csv", row.names = F)
  




#-----------------------#
# identify animals with TBD that are good candidates for samples ####
# 
# tbd <- master %>% filter(Mgtry == "TBD") %>% select(AID)
# tbd
# 
# 
# 
# setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/")
# D14 <- read_excel("Annual Capture Data/RD2H_MD_Dec2014_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# D15 <- read_excel("Annual Capture Data/RD2H_MD_Dec2015_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# D16 <- read_excel("Annual Capture Data/RD2H_MD_Dec2016_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# D17 <- read_excel("Annual Capture Data/RD2H_MD_Dec2017_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# D18 <- read_excel("Annual Capture Data/RD2H_MD_Dec2018_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# D19 <- read_excel("Annual Capture Data/RD2H_MD_Dec2019_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# D20 <- read_excel("Annual Capture Data/RD2H_MD_Dec2020_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# D21 <- read_excel("Annual Capture Data/RD2H_MD_Dec2021_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# D22 <- read_excel("Annual Capture Data/RD2H_MD_Dec2022_CaptureData.xlsx") %>% select(DeerID) %>% rename(AID = DeerID) %>% mutate(StagingArea = NA)
# D23 <- read_excel("Annual Capture Data/RD2H_MD_Dec2023_CaptureData.xlsx") %>% select(DeerID) %>% rename(AID = DeerID) %>% mutate(StagingArea = NA)
# D24 <- read_excel("Annual Capture Data/RD2H_MD_Dec2024_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# 
# M14 <- read_excel("Annual Capture Data/RD2H_MD_March2014_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# M15 <- read_excel("Annual Capture Data/RD2H_MD_March2015_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# M16 <- read_excel("Annual Capture Data/RD2H_MD_March2016_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# M17 <- read_excel("Annual Capture Data/RD2H_MD_March2017_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# M18 <- read_excel("Annual Capture Data/RD2H_MD_March2018_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# M19 <- read_excel("Annual Capture Data/RD2H_MD_March2019_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# M20 <- read_excel("Annual Capture Data/RD2H_MD_March2020_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# M21 <- read_excel("Annual Capture Data/RD2H_MD_March2021_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# M22 <- read_excel("Annual Capture Data/RD2H_MD_March2022_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# M23 <- read_excel("Annual Capture Data/RD2H_MD_March2023_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# M24 <- read_excel("Annual Capture Data/RD2H_MD_March2024_CaptureData.xlsx") %>% select(DeerID, StagingArea) %>% rename(AID = DeerID)
# 
# MARall <- rbind(M14, M15, M16, M17, M18, M19, M20, M21, M22, M23,M24)
# DECall <- rbind(D14, D15, D16, D17, D18, D19, D20, D21,  D22,D23,D24) 
# 
# CAPall <- rbind(DECall, MARall)
# 
# 
# where <- CAPall %>% filter(AID %in% tbd$AID & AID %in% aliveF$AID)
# where %>% group_by(StagingArea) %>% reframe(n = n())
# where %>% filter(AID %in% CapturePlan$AID) %>% group_by(StagingArea) %>% reframe(n = n())
# where %>% arrange(StagingArea) %>% as.data.frame()
# 



#-----------------------#
# ####
#-----------------------#
# ####
#-----------------------#
# ####


