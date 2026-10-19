# Code to monitor who died since last December captures and the number to replace in each strategy
#Luke Wilde
#03/03/24

# load packages####
library(tidyverse)
library(sf)
library(mapview)
library(stringr)
library(lubridate)


load("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_CleanData.RData")
final.sf <- rdh_data.sf


#-----------------------#
# load master and gps data ####
master <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_10DECEMBER2023.csv")
master$StopDate <- as.Date(master$StopDate, format = "%m/%d/%Y")
alive <- master[which(is.na(master$StopDate)),]

#females only!
aliveF <- alive %>% filter(Fawn.Sex != "M")

#from FRS script
gps <- final.sf %>% filter(POSIXct > as.POSIXct("2023-07-10 01:00:00.00") & POSIXct <= as.POSIXct("2023-08-20 01:00:00.00"))

#-----------------------#
# load last three captures ####
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

# x <- data.frame("DeerID" = as.character(c(601, 603, 605,606,607,608,609,610,611,612,613,614,615,616,617,618, 619,620,621,622,623,624,625,626,627,628,629,630,631)), "MigrationStrategy2" = c("Long","Long", "Short","Medium","Medium","Medium","N/A","Short","Long","N/A","N/A","N/A","N/A","Short","Long","N/A","Medium","Long","Medium", "Medium","Long", "Long", "Long","Medium", "Long","Medium","Medium","Long","Long")
# )

#tmp <- M22 %>% left_join(x, by = "DeerID")
#M22$MigrationBehavior <- ifelse(tmp$MigrationBehavior =="N/A", tmp$MigrationStrategy2,tmp$MigrationBehavior)

MARall <- rbind(M14, M15, M16, M17, M18, M19, M20, M21, M22, M23) %>% mutate(Year = year(CapDate))
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

DECall <- rbind(D14, D15, D16, D17, D18, D19, D20, D21,  D22) %>% mutate(Year = year(CapDate))
DECall$CapEvent <- paste0("Dec", " ", DECall$Year, sep = "")

CAPall <- rbind(DECall, MARall)


CAPall %>% dplyr::filter(!is.na(scaledIFBFat)) %>% dplyr::summarise(n = n())
CAPall %>% dplyr::filter(!is.na(scaledIFBFat)) %>% dplyr::summarise(n = length(unique(DeerID)))

#-----------------------#
# ID longitudinal ####
library(dplyr)
rec <- CAPall %>% mutate(DeerID = as.numeric(DeerID)) %>% filter(!is.na(DeerID)) %>% dplyr::rename(AID = DeerID) %>% dplyr::mutate(AID = as.numeric(AID)) %>% dplyr::filter(!is.na(AID)) %>% mutate( mY = paste(month(CapDate),"_", year(CapDate), sep = "")) %>% dplyr::select(AID, mY)

record <- rec %>% dplyr::group_by(AID) %>% dplyr::summarise(n = length(unique(mY))) %>% dplyr::filter(AID %in% alive$AID) %>% dplyr::arrange(desc(n))

#females only
available <- aliveF %>% dplyr::select(AID, Mgtry) %>% left_join(record, by = "AID") %>% drop_na(AID) %>% arrange(Mgtry, desc(n), AID)

View(available)

#make sure they have been caught at least once in the last 3 capture events
recent <- rec %>% filter(mY %in% c("12_2022", "3_2023", "12_2023"))
mostrecent <- rec %>% filter(mY %in% c("3_2023", "12_2023"))
priority <- available %>% filter(AID %in% c(recent$AID))

longitudinal <- priority[c(1:11,36:45,66:80),] #those with >1 and recent captures

longitudinal %>% dplyr::group_by(Mgtry) %>% dplyr::summarise(n = n())


#-----------------------#
# prepare lists - additions to catch ####
BrokenCollars <- alive %>% filter(AID %in% c(545, 710, 280, 529, 424, 460, 515)) %>% select(AID, Mgtry, Fawn.Sex)

longitudinal <- longitudinal %>% filter(!AID %in% BrokenCollars$AID)

LowVolts <- alive %>% filter(AID %in% c(711, 712, 737)) %>% select(AID, Mgtry, Fawn.Sex)
REPLACE <- alive %>% filter(AID %in% c(770, 771, 852, 777, 778, 714, 752, 622, 545,  710, 280, 529,424,721, 743, 537, 463, 457, 782, 738, 659, 762, 706, 280, 424, 529, 545, 622, 463, 659, 710, 537, 771, 712, 711, 770, 457, 852, 762)) %>% select(AID, Mgtry, Fawn.Sex)
REFIT_DoNotDrop <-  alive %>% filter(AID %in% c(858)) %>% select(AID, Mgtry, Fawn.Sex) 
ADULTMALE <- alive %>% filter(AID %in% c(683, 655, 657, 760, 765, 783, 784, 781)) %>% select(AID, Mgtry, Fawn.Sex) 

longcap <- longitudinal %>% mutate(Fawn.Sex = "") %>% select(AID, Mgtry, Fawn.Sex)

CapThese <- rbind(longcap, LowVolts, BrokenCollars, REFIT_DoNotDrop, REPLACE, ADULTMALE)  
CapThese <- CapThese %>% distinct(AID, Mgtry, Fawn.Sex)


CapTheseF <- CapThese %>% arrange(AID) %>% dplyr::filter(!AID %in% BrokenCollars$AID & !Fawn.Sex %in% c("M"))

CapTheseF %>% dplyr::distinct(AID, Mgtry) %>% dplyr::group_by(Mgtry) %>% dplyr::summarise(n = n())  

#define needs
needLong <- 6
needMedium <- 3
needShort <- 4

#-----------------------#
# Choose new animals ####

gps1 <- gps %>% filter(AID %in% aliveF$AID) %>%   mutate(class = ifelse(AID %in% CapTheseF$AID, "Cap'd", "Option"), AID = as.character(AID)) %>% group_by(AID, Date) %>% sample_n(1)
  
#View(gps %>% filter(class == "Option") %>%  dplyr::select(AID, Mgtry) %>% distinct(AID, Mgtry) %>% arrange(Mgtry, AID))
mapview(gps1[which(gps1$Mgtry == "Short" & gps1$class == "Cap'd"),], col.regions = "grey") +
mapview(gps1[which(gps1$Mgtry == "Short" & gps1$class == "Option"),], zcol = "AID")

addLong <- c(718, 630, 621, 603, 631, 736)
addMedium <- c(725, 669, 626)
addShort <- c(729, 768, 730, 779)

additions <- c(addLong, addMedium, addShort)

CapturePlan <- data.frame(AID = c(CapThese$AID, additions)) %>% left_join(alive %>% dplyr::select(AID, Mgtry, TagColor, TagNum, CllrSrN, Frequency), by = c("AID"))

#-------------------------------------#
#Prep data sheet####
#DNA collected info
dna_filed <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Data/RDH_VGLGeneticMarkers_2020to2024.csv")
dna_filed <- dna_filed %>% select(AID) %>% mutate(AID = as.numeric(AID))


# teeth 
allcaptures <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Data/RDH_AgeByCementum_AllDeer_2014to2023_19FEBRUARY2024.csv")
NeedTooth <- allcaptures %>% filter(is.na(BirthYear)) %>% mutate(AID = DeerID) %>% select(AID)

CapturePlan <- CapturePlan %>% mutate(Replace = ifelse(AID %in% REPLACE$AID & !AID %in% BrokenCollars$AID & !AID %in% LowVolts$AID, "Replace", ifelse(AID %in% BrokenCollars$AID, "Replace/Broken", ifelse(AID %in% LowVolts$AID, "Replace/LowBatt", ""))), Refit = ifelse(AID %in% REFIT_DoNotDrop$AID, "Refit/DoNotDrop", ifelse(AID %in% ADULTMALE$AID, "Add Drop Device", "")), CollectPurp = ifelse(AID %in% dna_filed$AID, "", "Collect DNA"), PullTooth = ifelse(AID %in% NeedTooth$AID, "CollectTooth", "")) %>% arrange(AID) %>% filter(AID > 0)

CapturePlan %>% dplyr::filter(!AID %in% BrokenCollars$AID) %>% dplyr::filter(!AID %in% alive[alive$Fawn.Sex == "M", "AID"]) %>% dplyr::group_by(Mgtry) %>% dplyr::summarise(n = n()) 

write.csv(CapturePlan, "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CapturePlans/2024/March/RDH_March2024_CapList_v3.csv", row.names = F)
  




#-----------------------#
# ####
#-----------------------#
# ####
#-----------------------#
# ####
#-----------------------#
# ####
#-----------------------#
# ####






# #load in data
# #load master list for survival####
# master <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_21OCTOBER2023.csv")
# head(master)
# 
# master$StopDate <- as.Date(master$StopDate, format = "%m/%d/%Y")
# alive <- master[which(is.na(master$StopDate)),]
# 
# #females only!
# #alive <- alive %>% filter(Fawn.Sex != "M")
# 
# #load capture data ####
# #---------------------------------#
# setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/")
# D14 <- read_excel("Annual Capture Data/RD2H_MD_Dec2014_CaptureData.xlsx")
# D15 <- read_excel("Annual Capture Data/RD2H_MD_Dec2015_CaptureData.xlsx")
# D16 <- read_excel("Annual Capture Data/RD2H_MD_Dec2016_CaptureData.xlsx")
# D17 <- read_excel("Annual Capture Data/RD2H_MD_Dec2017_CaptureData.xlsx")
# D18 <- read_excel("Annual Capture Data/RD2H_MD_Dec2018_CaptureData.xlsx")
# D19 <- read_excel("Annual Capture Data/RD2H_MD_Dec2019_CaptureData.xlsx")
# D20 <- read_excel("Annual Capture Data/RD2H_MD_Dec2020_CaptureData.xlsx")
# D21 <- read_excel("Annual Capture Data/RD2H_MD_Dec2021_CaptureData.xlsx")
# D22 <- read_excel("Annual Capture Data/RD2H_MD_Dec2022_CaptureData.xlsx")
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
# # Format datasets ##
# 
# 
# #Mar
# M14 <- M14 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# M15 <- M15 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# M16 <- M16 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# M17 <- M17 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# M18 <- M18 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# M19 <- M19 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# M20 <- M20 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# M21 <- M21 %>% filter(GeneralAgeClass %in% c("Adult","Fawn","Yearling")) %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# M22 <- M22 %>% filter(GeneralAgeClass %in% c("Adult","Fawn","Yearling")) %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# M23 <- M23 %>% filter(GeneralAgeClass %in% c("Adult","Fawn","Yearling")) %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# 
# # x <- data.frame("DeerID" = as.character(c(601, 603, 605,606,607,608,609,610,611,612,613,614,615,616,617,618, 619,620,621,622,623,624,625,626,627,628,629,630,631)), "MigrationStrategy2" = c("Long","Long", "Short","Medium","Medium","Medium","N/A","Short","Long","N/A","N/A","N/A","N/A","Short","Long","N/A","Medium","Long","Medium", "Medium","Long", "Long", "Long","Medium", "Long","Medium","Medium","Long","Long")
# # )
# 
# #tmp <- M22 %>% left_join(x, by = "DeerID")
# #M22$MigrationBehavior <- ifelse(tmp$MigrationBehavior =="N/A", tmp$MigrationStrategy2,tmp$MigrationBehavior)              
# 
# MARall <- rbind(M14, M15, M16, M17, M18, M19, M20, M21, M22, M23) %>% mutate(Year = year(CapDate)) 
# MARall$CapEvent <- paste0("Mar", " ", MARall$Year, sep = "")
# 
# #Dec
# D14 <- D14 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# D15 <- D15 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# D16 <- D16 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# D17 <- D17 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# D18 <- D18 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# D19 <- D19 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# D20 <- D20 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# D21 <- D21 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
# D22 <- D22 %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age) %>% mutate(Age = 0)
# 
# DECall <- rbind(D14, D15, D16, D17, D18, D19, D20, D21,  D22) %>% mutate(Year = year(CapDate))
# DECall$CapEvent <- paste0("Dec", " ", DECall$Year, sep = "")
# 
# CAPall <- rbind(DECall, MARall)
# 
# 
# CAPall %>% filter(!is.na(scaledIFBFat)) %>% summarise(n = n())
# CAPall %>% filter(!is.na(scaledIFBFat)) %>% summarise(n = length(unique(DeerID)))
# 
# rec <- CAPall %>% rename(AID = DeerID) %>% mutate(AID = as.numeric(AID), mY = paste(month(CapDate),"_", year(CapDate), sep = "")) %>% dplyr::select(AID, mY)
# 
# record <- rec %>% group_by(AID) %>% summarise(n = length(unique(mY))) %>% filter(AID %in% alive$AID) %>% arrange(desc(n))
# 
# #females only
# available <- alive %>% filter(Fawn.Sex != "M") %>% dplyr::select(AID, Mgtry) %>% left_join(record, by = "AID") %>% arrange(Mgtry, desc(n), AID)
# 
# View(available)
# 
# recent <- rec %>% filter(mY %in% c("3_2022", "12_2021", "3_2023", "12_2022"))
# 
# priority <- available %>% filter(AID %in% c(rec$AID))
# 
# longitudinal <- priority[c(1:16,50:61,92:109),] #those with >1 and recent captures
# 
# longitudinal %>% group_by(Mgtry) %>% summarise(n = n())
# 
# #do not catch
# #NotAvailForLongitudinal <- c(284,622,610,418,431, 340, 530)
# 
# #need to catch these:
# REPLACECOLLARS <- alive %>% filter(AID %in% c(702, 704,710, 659, 445,446, 457,463,537,542,683,743,280,424,529,545,622,242,447,753,754, 759, 760, 766,  769, 768, 765)) %>% select(AID, Mgtry, Fawn.Sex)
# REMOVECOLLARS <- NA #alive %>% filter(CllrSrN %in% c("706105A","715847A","728186A","728204A","728214A")) %>% select(AID, Mgtry)
# 
# REPLACE_ExtraCap <- REPLACECOLLARS[which(!REPLACECOLLARS$AID %in% longitudinal$AID),]
# 
# xx <- rbind(longitudinal[,1:2], REPLACE_ExtraCap[,1:2])
# 
# xx %>% group_by(Mgtry) %>% summarise(count = 20 - n(), n = n())
# 
# #Dec 2023 needs: 2 Long, 6 Medium, 0 Short to add
# 
# pre_list <- xx
# 
# avail <- master[which(is.na(master$StopDate)),]
# 
# males2refit <- master[which(is.na(master$StopDate)),] %>% filter(AID %in%  c(655, 657, 683, 753, 754, 759, 760, 766, 769, 768, 765)) %>% select(AID, Mgtry)
# 
# all <- rbind(pre_list, males2refit)
# 
# pre_list <- all %>% left_join(avail %>% dplyr::select(AID, Mgtry, TagColor, TagNum, CllrSrN, Frequency), by = c("AID", "Mgtry"))
# 
# pre_list
# 
# addLong <- c(601, 617)
# addMed <- c(669, 680, 628, 606, 607, 621)
# 
# additions <- data.frame(AID = c(addLong, addMed))
# add_list <- additions %>% left_join(avail %>% dplyr::select(AID, Mgtry, TagColor, TagNum, CllrSrN, Frequency), by = c("AID"))
# 
# finlist <- rbind(pre_list, add_list)
# 
# master %>% filter(is.na(StopDate) | StopDate >= "2023-03-10") %>% distinct(CllrSrN)
# 
# # #load in capture data from last ####
# lastcap <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CapturePlans/2023/March/RDH_Mar2023_CaptureList.csv")
# lastcap2 <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CapturePlans/2022/December/RDH_Dec2022_CaptureList_v1.csv"); head(lastcap)
# 
# #check that we really got DNA
# #Dec <- readxl::read_xlsx("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/Annual Capture Data/RD2H_MD_Dec2022_CaptureData.xlsx", sheet = 1)
# #DNAdec <- lastcap %>% filter(CollectDNA == "Yes") %>% select(AID)
# #dnaStored <- DNAdec %>% filter(AID %in% unique(Dec$DeerID))
# 
# 
# 
# #DNA collected info
# dna_filed <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Data/RDH_VGLGeneticMarkers_2020to2023.csv")
# dna_filed <- dna_filed %>% select(AID) %>% mutate(AID = as.numeric(AID))
# #dna_filedOrstored <- rbind(dna_filed, dnaStored)
# 
# # teeth
# allcaptures <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Data/RDH_AgeInformationByID_2014to2023.csv")
# NeedTooth <- allcaptures %>% filter(is.na(BirthYear)) %>% select(AID)
# 
# 
# finlist <- finlist %>% mutate(PurpleTop = ifelse(AID %in% dna_filed$AID,"","CollectDNA"), PullTooth = ifelse(AID %in% NeedTooth, "PullTooth", ifelse(!AID %in% allcaptures$AID, "PullTooth", "")), ReplaceCollar = ifelse(AID %in% REPLACECOLLARS$AID, "ReplaceCollar", ""), RefitCollar = ifelse(AID %in% c(655,657,683,768,765, 753, 754, 759,760,765,766,769), "Refit", ""))
# 
# finlist <- finlist %>%  mutate(PullTooth = ifelse(AID > 749, "", PullTooth))
# finlist
# 
# 
# 
# #check!
# REPLACECOLLARS %>% filter(!AID %in% finlist$AID)
# males2refit %>% filter(!AID %in% finlist$AID)
# 
# #---------------------------------#
# #begin with longitudinal ####
# Longitudinal <- lastcap %>% filter(AID %in% lastcap2$AID) %>% select(AID, Mgtry) %>% filter(AID %in% alive$AID)
# lastcap_AID <- as.data.frame(lastcap %>% filter(AID %in% master[which(is.na(master$StopDate)),"AID"]))[,"AID"]
# 
# 
# #load gps data####
# load("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_CleanAllData.RData")
# finalshape <- findata
# 
# 
# #Reproject shapefile into desired coordinate system####
# # finalshape_sf <- st_transform(finalshape, crs = "+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# # finalshape_sf <- finalshape_sf[-1]
# # finalshape_sf <- finalshape_sf %>% left_join(master %>% select(AID, Mgtry), by = "AID")
# 
# #finalshape_sf <- st_as_sf(finalshape)
# 
# 
# #---------------------------------------#
# #Sort data by ID####
# CurrentNumbers <- finalshape %>% arrange(AID, POSIXct)
# alive2 <- alive %>% filter(Fawn.Sex != "M")
# Potentials <- CurrentNumbers %>% dplyr::filter(AID %in% alive2$AID & !AID %in% pre_list$AID)
# 
# 
# 
# Potentials_lite <- Potentials %>% filter(Year == 2023) %>% group_by(AID, Date) %>% sample_n(1)
# 
# dna <- Potentials_lite %>% filter(!AID %in% unique(dna_filed$AID))
# tooth <- Potentials_lite %>% filter(!AID %in% unique(NeedTooth$AID))
# 
# alive %>% filter(AID %in% unique(dna$AID) | AID %in% unique(NeedTooth$AID)) %>% filter(Mgtry %in% c("Long", "Medium") | is.na(StopDate))
# 
# Potentials_lite %>% filter(Mgtry %in% c("Long", "Medium") & !AID %in% c(601,669,680))  %>% dplyr::select(AID, Mgtry) %>% distinct(AID, Mgtry)
# 
# addLong <- c(601, 617)
# addMed <- c(669, 680, 628, 606, 607, 621)
# 
# 
# 
# # LongNumbers <- CurrentNumbers %>% filter(AID %in% Longitudinal$AID)
# # #LongNumbers <- LongNumbers %>% filter(!AID %in% NotAvailForLongitudinal)
# # 
# # nums <- data.frame(mgtry = c("Long", "Medium", "Short"), Online = NA, Longitudinal = NA)
# # 
# # nums$Online <- as.data.frame(CurrentNumbers%>% st_drop_geometry()  %>% group_by(Mgtry) %>% summarise(n = n_distinct(AID)) %>% select(n) %>% rename( "Online" = "n"))
# # nums$Longitudinal <- as.data.frame(LongNumbers %>% st_drop_geometry() %>% group_by(Mgtry) %>% summarise(n = n_distinct(AID)) %>% select(n) %>% rename( "Longitudinal" = "n"))
# # AddNew <- 35 - nums$Online; names(AddNew) <- "AddNew"
# # AddLong <- 20 - nums$Longitudinal; names(AddLong) <- "AddLong"
# # 
# # CaptureNumbers <- cbind(nums, AddNew, AddLong)
# # #number of eachs strategy in Longitudinal####
# # CaptureNumbers
# # 
# # 
# # 
# # #t <- CurrentNumbers %>% group_by(Mgtry) %>% summarise(deer = toString(unique(AID))) %>% st_drop_geometry() %>% as.data.frame()
# # #t$deer[1]
# # 
# # alive <- master[which(is.na(master$StopDate)),]
# # #alive$CollarNum <- str_sub(alive$CllrSrN,1,nchar(alive$CllrSrN)-1) #unclear why I did this...
# # 
# # 
# # 
# # #now choose Longitudinal####
# # alivedeer <- as.data.frame(master[which(is.na(master$StopDate)),"AID"]); names(alivedeer)[1] <- "AID"
# # alivedeer <- alivedeer %>% arrange(AID) %>% na.omit()
# # alivedeer <- alivedeer %>% filter(!AID %in% LongNumbers$AID)
# # tmp <- alivedeer[,1]
# # tmp <- tmp[order(tmp)]
# # tmp
# # 
# 
# 
# class(finalshape_sf$AID)
# 
# finalshape_sf$AID <- as.integer(finalshape_sf$AID)
# 
# PotentialsToLongitudinal <- finalshape_sf %>% filter(AID %in% tmp)
# 
# #PotentialsToLongitudinal 
# 
# PotentialsToLongitudinal$POSIXct <- as.POSIXct(PotentialsToLongitudinal$POSIXct)
# PriorityLongitudinal <- PotentialsToLongitudinal %>% filter(CllrSrN %in% c("706081A","706085A","706086A","706088A","706090A","706091A","706097A","706098A","706103A","706105A","706107A","706108A","706109A","707228A","723569A","723570A")); unique(PriorityLongitudinal$AID) #these are collars that need to be replaced anyways
# 
# str(PotentialsToLongitudinal)
# Summer <- PotentialsToLongitudinal %>% filter(Month %in% c(7,8))
# 
# CurrentNumbers_lite <- CurrentNumbers %>% group_by(AID, Day) %>%  sample_n(1)
# Summer_lite_priority <- Summer %>% group_by(AID, Day) %>% sample_n(1) %>% filter(AID %in% unique(PriorityLongitudinal$AID))
# Summer_lite <- Summer %>% group_by(AID, Day) %>% sample_n(1) #%>% filter(!AID %in% unique(PriorityLongitudinal$AID))
# #Summer_lite_19 <- Summer_lite %>% filter(AID < 600)
# dna <- Summer_lite %>% filter(!AID %in% unique(dna_filed)); unique(dna$AID)
# 
# #mapview(Summer_lite, legend = mapviewGetOption("legend"), zcol = "Mgtry", popup = "AID")
# mapview(CurrentNumbers_lite, col.regions = "grey", alpha = .05) + mapview(dna, legend = mapviewGetOption("legend"), zcol = "Mgtry", popup = "AID") 
# mapview(CurrentNumbers_lite, col.regions = "grey", alpha = .05) + mapview(Summer_lite, legend = mapviewGetOption("legend"), zcol = "Mgtry", popup = "AID")
# 
# 
# LongDist <- data.frame(AID = c(543,541,665,611,681), Mgtry = rep("Long", 5))
# MedDist <- data.frame(AID =c(619,622, 509, 629, 659), Mgtry = rep("Medium", 5))
# ShortDist <- data.frame(AID =c(529, 682, 545, 521, 666, 610, 652, 605), Mgtry = rep("Short", 8))
# 
# bucks <- c(655,683,657,656,651)
# 
# #could capture any of these long: 447, 452, 541, 543
# AddToLongitudinal <- rbind(LongDist, MedDist, ShortDist)
# AddToLongitudinal <- AddToLongitudinal %>% filter(!AID %in% bucks)
# 
# REPLACECOLLARS <- REPLACECOLLARS %>% select(AID, Mgtry)
# 
# 
# #must capture
# MUSTCAPTURE <- rbind(AddToLongitudinal, REPLACECOLLARS, REMOVECOLLARS)
# MUSTCAPTURE <- MUSTCAPTURE %>% filter(!duplicated(AID)) %>% arrange(AID)
# MUSTCAPTURE
# 
# 
# #create final list ####
# MUSTCAPTURENow <- MUSTCAPTURE %>% filter(AID %in% alive$AID)
# LongitudinalNow <- Longitudinal %>% filter(AID %in% alive$AID)
# #HIGHRESDATA<- alive %>% filter(AID %in% HIGHRESDATA$AID)
# FinalList <- rbind(LongitudinalNow, MUSTCAPTURENow) #, HIGHRESDATA , REPLACECOLLARS)
# FinalList <- FinalList %>% select(AID,  Mgtry)
# FinalList <- FinalList %>% arrange(AID)
# 
# 
# FinalList <- FinalList %>% filter(!duplicated(AID)) %>% arrange(AID) # %>% filter(!AID %in% NotAvailForLongitudinal)
# FinalList
# 
# FinalList %>% group_by(Mgtry) %>% summarise(count = n())
# nrow(FinalList)
# 
# #last checks
# REPLACECOLLARS %>% filter(!AID %in% FinalList$AID)
# REMOVECOLLARS %>% filter(!AID %in% FinalList$AID)
# AddToLongitudinal %>% filter(!AID %in% FinalList$AID)
# Longitudinal %>% filter(!AID %in% FinalList$AID & AID %in% alive$AID)
# 
# #last checks
# REPLACECOLLARS %>% filter(AID %in% FinalList$AID)
# REMOVECOLLARS %>% filter(AID %in% FinalList$AID)
# AddToLongitudinal %>% filter(AID %in% FinalList$AID)
# Longitudinal %>% filter(AID %in% FinalList$AID)
# #alive %>% filter(!AID %in% FinalList$AID & AID < 600)
# 
# 
# # tmp2 <- finalshape_sf %>% group_by(AID, Day) %>% sample_n(1) %>% filter(AID %in% alive$AID & !Mgtry == "Long" & !AID %in% FinalList$AID) %>% filter(Month == "08" & Mgtry == "Short")
# # 
# # mapview(tmp2, zcol = "AID")
# 
# 
# 
# 
# 
# FinalList$CollectTooth <- ifelse (FinalList$AID %in% NeedTooth$AID, "Yes", "")
# FinalList$CollectDNA <- ifelse(FinalList$AID %in% dna_filed$AID, "", "Yes")
# FinalList$Replace <- ifelse(FinalList$AID %in% REPLACECOLLARS$AID, "Replace", ifelse(FinalList$AID %in% REMOVECOLLARS$AID, "RemoveCollar",""))
# FinalList$Replace <- ifelse(FinalList$AID %in% bucks, "RemoveCollar", FinalList$Replace)
# FinalList$Replace <- ifelse(FinalList$AID %in% c(255,665), "Replace Iridium", FinalList$Replace)
# #FinalList$CollarStatus <- ifelse(FinalList$AID %in% REPLACECOLLARS$AID, "BatteryExhausted",ifelse(FinalList$AID %in% REMOVECOLLARS$AID, "RemoveCollar",""))
# 
# FinalList$CapNum	<- ""
# FinalList$CapDate<- ""
# 
# FinalList <- FinalList %>% left_join(alive %>% select(AID, TagColor, TagNum, CllrSrN, Frequency)) %>% select(names(lastcap)) %>% arrange(Frequency)
# 
# FinalList %>% filter(AID %in% REMOVECOLLARS$AID)
# 
# FinalList
# 
# write.csv(FinalList,"C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CapturePlans/2023/March/RDH_Dec2023_CaptureList.csv", row.names = F)
# 
# 
# 
# x <- finalshape_sf %>% filter(AID %in% FinalList$AID) %>% group_by(AID) %>%  mapview()
# 
# # 
# # 
# # 
# 
# staging_sites <- st_as_sf(data.frame(Type = c("Staging Area","Staging Area","Staging Area","Alternate Area"), Long = c(-109.16638,-108.96086,-109.20340,-108.99505), Lat = c(41.73280,41.79027,42.50627,41.77891)), coords = c("Long", "Lat"), crs = "+proj=latlon +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs")
# 
# xx <- mapview(staging_sites, zcol = "Type", cex = 10, map.types = c("OpenStreetMap.DE")) + mapview(final.point.ll, col.regions = "black", legend = F, map.types = c("OpenStreetMap.DE")) 
# 
# mapshot(xx, url = "./RDH_DecemberCaptures.html")


