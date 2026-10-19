# Code to conduct simple plotting for WGBGLC graphs and others
# written by Luke Wilde


# Written: Sept. 28 2022
# Last updated: May 30, 2024


# Load Libraries ####
#-----------------------------------#
library(tidyverse)
library(lubridate)
library(readxl)


# Call data ####
#-----------------------------------#

#mig timing table
MigrationTimes <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/RDH_AllSpringFallMigrationTiming_2014_thru_2022.csv")

#load IDs
meta <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_10DECEMBER2024.csv")

setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/")
FRS15 <- read_excel("Fawn Recruitment/RD2H_MD_Fall2015_FawnRecruitment.xlsx")
FRS17 <- read_excel("Fawn Recruitment/RD2H_MD_Fall2017_FawnRecruitment.xlsx")
FRS18 <- read_excel("Fawn Recruitment/RD2H_MD_Fall2018_FawnRecruitment.xlsx")
FRS19 <- read_excel("Fawn Recruitment/RD2H_MD_Fall2019_FawnRecruitment.xlsx")
FRS20 <- read_excel("Fawn Recruitment/RD2H_MD_Fall2020_FawnRecruitment.xlsx")
FRS21 <- read_excel("Fawn Recruitment/RD2H_MD_Fall2021_FawnRecruitment.xlsx")
FRS22 <- read_excel("Fawn Recruitment/RD2H_MD_Fall2022_FawnRecruitment.xlsx")
FRS23 <- read_excel("Fawn Recruitment/RD2H_MD_Fall2023_FawnRecruitment.xlsx")
FRS24 <- read_excel("Fawn Recruitment/RD2H_MD_Fall2024_FawnRecruitment.xlsx")

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
M24 <- read_excel("Annual Capture Data/RD2H_MD_March2024_CaptureData.xlsx") #warning is: Tayler forgot to record one time

# Format datasets ####
#-----------------------------------#
#FRS

FRS15 <- FRS15 %>% select(Date, MigrationStrategy, DeerID,  "YoungAtHeel?", NumYoung, NumDoes, NumFawns, RecruitmentConfidence) %>% rename(., YoungAtHeel = "YoungAtHeel?") %>% arrange(Date)
FRS17 <- FRS17 %>% select(Date, MigrationStrategy, DeerID,  YoungAtHeel, NumYoung, NumDoes, NumFawns, RecruitmentConfidence)%>% arrange(Date)
FRS18 <- FRS18  %>% select(Date, MigrationStrategy, DeerID,  "YoungAtHeel?", NumYoung, NumDoes, NumFawns, RecruitmentConfidence) %>% rename(., YoungAtHeel = "YoungAtHeel?")%>% arrange(Date)
FRS19 <- FRS19 %>% select(Date, MigrationStrategy, DeerID,  YoungAtHeel, NumYoung, NumDoes, NumFawns, RecruitmentConfidence)%>% arrange(Date)
FRS20 <- FRS20 %>% select(Date, MigrationStrategy, DeerID,  YoungAtHeel, NumYoung, NumDoes, NumFawns, RecruitmentConfidence)%>% arrange(Date)
FRS21 <- FRS21 %>% select(Date, MigrationStrategy, DeerID, YoungAtHeel,NumYoung, NumDoes, NumFawns, RecruitmentConfidence)%>% arrange(Date)
FRS22 <- FRS22 %>% select(Date, MigrationStrategy, DeerID, YoungAtHeel,NumYoung, NumDoes, NumFawns, RecruitmentConfidence)%>% arrange(Date)
FRS23 <- FRS23 %>% mutate(MigrationStrategy = NA) %>%  rename(YoungAtHeel = 'YoungAtHeel (1=Yes, 0=No)', NumYoung = 'NumOfOffspring (with Collar)')
FRS24 <- FRS24 %>% mutate(MigrationStrategy = NA) %>%  rename(YoungAtHeel = 'YoungAtHeel (1=Yes, 0=No)', NumYoung = 'NumOfOffspring (with Collar)')

FRS23 <- FRS23 %>% mutate(AID = DeerID) %>% select(-MigrationStrategy) %>% left_join(meta %>% mutate(MigrationStrategy = Mgtry) %>% select(AID, MigrationStrategy), by = "AID", relationship = "many-to-many") %>% select(Date, MigrationStrategy, DeerID, YoungAtHeel,NumYoung, NumDoes, NumFawns, RecruitmentConfidence) %>% arrange(Date)

FRS24 <- FRS24 %>% mutate(AID = DeerID) %>% select(-MigrationStrategy) %>% left_join(meta %>% mutate(MigrationStrategy = Mgtry) %>% select(AID, MigrationStrategy), by = "AID", relationship = "many-to-many") %>% select(Date, MigrationStrategy, DeerID, YoungAtHeel,NumYoung, NumDoes, NumFawns, RecruitmentConfidence) %>% arrange(Date)

FRS15 <- FRS15[!duplicated(FRS15[, c("DeerID")], fromLast=T),]
FRS17 <- FRS17[!duplicated(FRS17[, c("DeerID")], fromLast=T),]
FRS18 <- FRS18[!duplicated(FRS18[, c("DeerID")], fromLast=T),]
FRS19 <- FRS19[!duplicated(FRS19[, c("DeerID")], fromLast=T),]
FRS20 <- FRS20[!duplicated(FRS20[, c("DeerID")], fromLast=T),]
FRS21 <- FRS21[!duplicated(FRS21[, c("DeerID")], fromLast=T),]
FRS22 <- FRS22[!duplicated(FRS22[, c("DeerID")], fromLast=T),]
FRS23 <- FRS23[!duplicated(FRS23[, c("DeerID")], fromLast=T),]
FRS24 <- FRS24[!duplicated(FRS24[, c("DeerID")], fromLast=T),]


FRSall <- rbind(FRS17, FRS18, FRS19, FRS20, FRS21, FRS22, FRS23,FRS24) %>% mutate(Year = as.character(year(Date))) # 2015 has too few records of Medium or Short to keep

FRSall$MigrationStrategy <- ifelse(FRSall$MigrationStrategy == "Resident", "Short", FRSall$MigrationStrategy)

str(FRSall)
head(FRSall)
tail(FRSall)

save(FRSall, file = "~/PhD_Chapters/Datasets/ChapterThree_Profiles/Data/FawnRecruitment_2014to2024.RData")


#FRSall <- FRSall %>% group_by(Year) %>%  distinct(DeerID, fromLast = T)

#---------------------------#
# State (-Age, Genetics) ####

# Mar
M14 <- M14  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm), as.numeric)
M15 <- M15   %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm), as.numeric)
M16 <- M16  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm), as.numeric)
M17 <- M17   %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm), as.numeric)
M18 <- M18   %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm), as.numeric)
M19 <- M19   %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm), as.numeric)
M20 <- M20   %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm), as.numeric)
M21 <- M21   %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm), as.numeric) %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = AID) %>% select(DeerID, MigrationBehavior), by = "DeerID")
M22 <- M22   %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm), as.numeric) %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = AID) %>% select(DeerID, MigrationBehavior), by = "DeerID") 
M23 <- M23   %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm), as.numeric) %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = AID) %>% select(DeerID, MigrationBehavior), by = "DeerID") 
M24 <- M24   %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Pregnant, FetusNum, EyeDiam1_mm,EyeDiam2_mm), as.numeric) %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = AID) %>% select(DeerID, MigrationBehavior), by = "DeerID") 



MARall <- bind_rows(M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24) %>% mutate(Year = year(CapDate)) 
MARall$CapEvent <- paste0("Mar", " ", MARall$Year, sep = "")



save(MARall, file = "~/PhD_Chapters/Datasets/ChapterThree_Profiles/Data/CaptureData_FetusData_2014to2024.RData")


#Dec
D14 <- D14 %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric)
D15 <- D15  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric)
D16 <- D16  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric)
D17 <- D17  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric)
D18 <- D18  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric)
D19 <- D19  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric)
D20 <- D20  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric)
D21 <- D21  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric) %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = AID) %>% select(DeerID, MigrationBehavior), by = "DeerID", relationship = "many-to-many")
D22 <- D22  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric) %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = AID) %>% select(DeerID, MigrationBehavior), by = "DeerID", relationship = "many-to-many")
D23 <- D23  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric) %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = AID) %>% select(DeerID, MigrationBehavior), by = "DeerID", relationship = "many-to-many")
D24 <- D24  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric) %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = AID) %>% select(DeerID, MigrationBehavior), by = "DeerID", relationship = "many-to-many")

DECall <- rbind(D14, D15, D16, D17, D18, D19, D20, D21,  D22, D23, D24) %>% mutate(Year = year(CapDate))
DECall$CapEvent <- paste0("Dec", " ", DECall$Year, sep = "")


save(DECall, file = "~/PhD_Chapters/Datasets/ChapterThree_Profiles/Data/CaptureData_LactData_2014to2024.RData")


CAPall <- bind_rows(DECall, MARall) %>% arrange(CapDate) %>% distinct(DeerID, CapDate,.keep_all = T)

save(CAPall, file = "~/PhD_Chapters/Datasets/ChapterThree_Profiles/Data/CaptureData_BodyMorph_2014to2024.RData")


#---------------------------#
# Personality - NOTRUN ####
# 
# #reload
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
# D24 <- read_excel("Annual Capture Data/RD2H_MD_Dec2024_CaptureData.xlsx")
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
# M23 <- read_excel("Annual Capture Data/RD2H_MD_March2023_CaptureData.xlsx")
# M24 <- read_excel("Annual Capture Data/RD2H_MD_March2024_CaptureData.xlsx") #warning is: Tayler forgot to record one time
# 
# 
# # Mar
# M14 <- M14  %>% select(CapDate, MigrationBehavior, Sex, Temp1, ChaseTime) %>% mutate_at(vars(Temp1), as.numeric)
# M15 <- M15    %>% select(CapDate,MigrationBehavior, Sex, Temp1, ChaseTime) %>% mutate_at(vars(Temp1), as.numeric)
# M16 <- M16   %>% select(CapDate,MigrationBehavior, Sex, Temp1, ChaseTime, Vocal, Activity, Panting, Hesitation, KickedRelease, Walk, Trot, Run, Stot) %>% mutate_at(vars(Temp1,Vocal, Activity), as.numeric)
# M17 <- M17    %>% select(CapDate,MigrationBehavior, Sex, Temp1, ChaseTime, Vocal, Activity, Panting, Hesitation, KickedRelease, Walk, Trot, Run, Stot) %>% mutate_at(vars(Temp1,Vocal, Activity), as.numeric)
# M18 <- M18    %>% select(CapDate,MigrationBehavior, Sex, Temp1, ChaseTime, Vocal, Activity, Panting, Hesitation, KickedRelease, Walk, Trot, Run, Stot) %>% mutate_at(vars(Temp1,Vocal, Activity), as.numeric)
# M19 <- M19    %>% select(CapDate, MigrationBehavior, Sex, Temp1, ChaseTime, Vocal, Activity, Panting, Hesitation, KickedRelease, Walk, Trot, Run, Stot) %>% mutate_at(vars(Temp1,Vocal, Activity), as.numeric)
# M20 <- M20    %>% select(CapDate, MigrationBehavior,Sex, Temp1, ChaseTime, Vocal, Activity, Panting, Hesitation, KickedRelease, Walk, Trot, Run, Stot) %>% mutate_at(vars(Temp1,Vocal, Activity), as.numeric)
# M21 <- M21    %>% select(CapDate,MigrationBehavior, Sex, Temp1, ChaseTime, Vocal, Activity, Panting, Hesitation, KickedRelease, Walk, Trot, Run, Stot) %>% mutate_at(vars(Temp1,Vocal, Activity), as.numeric) %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = AID) %>% select(DeerID, MigrationBehavior), by = "DeerID")
# M22 <- M22    %>% select(CapDate,MigrationBehavior, Sex, Temp1, ChaseTime, Vocal, Activity, Panting, Hesitation, KickedRelease, Walk, Trot, Run, Stot) %>% mutate_at(vars(Temp1,Vocal, Activity), as.numeric) %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = AID) %>% select(DeerID, MigrationBehavior), by = "DeerID") 
# M23 <- M23   %>% select(CapDate,MigrationBehavior, Sex, Temp1, ChaseTime, Vocal, Activity, Panting, Hesitation, KickedRelease, Walk, Trot, Run, Stot) %>% mutate_at(vars(Temp1,Vocal, Activity), as.numeric) %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = AID) %>% select(DeerID, MigrationBehavior), by = "DeerID") 
# M24 <- M24   %>% select(CapDate,MigrationBehavior, Sex, Temp1, ChaseTime, Vocal, Activity, Panting, Hesitation, KickedRelease, Walk, Trot, Run, Stot) %>% mutate_at(vars(Temp1,Vocal, Activity), as.numeric) %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = AID) %>% select(DeerID, MigrationBehavior), by = "DeerID") 
# 
# 
# 
# MARpers <- bind_rows(M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24) %>% mutate(Year = year(CapDate)) 
# MARpers$CapEvent <- paste0("Mar", " ", MARpers$Year, sep = "")
# 
# 
# 
# #Dec
# D14 <- D14 %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric)
# D15 <- D15  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric)
# D16 <- D16  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric)
# D17 <- D17  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric)
# D18 <- D18  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric)
# D19 <- D19  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric)
# D20 <- D20  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric)
# D21 <- D21  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric) %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = AID) %>% select(DeerID, MigrationBehavior), by = "DeerID", relationship = "many-to-many")
# D22 <- D22  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric) %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = AID) %>% select(DeerID, MigrationBehavior), by = "DeerID", relationship = "many-to-many")
# D23 <- D23  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric) %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = AID) %>% select(DeerID, MigrationBehavior), by = "DeerID", relationship = "many-to-many")
# D24 <- D24  %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, MigrationBehavior, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, Sex, LactationYN, Lactation) %>% mutate_at(vars(MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_kg, DeerID, scaledIFBFat,BodyCond, newLIVINDEX,ScaledMaxFat,IFBodyMass, IFFFLeanMass, LactationYN), as.numeric) %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = AID) %>% select(DeerID, MigrationBehavior), by = "DeerID", relationship = "many-to-many")
# 
# DECpers <- rbind(D14, D15, D16, D17, D18, D19, D20, D21,  D22, D23, D24) %>% mutate(Year = year(CapDate))
# DECpers$CapEvent <- paste0("Dec", " ", DECpers$Year, sep = "")
# 
# 
# CAPpers <- bind_rows(DECpers, MARpers) %>% arrange(CapDate) %>% distinct(DeerID, CapDate,.keep_all = T)
# 
# save(CAPpers, file = "~/PhD_Chapters/Datasets/ChapterThree_Profiles/Data/CaptureData_Personality_2014to2024.RData")
