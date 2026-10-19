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
meta <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_11DECEMBER2025.csv")

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
FRS25 <- read_excel("Fawn Recruitment/RD2H_MD_Fall2025_FawnRecruitment.xlsx")

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
FRS25 <- FRS25 %>% mutate(MigrationStrategy = NA) %>%  rename(YoungAtHeel = 'YoungAtHeel (1=Yes, 0=No)', NumYoung = 'NumOfOffspring (with Collar)')

FRS23 <- FRS23 %>% mutate(AID = DeerID) %>% select(-MigrationStrategy) %>% left_join(meta %>% mutate(MigrationStrategy = Mgtry) %>% select(AID, MigrationStrategy), by = "AID", relationship = "many-to-many") %>% select(Date, MigrationStrategy, DeerID, YoungAtHeel,NumYoung, NumDoes, NumFawns, RecruitmentConfidence) %>% arrange(Date)

FRS24 <- FRS24 %>% mutate(AID = DeerID) %>% select(-MigrationStrategy) %>% left_join(meta %>% mutate(MigrationStrategy = Mgtry) %>% select(AID, MigrationStrategy), by = "AID", relationship = "many-to-many") %>% select(Date, MigrationStrategy, DeerID, YoungAtHeel,NumYoung, NumDoes, NumFawns, RecruitmentConfidence) %>% arrange(Date)

FRS25 <- FRS25 %>% mutate(AID = DeerID) %>% select(-MigrationStrategy) %>% left_join(meta %>% mutate(MigrationStrategy = Mgtry) %>% select(AID, MigrationStrategy), by = "AID", relationship = "many-to-many") %>% select(Date, MigrationStrategy, DeerID, YoungAtHeel,NumYoung, NumDoes, NumFawns, RecruitmentConfidence) %>% arrange(Date)

FRS15 <- FRS15[!duplicated(FRS15[, c("DeerID")], fromLast=T),]
FRS17 <- FRS17[!duplicated(FRS17[, c("DeerID")], fromLast=T),]
FRS18 <- FRS18[!duplicated(FRS18[, c("DeerID")], fromLast=T),]
FRS19 <- FRS19[!duplicated(FRS19[, c("DeerID")], fromLast=T),]
FRS20 <- FRS20[!duplicated(FRS20[, c("DeerID")], fromLast=T),]
FRS21 <- FRS21[!duplicated(FRS21[, c("DeerID")], fromLast=T),]
FRS22 <- FRS22[!duplicated(FRS22[, c("DeerID")], fromLast=T),]
FRS23 <- FRS23[!duplicated(FRS23[, c("DeerID")], fromLast=T),]
FRS24 <- FRS24[!duplicated(FRS24[, c("DeerID")], fromLast=T),]
FRS25 <- FRS25[!duplicated(FRS25[, c("DeerID")], fromLast=T),]

FRSall <- rbind(FRS17, FRS18, FRS19, FRS20, FRS21, FRS22, FRS23,FRS24,FRS25) %>% mutate(Year = as.character(year(Date))) # 2015 has too few records of Medium or Short to keep

FRSall$MigrationStrategy <- ifelse(FRSall$MigrationStrategy == "Resident", "Short", FRSall$MigrationStrategy)

str(FRSall)
head(FRSall)
tail(FRSall)



#save(FRSall, file = "~/PhD_Chapters/Datasets/ChapterThree_Profiles/Data/FawnRecruitment_2014to2024.RData")


#FRSall <- FRSall %>% group_by(Year) %>%  distinct(DeerID, fromLast = T)

#Mar
M14 <- M14  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
M15 <- M15  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
M16 <- M16  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
M17 <- M17  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
M18 <- M18  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
M19 <- M19  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
M20 <- M20  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
M21 <- M21  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
M22 <- M22  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
M23 <- M23  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
M24 <- M24  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
M25 <- M25  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)

# x <- data.frame("DeerID" = as.character(c(601, 603, 605,606,607,608,609,610,611,612,613,614,615,616,617,618, 619,620,621,622,623,624,625,626,627,628,629,630,631)), "MigrationStrategy2" = c("Long","Long", "Short","Medium","Medium","Medium","N/A","Short","Long","N/A","N/A","N/A","N/A","Short","Long","N/A","Medium","Long","Medium", "Medium","Long", "Long", "Long","Medium", "Long","Medium","Medium","Long","Long")
# )

# tmp <- M22 %>% left_join(x, by = "DeerID")
# M22$MigrationBehavior <- ifelse(tmp$MigrationBehavior =="N/A", tmp$MigrationStrategy2,tmp$MigrationBehavior)              

M22 <- M22 %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = as.character(AID)) %>% select(DeerID, MigrationBehavior), by = "DeerID")
M23 <- M23 %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = as.character(AID)) %>% select(DeerID, MigrationBehavior), by = "DeerID")


MARall <- rbind(M14, M15, M16, M17, M18, M19, M20, M21, M22, M23, M24,M25) %>% mutate(Year = year(CapDate)) 
MARall$CapEvent <- paste0("Mar", " ", MARall$Year, sep = "")

## !!! ATTN: include , FetusNum, EyeDiam1_mm and then output this
#save(MARall, file = "~/PhD_Chapters/Datasets/ChapterThree_Profiles/Data/CaptureData_FetusData_2014to2024.RData")


#Dec
D14 <- D14  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
D15 <- D15  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
D16 <- D16  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
D17 <- D17  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
D18 <- D18  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
D19 <- D19  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
D20 <- D20  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
D21 <- D21  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex)
D22 <- D22  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex) %>% mutate(Age = 0)
D23 <- D23  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex) %>% mutate(Age = 0)
D24 <- D24  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex) %>% mutate(Age = 0)
D25 <- D25  %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age,Sex) %>% mutate(Age = 0)

D23 <- D23 %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = as.character(AID)) %>% select(DeerID, MigrationBehavior), by = "DeerID", relationship = "many-to-many")
D23 <- D23[!duplicated(D23$DeerID),]; D23

D24 <- D24 %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = as.character(AID)) %>% select(DeerID, MigrationBehavior), by = "DeerID", relationship = "many-to-many")
D24 <- D24[!duplicated(D24$DeerID),]; D24

D25 <- D25 %>% select(-MigrationBehavior) %>% left_join(meta %>% mutate(MigrationBehavior = Mgtry, DeerID = AID) %>% select(DeerID, MigrationBehavior), by = "DeerID", relationship = "many-to-many")
D25 <- D25[!duplicated(D25$DeerID),]; D25

DECall <- rbind(D14, D15, D16, D17, D18, D19, D20, D21,  D22, D23, D24, D25) %>% mutate(Year = year(CapDate))
DECall$CapEvent <- paste0("Dec", " ", DECall$Year, sep = "")


## !!! ATTN: include , Lactation and then output this
#save(DECall, file = "~/PhD_Chapters/Datasets/ChapterThree_Profiles/Data/CaptureData_LactData_2014to2024.RData")


CAPall <- rbind(DECall, MARall)


CAPall %>% summarise(n = n())
CAPall %>% summarise(n = length(unique(DeerID)))

CAPall$MigrationBehavior <- ifelse(CAPall$MigrationBehavior == "Resident", "Short", CAPall$MigrationBehavior)

#save(CAPall, file = "~/PhD_Chapters/Datasets/ChapterThree_Profiles/Data/CaptureData_2014to2024.RData")

#unique years
years <- CAPall  %>%  arrange(DeerID, Year) %>%
  group_by(DeerID) %>% summarise(n = n()); sum(years$n)

#consecutive years
u <- CAPall  %>%  arrange(DeerID, Year) %>%
  group_by(DeerID) %>%
  mutate(Consecutive = c(FALSE, diff(Year) == 1)) %>%
  summarise(Num_Consecutive_Years = sum(Consecutive)) %>%
  filter(Num_Consecutive_Years > 0)

marCap <- CAPall %>% mutate(Month = as.numeric(format(CapDate, "%m")),Year = as.numeric(format(CapDate, "%Y"))) %>% filter(Month == 3) 
decCap <- CAPall %>% mutate(Month = as.numeric(format(CapDate, "%m")),Year = as.numeric(format(CapDate, "%Y"))) %>% filter(Month == 12) 

growing_season <- marCap %>% select(DeerID, CapEvent, Year) %>% left_join(decCap %>% select(DeerID, CapEvent, Year), by = c("DeerID", "Year") ) %>% drop_na(CapEvent.y)
dormant_season <- decCap %>% mutate(NextYear = Year + 1) %>% select(DeerID, CapEvent, Year, NextYear) %>%
  left_join(marCap %>% select(DeerID, CapEvent, Year), by = c("DeerID" = "DeerID", "NextYear" = "Year"))  %>%
  drop_na(CapEvent.y)

growing_season %>% distinct(DeerID)
dormant_season %>% distinct(DeerID)


# Fawn Recruitment ####
#-----------------------------------#

stat <- FRSall %>% filter(RecruitmentConfidence %in% c("HIGH","High")) %>% as_tibble() %>% filter(!MigrationStrategy %in% c("N/A", NA, "TBD", "Medium/Disperser")) %>% dplyr::group_by(MigrationStrategy, Year) %>% dplyr::summarise(FawnsPer100Does = (sum(as.numeric(NumYoung))/length(unique(DeerID)) * 100));stat

ggplot(stat) + geom_bar(aes(x = Year, y = FawnsPer100Does, fill = MigrationStrategy), stat = "identity", width=.5, position = "dodge") + scale_fill_manual(values = c("mediumorchid4","darkorange","darkgreen")) + theme_classic() + labs(y = "Number of Fawns per 100 Does", x = "Year", fill = "Migration Strategy") + lims(y = c(0,120)) + theme(legend.position = "bottom") + geom_hline(aes(yintercept = 100), linetype = "dashed") + scale_y_continuous(limits = c(0,120), breaks = seq(0,130,15)) + theme(axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 16), axis.title.x = element_blank(), legend.text = element_text(size = 18), legend.title = element_text(size = 18))

ggsave("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/GrantApplications/WGBGLC_Grant/AnnualReports/2025/FRS_Figure_EOY.png", width = 12, height = 8)

# IFBF Dynamics ####
#-----------------------------------#
CAPall <- CAPall %>% filter(Sex == "Female")

stat2 <- na.omit(CAPall %>% select(!Age)) %>% group_by(MigrationBehavior, CapEvent) %>% summarise(Mean = mean(scaledIFBFat), SE = sd(scaledIFBFat)/sqrt(n())) %>% filter(MigrationBehavior != "N/A") %>% ungroup() #na.omit(CAPall)

stat2$CapEvent <- factor(stat2$CapEvent, levels = c("Mar 2014", "Dec 2014", "Mar 2015", "Dec 2015", "Mar 2016", "Dec 2016", "Mar 2017", "Dec 2017", "Mar 2018", "Dec 2018", "Mar 2019", "Dec 2019", "Mar 2020", "Dec 2020", "Mar 2021", "Dec 2021" , "Mar 2022", "Dec 2022", "Mar 2023", "Dec 2023", "Mar 2024", "Dec 2024", "Mar 2025", "Dec 2025"))



CAPall$MigrationBehavior <- ifelse(CAPall$MigrationBehavior == "Resident", "Short", CAPall$MigrationBehavior)
CAPall$MigrationBehavior <- ifelse(CAPall$MigrationBehavior == "Odd", "TBD", DECall$MigrationBehavior)
stat3 <- na.omit(DECall %>% select(!Age)) %>% group_by(MigrationBehavior) %>% summarise(Mean = mean(scaledIFBFat)) %>% filter(!MigrationBehavior %in% c(NA,"TBD","N/A","Medium/Disperser")) %>% ungroup()


ggplot(stat2 %>% filter(!MigrationBehavior %in% c("TBD","Odd","Medium/Disperser")) %>% mutate(MigrationBehavior = ifelse(MigrationBehavior == "Resident", "Short", MigrationBehavior))) + geom_pointrange(aes(x = CapEvent, y = Mean, ymin = Mean - SE, ymax = Mean + SE, group = MigrationBehavior, colour = MigrationBehavior), position = position_dodge2(width = .175)) + geom_line(aes(x = CapEvent, y = Mean, group = MigrationBehavior, colour = MigrationBehavior), position = position_dodge2(width = .175)) + scale_colour_manual(values = c("mediumorchid4","orange2","darkgreen")) + theme_classic() + labs(y = "Mean % Ingesta-free Body Fat (+/- SE)", x = "Date", colour = "Migration Strategy") + theme(legend.position = "bottom") + theme(axis.text.x = element_text(angle = 60, hjust=1, size = 12), axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 16), axis.title.x = element_blank(), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) + coord_cartesian(y = c(2.5,12.5)) 

ggsave("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/GrantApplications/WGBGLC_Grant/AnnualReports/2025/IFBF_Figure_EOY.png", width = 12, height = 8)


# stat2 <- na.omit(MARall %>% select(!Age)) %>% group_by(MigrationBehavior, CapEvent) %>% summarise(Mean = mean(scaledIFBFat), SE = sd(scaledIFBFat)/sqrt(n())) %>% filter(MigrationBehavior != "N/A") %>% ungroup() #na.omit(CAPall)
# 
# stat2$CapEvent <- factor(stat2$CapEvent, levels = c("Mar 2014", "Mar 2015", "Mar 2016", "Mar 2017", "Mar 2018", "Mar 2019", "Mar 2020", "Mar 2021", "Mar 2022", "Mar 2023"))
# 
# stat3 <- na.omit(MARall %>% select(!Age)) %>% group_by(MigrationBehavior) %>% summarise(Mean = mean(scaledIFBFat)) %>% filter(MigrationBehavior != "N/A") %>% ungroup()
# 
# ggplot(stat2) + geom_pointrange(aes(x = CapEvent, y = Mean, ymin = Mean - SE, ymax = Mean + SE, group = MigrationBehavior, colour = MigrationBehavior), position = position_dodge2(width = .175)) + geom_line(aes(x = CapEvent, y = Mean, group = MigrationBehavior, colour = MigrationBehavior), position = position_dodge2(width = .175)) + scale_colour_manual(values = c("mediumorchid4","darkorange","darkgreen")) + theme_classic() + labs(y = "Mean % Ingesta-free Body Fat (+/- SE)", x = "Date", colour = "Migration Strategy") + theme(legend.position = "bottom") + theme(axis.text.x = element_text(angle = 60, hjust=1, size = 12), axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 16), axis.title.x = element_blank(), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) + coord_cartesian(y = c(2,10)) + geom_hline(data = stat3, aes(yintercept = Mean, group = MigrationBehavior, colour = MigrationBehavior), linetype = "dashed", size = .75)
# 
# ggsave("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/GrantApplications/WGBGLC_Grant/AnnualReports/2023/March_Only_IFBF_Figure_v2.png", width = 12, height = 8)

# Migration timing graph ####
#----------------------------------------------------#
# 
# #clean
# DeerID <- DeerID %>% arrange(StartDateTime)
# DeerID <- DeerID[!duplicated(DeerID[, c("AID")], fromLast=T),]
# 
# head(DeerID)
# MigrationTimes %>% left_join(DeerID %>% select(AID, Mgtry), by = "AID")
# 

CAPall %>% filter(CapDate > as.Date("2022-05-15") & CapDate < as.Date("2024-05-15")) %>% summarise(n = length(unique(DeerID)))
