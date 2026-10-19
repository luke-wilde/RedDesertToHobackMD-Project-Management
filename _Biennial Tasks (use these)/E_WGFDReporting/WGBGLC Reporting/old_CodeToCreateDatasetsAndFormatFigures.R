# Code to conduct simple plotting for WGBGLC graphs and others
# written by Luke Wilde
# Sept. 28 2022

# Load Libraries ####
#-----------------------------------#
library(tidyverse)
library(lubridate)
library(readxl)


# Call data ####
#-----------------------------------#

#mig timing table
MigrationTimes <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/AllSpringFallMigrationTiming_2014_thru_2021.csv")

#load IDs
DeerID <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_09DECEMBER2022.csv")

setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/")
FRS15 <- read_excel("Fawn Recruitment/RD2H_MD_Fall2015_FawnRecruitment.xlsx")
FRS17 <- read_excel("Fawn Recruitment/RD2H_MD_Fall2017_FawnRecruitment.xlsx")
FRS18 <- read_excel("Fawn Recruitment/RD2H_MD_Fall2018_FawnRecruitment.xlsx")
FRS19 <- read_excel("Fawn Recruitment/RD2H_MD_Fall2019_FawnRecruitment.xlsx")
FRS20 <- read_excel("Fawn Recruitment/RD2H_MD_Fall2020_FawnRecruitment.xlsx")
FRS21 <- read_excel("Fawn Recruitment/RD2H_MD_Fall2021_FawnRecruitment.xlsx")
FRS22 <- read_excel("Fawn Recruitment/RD2H_MD_Fall2022_FawnRecruitment.xlsx")

D14 <- read_excel("Annual Capture Data/RD2H_MD_Dec2014_CaptureData.xlsx")
D15 <- read_excel("Annual Capture Data/RD2H_MD_Dec2015_CaptureData.xlsx")
D16 <- read_excel("Annual Capture Data/RD2H_MD_Dec2016_CaptureData.xlsx")
D17 <- read_excel("Annual Capture Data/RD2H_MD_Dec2017_CaptureData.xlsx")
D18 <- read_excel("Annual Capture Data/RD2H_MD_Dec2018_CaptureData.xlsx")
D19 <- read_excel("Annual Capture Data/RD2H_MD_Dec2019_CaptureData.xlsx")
D20 <- read_excel("Annual Capture Data/RD2H_MD_Dec2020_CaptureData.xlsx")
D21 <- read_excel("Annual Capture Data/RD2H_MD_Dec2021_CaptureData.xlsx")
D22 <- read_excel("Annual Capture Data/RD2H_MD_Dec2022_CaptureData.xlsx")

M14 <- read_excel("Annual Capture Data/RD2H_MD_March2014_CaptureData.xlsx")
M15 <- read_excel("Annual Capture Data/RD2H_MD_March2015_CaptureData.xlsx")
M16 <- read_excel("Annual Capture Data/RD2H_MD_March2016_CaptureData.xlsx")
M17 <- read_excel("Annual Capture Data/RD2H_MD_March2017_CaptureData.xlsx")
M18 <- read_excel("Annual Capture Data/RD2H_MD_March2018_CaptureData.xlsx")
M19 <- read_excel("Annual Capture Data/RD2H_MD_March2019_CaptureData.xlsx")
M20 <- read_excel("Annual Capture Data/RD2H_MD_March2020_CaptureData.xlsx")
M21 <- read_excel("Annual Capture Data/RD2H_MD_March2021_CaptureData.xlsx")
M22 <- read_excel("Annual Capture Data/RD2H_MD_March2022_CaptureData.xlsx")


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

FRS15 <- FRS15[!duplicated(FRS15[, c("DeerID")], fromLast=T),]
FRS17 <- FRS17[!duplicated(FRS17[, c("DeerID")], fromLast=T),]
FRS18 <- FRS18[!duplicated(FRS18[, c("DeerID")], fromLast=T),]
FRS19 <- FRS19[!duplicated(FRS19[, c("DeerID")], fromLast=T),]
FRS20 <- FRS20[!duplicated(FRS20[, c("DeerID")], fromLast=T),]
FRS21 <- FRS21[!duplicated(FRS21[, c("DeerID")], fromLast=T),]
FRS22 <- FRS22[!duplicated(FRS22[, c("DeerID")], fromLast=T),]

FRSall <- rbind(FRS17, FRS18, FRS19, FRS20, FRS21, FRS22) %>% mutate(Year = as.character(year(Date))) # 2015 had some issue???

str(FRSall)
head(FRSall)
tail(FRSall)

#FRSall <- FRSall %>% group_by(Year) %>%  distinct(DeerID, fromLast = T)

#Mar
M14 <- M14 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm, BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M15 <- M15 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M16 <- M16 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M17 <- M17 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M18 <- M18 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M19 <- M19 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M20 <- M20 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M21 <- M21 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)
M22 <- M22 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age)

x <- data.frame("DeerID" = as.character(c(601, 603, 605,606,607,608,609,610,611,612,613,614,615,616,617,618, 619,620,621,622,623,624,625,626,627,628,629,630,631)), "MigrationStrategy2" = c("Long","Long", "Short","Medium","Medium","Medium","N/A","Short","Long","N/A","N/A","N/A","N/A","Short","Long","N/A","Medium","Long","Medium", "Medium","Long", "Long", "Long","Medium", "Long","Medium","Medium","Long","Long")
)

tmp <- M22 %>% left_join(x, by = "DeerID")
M22$MigrationBehavior <- ifelse(tmp$MigrationBehavior =="N/A", tmp$MigrationStrategy2,tmp$MigrationBehavior)              

MARall <- rbind(M14, M15, M16, M17, M18, M19, M20, M21, M22) %>% mutate(Year = year(CapDate))
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
D22 <- D22 %>% filter(GeneralAgeClass == "Adult") %>% select(CapDate, MetatarsusLength_cm,BodyLength_cm, BodyGirth_cm, NetWeight_lbs, MigrationBehavior, DeerID, scaledIFBFat, Age) %>% mutate(Age = 0)

DECall <- rbind(D14, D15, D16, D17, D18, D19, D20, D21,  D22) %>% mutate(Year = year(CapDate))
DECall$CapEvent <- paste0("Dec", " ", DECall$Year, sep = "")

CAPall <- rbind(DECall, MARall)


# Fawn Recruitment ####
#-----------------------------------#

stat <- FRSall %>% filter(RecruitmentConfidence == "HIGH") %>% as_tibble() %>% filter(MigrationStrategy != "N/A") %>% dplyr::group_by(MigrationStrategy, Year) %>% dplyr::summarise(FawnsPer100Does = (sum(NumYoung)/length(unique(DeerID)) * 100))
stat

ggplot(stat) + geom_bar(aes(x = Year, y = FawnsPer100Does, fill = MigrationStrategy), stat = "identity", width=.5, position = "dodge") + scale_fill_manual(values = c("mediumorchid4","darkorange","darkgreen")) + theme_classic() + labs(y = "Number of Fawns per 100 Does", x = "Year", fill = "Migration Strategy") + lims(y = c(0,115)) + theme(legend.position = "bottom") + geom_hline(aes(yintercept = 100), linetype = "dashed") + scale_y_continuous(limits = c(0,105), breaks = seq(0,105,15)) + theme(axis.text.x = element_text(size = 18), axis.text.y = element_text(size = 12), axis.title.y = element_text(size = 16), axis.title.x = element_blank(), legend.text = element_text(size = 18), legend.title = element_text(size = 18))

ggsave("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/GrantApplications/WGBGLC_Grant/AnnualReports/2022/FRS_Figure_new.png", width = 12, height = 8)

# IFBF Dynamics ####
#-----------------------------------#

stat2 <- na.omit(CAPall) %>% group_by(MigrationBehavior, CapEvent) %>% summarise(Mean = mean(scaledIFBFat), SE = sd(scaledIFBFat)/(length(unique(DeerID))^.5)) %>% filter(MigrationBehavior != "N/A") %>% ungroup() #na.omit(CAPall)

stat2$CapEvent <- factor(stat2$CapEvent, levels = c("Mar 2014", "Dec 2014", "Mar 2015", "Dec 2015", "Mar 2016", "Dec 2016", "Mar 2017", "Dec 2017", "Mar 2018", "Dec 2018", "Mar 2019", "Dec 2019", "Mar 2020", "Dec 2020", "Mar 2021", "Dec 2021" , "Mar 2022", "Dec 2022"))

ggplot(stat2) + geom_pointrange(aes(x = CapEvent, y = Mean, ymin = Mean - SE, ymax = Mean + SE, group = MigrationBehavior, colour = MigrationBehavior), position = position_dodge2(width = .1)) + geom_line(aes(x = CapEvent, y = Mean, group = MigrationBehavior, colour = MigrationBehavior), position = position_dodge2(width = .1)) + scale_colour_manual(values = c("mediumorchid4","darkorange","darkgreen")) + theme_classic() + labs(y = "Mean Ingesta-free Body Fat (%)", x = "Date", colour = "Migration Strategy") + theme(legend.position = "bottom") + theme(axis.text.x = element_text(angle = 60, hjust=1, size = 12), axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 16), axis.title.x = element_blank(), legend.text = element_text(size = 18), legend.title = element_text(size = 18)) + coord_cartesian(y = c(0,12.5))

ggsave("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/GrantApplications/WGBGLC_Grant/AnnualReports/2022/IFBF_Figure_v2.png", width = 12, height = 8)

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

