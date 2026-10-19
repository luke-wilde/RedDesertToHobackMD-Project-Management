# Code to create fawn recruitment sheets
# written by Luke Wilde
# Oct. 1 2023
# update Oct. 17 2026

# Load Libraries ####
#-----------------------------------#
library(tidyverse)
library(lubridate)
library(readxl)


# Call data ####
#-----------------------------------#
setwd("C:/Users/lwilde2/Documents/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/")
M24 <- read_excel("Annual Capture Data/RD2H_MD_March2026_CaptureData.xlsx")



#load IDs
DeerID.Data <- read.csv("C:/Users/lwilde2/Documents/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_15MARCH2026.csv")

# Seperate into Pregnant and Males ####
#------------------------------------#
preg <- M24 %>% select(DeerID, Pregnant, PregYear, FetusNum) %>% filter(Pregnant == "TRUE" & FetusNum > 0) %>% rename(AID = DeerID)
notpreg <- M24 %>% select(DeerID, Pregnant, PregYear, FetusNum) %>% filter(Pregnant == "FALSE" & FetusNum == 0) %>% rename(AID = DeerID)
male <- M24 %>% select(DeerID, Sex) %>% filter(Sex == "Male") %>% rename(AID = DeerID)


# grab all alive deer ####
#-----------------------------------#
#Merge DeerIDs based on unique CllrSrN and range of dates
#Format date and time columns
DeerID.Data$StartDate <- as.POSIXct(DeerID.Data$StartDate, format="%m/%d/%Y", origin = "1970-01-01")
DeerID.Data$StopDate <- as.POSIXct(DeerID.Data$StopDate,format="%m/%d/%Y", origin = "1970-01-01")

DeerID.Data$Startdt <- paste(as.character(DeerID.Data$StartDate), DeerID.Data$StartTime, sep=" ")
DeerID.Data$StartDateTime <- as.POSIXct(strptime(DeerID.Data$Startdt, format="%Y-%m-%d %H:%M:%S"), origin = "1970-01-01", tz="MST")

DeerID.Data$Stopdt <- paste(as.character(DeerID.Data$StopDate), DeerID.Data$StopTime, sep=" ")
DeerID.Data$StopDateTime <- as.POSIXct(strptime(DeerID.Data$Stopdt, format="%Y-%m-%d %H:%M:%S"), origin = "1970-01-01", tz="MST")


#Format date and time columns
DeerID.Data$StartDate <- as.POSIXct(DeerID.Data$StartDate)
DeerID.Data$StopDate <- as.POSIXct(DeerID.Data$StopDate)

names(DeerID.Data)

#Remove unnecessary columns
DeerID.Data<- DeerID.Data %>% dplyr::select(AID, Mgtry, TagColor, TagNum, Frequency, CllrSrN, StartDate, StopDate, Fawn.Sex)
head(DeerID.Data)

alive <- DeerID.Data %>% filter(is.na(StopDate))

# connect males, pregnant, and alive ####
#-----------------------------------#

#remove those not pregnant
#alive <- alive %>% filter(AID %in% notpreg$AID)  #alive %>% mutate(Preg = ifelse(AID %in% notpreg$AID, " ", ifelse(AID %in% male$AID, " ", ifelse(AID %in% preg$AID, "PREG", "UNK"))))
alive

males <- alive %>% filter(AID %in% male$AID | Fawn.Sex == "M") %>% select (AID, TagColor, TagNum, Frequency, CllrSrN) %>% mutate(Date = " ", CollarFit = " ", SeenWith = " ", Notes = " ") 

FFR <- alive %>% filter(!AID %in% males$AID) %>% select(AID, TagColor, TagNum, Frequency, CllrSrN) %>% mutate(Date = " ", CollarFit = " ", NumFawns = " ", ConfLevel = " ", SeenWith = " ", Notes = " ") 

males <- males %>% filter(AID > 949 ) #%>% filter(AID > 849 | AID < 800)

nrow(FFR); nrow(males)


write.csv(FFR, "G:/My Drive/RDH/Field Work/RDH_2026_FawnRecCheck.csv", row.names = F)
write.csv(males, "G:/My Drive/RDH/Field Work/RDH_2026_BuckCheck.csv", row.names = F)

# ####
#-----------------------------------#