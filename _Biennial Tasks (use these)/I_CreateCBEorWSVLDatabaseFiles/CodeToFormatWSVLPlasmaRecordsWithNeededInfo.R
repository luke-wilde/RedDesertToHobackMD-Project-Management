library(tidyverse)
library(lubridate)
library(readxl)
library(sf)

blood <- read_excel("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/LabAnalyses/Blood Spinning WSVL/2024/December/Wilde_BloodSamples.xlsx")

setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/")
D24 <- read_excel("Annual Capture Data/RD2H_MD_Dec2024_CaptureData.xlsx")


RDH_SampleRec <- blood %>% left_join(., D24 %>% dplyr::select(Species, CapDate, ProcessStartTime, StageUTM_E, GeneralAgeClass, Sex, LabAccess, DeerID), by = "LabAccess")

dec9 <- st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureRecords/2024/December/WY Red Desert 9122024.GPX")
dec7 <- st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureRecords/2024/December/WY Red desert MD 7122024.GPX")
dec8 <- st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureRecords/2024/December/WY Red desert MD 8122024.GPX")

all <- rbind(dec9, dec7, dec8)

age_info <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Data/Known Age/RDH_KnownAgeByAID_2014to2024.csv")

RDH_SampleRec <- RDH_SampleRec %>% left_join(., all %>% mutate(Lat = st_coordinates(.)[,2], Long = st_coordinates(.)[,1]) %>% dplyr::select(name, Lat, Long) %>% st_drop_geometry(), by = c("StageUTM_E" = "name")) %>% rename( "CaptureLocation" = "StageUTM_E" , "CapTime" = "ProcessStartTime") %>% mutate(CapDate = as.character(date(CapDate)), CapTime = (format(CapTime, "%H:%M:%S", tz = "MST"))) %>% left_join(., age_info %>% dplyr::select(AID, BirthYear), by = c("DeerID" = "AID")) %>% dplyr::select(LabAccess, Species, CapDate, CapTime, Lat, Long, GeneralAgeClass, BirthYear, Sex)

write.csv(RDH_SampleRec, row.names = F, "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/LabAnalyses/Blood Spinning WSVL/2024/December/Wilde_RedDesertMuleDeer_Dec2024_PlasmaSampleRecords.csv")
