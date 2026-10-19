library(tidyverse)
library(lubridate)
library(readxl)
library(sf)

select <- dplyr::select

setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/")
D24 <- read_excel("Annual Capture Data/RD2H_MD_Dec2024_CaptureData.xlsx")

dec9 <- st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureRecords/2024/December/WY Red Desert 9122024.GPX")
dec7 <- st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureRecords/2024/December/WY Red desert MD 7122024.GPX")
dec8 <- st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureRecords/2024/December/WY Red desert MD 8122024.GPX")

all <- rbind(dec9, dec7, dec8)

D24 <- D24 %>% select(StagingArea, CBETag, CapDate, Species, Sex, AdminProphylatics, AdminPainReliefMed, StageUTM_E, `Other Release Notes`) %>% left_join(., all %>% mutate(Lat = st_coordinates(.)[,2], Long = st_coordinates(.)[,1]) %>% dplyr::select(name, Lat, Long) %>% st_drop_geometry(), by = c("StageUTM_E" = "name"))




write.csv(D24, row.names = F, "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/Chapter33Permit/2024/Wilde_RedDesertMuleDeer_Dec2024_CBE.csv")
