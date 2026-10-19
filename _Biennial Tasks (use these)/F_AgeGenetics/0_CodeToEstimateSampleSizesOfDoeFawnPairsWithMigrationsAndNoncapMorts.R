#--------------------------------#
# libraries ####
library(tidyverse)
library(lubridate)

#--------------------------------#
# Load and fix data ####
pairs <- read_excel("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Data/RDH_DoeFawnPairs_Rolling.xlsx")

DEERID_new <- "C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_5MARCH2025.csv"

setwd("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/")
DeerID.Data <- read.csv(DEERID_new, header=TRUE, sep=",")

#Format date and time columns
DeerID.Data$StartDate <- as.POSIXct(DeerID.Data$StartDate, format="%m/%d/%Y", origin = "1970-01-01")
DeerID.Data$StopDate <- as.POSIXct(DeerID.Data$StopDate,format="%m/%d/%Y", origin = "1970-01-01")

DeerID.Data$Startdt <- paste(as.character(DeerID.Data$StartDate), DeerID.Data$StartTime, sep=" ")
DeerID.Data$StartDateTime <- as.POSIXct(strptime(DeerID.Data$Startdt, format="%Y-%m-%d %H:%M:%S"), origin = "1970-01-01", tz="MST")

DeerID.Data$Stopdt <- paste(as.character(DeerID.Data$StopDate), DeerID.Data$StopTime, sep=" ")
DeerID.Data$StopDateTime <- as.POSIXct(strptime(DeerID.Data$Stopdt, format="%Y-%m-%d %H:%M:%S"), origin = "1970-01-01", tz="MST")
names(DeerID.Data)
head(DeerID.Data)

meta <- DeerID.Data %>% arrange(AID, StartDateTime)
rm(DeerID.Data, DEERID_new)

#--------------------------------#
# find start and end for each ####
moms <- pairs %>% select(Family, `Mother AID`)
fawns <- pairs %>% select(Family, `Fawn AID`)

#find start
start <- meta %>% group_by(AID) %>% filter(StartDateTime == first(StartDateTime)) %>% select(AID, StartDateTime)
end <- meta %>% group_by(AID) %>% filter(StopDateTime == last(StopDateTime) | is.na(StopDateTime)) %>% mutate(StopDateTime = as.POSIXct(ifelse(is.na(StopDateTime), ymd_hms(paste(Sys.Date()," 00:00:00",sep="")), StopDateTime))) %>% select(AID, StopDateTime)

dur <- start  %>% left_join(end , by="AID") %>% mutate(Duration = as.numeric(as.duration(interval(StartDateTime, StopDateTime)), "days"))

#join
moms <- moms %>% left_join(dur %>% mutate(AID = as.character(AID)), by = c(`Mother AID` = "AID"))
fawns <- fawns %>% left_join(dur %>% mutate(AID = as.double(AID)), by = c(`Fawn AID` = "AID"))

pairs <- pairs %>% left_join(moms, by = c("Family", "Mother AID"), relationship = "many-to-many") %>% left_join(fawns, by = c("Family", "Fawn AID"), relationship = "many-to-many", suffix=c(".mother",".fawn")) %>% distinct(Family, `Mother AID`, `Fawn AID`, .keep_all = T)

firstmigs <- pairs %>% mutate(month.fawn = month(StartDateTime.fawn), month.mother = month(StartDateTime.mother)) %>% filter(case_when(
  month.mother == 3 ~ Duration.mother > 90,
  month.mother == 12 ~ Duration.mother > 180,
  TRUE ~ TRUE
) & `Age of First Migration Together` %in% c("Fawn") &  case_when(
  month.fawn == 3 ~ Duration.fawn > 90,
  month.fawn == 12 ~ Duration.fawn > 180,
  TRUE ~ TRUE
)) %>% distinct(`Fawn AID`,.keep_all=T)

secondmigs <- pairs %>% mutate(month.fawn = month(StartDateTime.fawn), month.mother = month(StartDateTime.mother)) %>% filter(case_when(
  month.mother == 3 ~ Duration.mother > 90,
  month.mother == 12 ~ Duration.mother > 180,
  TRUE ~ TRUE
) & `Age of First Migration Together` %in% c("Yearling") &  case_when(
  month.fawn == 3 ~ Duration.fawn > 90,
  month.fawn == 12 ~ Duration.fawn > 180,
  TRUE ~ TRUE
)) %>% distinct(`Fawn AID`,.keep_all=T)

adultmigs <- pairs %>% mutate(month.fawn = month(StartDateTime.fawn), month.mother = month(StartDateTime.mother)) %>% filter(case_when(
  month.mother == 3 ~ Duration.mother > 90,
  month.mother == 12 ~ Duration.mother > 180,
  TRUE ~ TRUE
) & `Age of First Migration Together` %in% c("Adult") &  case_when(
  month.fawn == 3 ~ Duration.fawn > 90,
  month.fawn == 12 ~ Duration.fawn > 180,
  TRUE ~ TRUE
)) %>% distinct(`Fawn AID`,.keep_all=T)

killedfawns <- pairs %>% mutate(month.fawn = month(StartDateTime.fawn), month.mother = month(StartDateTime.mother)) %>% filter(`Age of First Migration Together` %in% c("Fawn") &  case_when(
  month.fawn == 3 ~ Duration.fawn < 14,
  month.fawn == 12 ~ Duration.fawn < 14,
  TRUE ~ TRUE
)) %>% distinct(`Fawn AID`,.keep_all=T)

loststudy <- pairs %>% mutate(month.fawn = month(StartDateTime.fawn), month.mother = month(StartDateTime.mother)) %>% filter(case_when(
  month.mother == 3 ~ Duration.mother > 14,
  month.mother == 12 ~ Duration.mother > 14,
  TRUE ~ TRUE
) & `Age of First Migration Together` %in% c("Fawn") &  case_when(
  month.fawn == 3 ~ Duration.fawn > 14,
  month.fawn == 12 ~ Duration.fawn > 14,
  TRUE ~ TRUE
)) %>% distinct(`Fawn AID`,.keep_all=T)



#--------------------------------#
# now the fawns we didnt have mom ####
allfawn <- meta %>% filter(Fawn.Sex %in% c("M","F")) %>% reframe(AID = unique(AID))

unkMoms <- allfawn %>% filter(!AID %in% pairs$`Fawn AID`)

unkMoms <- unkMoms %>% left_join(dur %>% mutate(AID = as.double(AID)), by = c("AID"))

killedunkMoms <- unkMoms %>% mutate(month.fawn = month(StartDateTime)) %>% filter( case_when(
  month.fawn == 3 ~ Duration < 14,
  month.fawn == 12 ~ Duration < 14,
  TRUE ~ TRUE
)) %>% distinct(AID,.keep_all=T)

#--------------------------------#
# ####


#--------------------------------#
# ####
#--------------------------------#
# ####
#--------------------------------#
# ####
#--------------------------------#
# ####
#--------------------------------#
# ####

