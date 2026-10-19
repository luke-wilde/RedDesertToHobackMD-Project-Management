# Code to prepare WYNDD observation record for ch33
# Luke Wilde
# 1.27.2025

#-----------------------#
# prepare env ####
#install.packages("sf")
library(tidyverse)
library(sf)
library(lubridate)
library(readxl)
library(mapview)

#-----------------------#
# load data ####

year <- 2024

#data
meta <- "C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_10DECEMBER2024.csv"

#location of capture records
Dec <- read_excel(paste("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/Annual Capture Data/RD2H_MD_Dec",year,"_CaptureData.xlsx",sep=""))
Mar <- read_excel(paste("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/Annual Capture Data/RD2H_MD_March",year,"_CaptureData.xlsx",sep=""))

#location of data
load("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_CleanData.RData")

#age data
agedata <- read_csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/0_MainDataSets/RDH_KnownAgeByAID_Rolling.csv")

#capture locations -- ATTN did not use this in 2024 but will for future! Need to build it in!
m_loc <- c(st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/March_2024/WY Red Desert 1332024.GPX"), st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/March_2024/WY Red Desert MD 1232024.GPX"))
d_loc <- c(st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/December_2024/WY Red Desert MD 8122024.GPX"),
          st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/December_2024/WY Red Desert 9122024.GPX"),
                  st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/December_2024/WY Red desert MD 7122024.GPX"))

DeerID <- read.csv(meta)

#reformat Dates
DeerID$StopDate <- as.POSIXct(DeerID$StopDate, format = "%m/%d/%Y", tz = "MST")
DeerID$StartDate <- as.POSIXct(DeerID$StartDate, format = "%m/%d/%Y", tz = "MST")
DeerID$CollarMalfDate <- as.POSIXct(DeerID$CollarMalfDate, format = "%m/%d/%Y", tz = "MST")

#-----------------------#
# ####

df <- data.frame("Permittee Last Name"=rep(NA,200), "Permit ID#"=NA,"Observer"=NA,"Data Entry Personell"=NA,"Date"=NA,"Species Scientific Name"=NA,"Detection Type"=NA, "Collar Frequency"=NA,	"1st Tag #"=NA,	"1st Tag Color"=NA,	"2nd Tag #"=NA,	"2nd Tag Color"=NA,	"Animal ID #"=NA,	"ma_adult_qty"=NA,	"ma_year_qty"=NA,	"ma_juv_qty"=NA,	"ma_larvae_qty"=NA,	"ma_meta_qty"=NA,	"ma_unk_qty"=NA,	"fe_adult_qty"=NA,	"fe_year_qty"=NA,	"fe_juv_qty"=NA,	"fe_larvae_qty"=NA,	"fe_meta_qty"=NA,	"fe_unk_qty"=NA,	"un_adult_qty"=NA,	"un_year_qty"=NA,	"un_juv_qty"=NA,	"un_larvae_qty"=NA,	"un_meta_qty"=NA,	"un_unk_qty"=NA,	"egg_masses_qty"=NA,	"Observer Activity"=NA,	"Mortality"=NA,	"White-Nose Damage"=NA,	"Animal Activity"=NA,	"Habitat Type"=NA,	"Dwelling Feature"=NA,	"X Coordinate"=NA,	"Y Coordinate"=NA,	"Coordinate System"=NA,	"Mapping Precision (m)"=NA,	"Observation Comment"=NA,	"Record Sensitivity"=NA)



#-----------------------#
# Collect all animals captured under Ch33 ####

d <- Dec %>% select(DeerID, CapEvent, CapDate, ProcessStartTime, TagColor, TagNum, Sex, GeneralAgeClass) %>% rename(AID = DeerID) 
m <- Mar %>% select(DeerID, CapEvent, CapDate, ProcessStartTime, TagColor, TagNum, Sex, GeneralAgeClass) %>% rename(AID = DeerID) 

#correct datetime 
d <- rbind(d) %>%  mutate(ProcessStartTime = format(ProcessStartTime, "%H:%M:%S"), CapDate = format(CapDate, "%Y-%m-%d"), CapDatePOS = as.POSIXct(ymd_hms(paste(CapDate, ProcessStartTime))), AID = as.integer(AID)) %>% as.data.frame()
m <- rbind(m) %>%  mutate(ProcessStartTime = format(ProcessStartTime, "%H:%M:%S"), CapDate = format(CapDate, "%Y-%m-%d"), CapDatePOS = as.POSIXct(ymd_hms(paste(CapDate, ProcessStartTime))), AID = as.integer(AID)) %>% as.data.frame()

#-----------------------#
# Option 1: Use capture record from David ####

#NEED TO CODE for 2025 report!

#-----------------------#
# Option 2 estimate capture location from GPS record ####
current <- rdh_data.sf %>% filter(Year == year)

#find nearest GPS record to capture
m_loc <- m %>% left_join(current, by = "AID",relationship = "many-to-many") %>% group_by(AID) %>%
  mutate(time_diff = abs(difftime(CapDatePOS, POSIXct, units = "secs"))) %>%
  slice_min(time_diff, with_ties = F) %>% ungroup() %>% select(AID, CllrSrN, Frequency, CapEvent, CapDate, ProcessStartTime, CapDatePOS, POSIXct, UTM_N, UTM_E, TagColor, TagNum, Sex, GeneralAgeClass)

d_loc <- d %>% left_join(current, by = "AID",relationship = "many-to-many") %>% group_by(AID) %>%
  mutate(time_diff = abs(difftime(CapDatePOS, POSIXct, units = "secs"))) %>%
  slice_min(time_diff, with_ties = F) %>% ungroup() %>% select(AID, CllrSrN, Frequency, CapEvent, CapDate, ProcessStartTime, CapDatePOS, POSIXct, UTM_N, UTM_E, TagColor, TagNum, Sex, GeneralAgeClass)

c_loc <- rbind(m_loc, d_loc)

head(c_loc)

#-----------------------#
# merge with age and classify ####

agedata <- agedata %>% mutate(AID = as.integer(AID)) %>% select(AID, BirthYear)

c_loc <- c_loc %>% left_join(agedata, by = "AID", relationship = "many-to-many") %>% mutate(Birth = ymd(paste(BirthYear, 6, 15,sep="-")), age = as.numeric((date(POSIXct) - Birth )/ 365))

c_loc <- c_loc %>% mutate(Sex)

#-----------------------# 
# Fill in non standard columns ####
df <- df[1:nrow(c_loc), ]
df$Date <- c_loc$CapDate
df$Collar.Frequency <- c_loc$Frequency
df$X1st.Tag.. <- c_loc$TagNum
df$X1st.Tag.Color <- c_loc$TagColor
df$Animal.ID.. <- c_loc$AID
df$fe_adult_qty <- ifelse(c_loc$Sex == "Female" & c_loc$GeneralAgeClass == "Adult", 1, "")
df$fe_juv_qty <- ifelse(c_loc$Sex == "Female" & c_loc$GeneralAgeClass == "Fawn", 1, "")
df$fe_year_qty <- ifelse(c_loc$Sex == "Female" & c_loc$GeneralAgeClass == "Yearling", 1, "")
df$ma_adult_qty <- ifelse(c_loc$Sex == "Male" & c_loc$GeneralAgeClass == "Adult", 1, "")
df$ma_juv_qty <- ifelse(c_loc$Sex == "Male" & c_loc$GeneralAgeClass == "Fawn", 1, "")
df$ma_year_qty <- ifelse(c_loc$Sex == "Male" & c_loc$GeneralAgeClass == "Yearling", 1, "")
df$X.Coordinate <- c_loc$UTM_E
df$Y.Coordinate <- c_loc$UTM_N

#-----------------------#
# Attach mortality information ####
morts <- DeerID %>% arrange(AID, StartDate) %>% group_by(AID) %>% slice_tail(n= 1) %>% select(AID, StopDate, Mort.Cause, CollarMalfDate)

morts <- morts %>% filter(AID %in% c_loc$AID) %>% mutate(Mortality = ifelse(is.na(StopDate),"NONE",ifelse(Mort.Cause == "", "Cause Undetermined",sub(" .*", "", Mort.Cause))))

unique(morts$Mortality)

morts <- morts %>%
  mutate(Mortality = recode(Mortality,
                            "Puma" = "Mountain Lion",
                            "Unknown" = "Cause Undetermined",
                            "Harvest" = "Legal Harvest",
                            "Vehicle" = "Highway Accident"))

c_loc <- c_loc %>% left_join(morts %>% select(AID, Mortality), by = "AID", relationship = "many-to-many")

df$Mortality <- c_loc$Mortality

#-----------------------#
# Fill in standard columns ####

df$Permittee.Last.Name <- "Kauffman"
df$Permit.ID. <- "937"
df$Observer <- "Matthew Kauffman; Luke Wilde"
df$Data.Entry.Personell <- "Luke Wilde"
df$Species.Scientific.Name <- "Odocoileus hemionous"
df$Detection.Type <- "caught and released"
df$Observer.Activity <- "Live Trapping Operation - Animal"
df$Animal.Activity <- "feeding/foraging"
df$Habitat.Type <- "5.12,5.21,5.33"
df$Dwelling.Feature <- "N/A"
df$Coordinate.System <- "NAD83 UTM Zone 12"
df$Mapping.Precision..m. <- "10"
df$Record.Sensitivity <- "2-year Hide"

write.csv(df, "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/Chapter33Permit/2024/Reporting/RDH_Kauffman_33-937_WYNDD Observation Data.csv", row.names = F)

#-----------------------#
# ####
#-----------------------#
# ####
