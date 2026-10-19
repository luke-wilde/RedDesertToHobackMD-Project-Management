#script to process migration table for input to BBMM

library(tidyverse)

#load new datasets
timing <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/2023/RDH_SpringAndFallMigrations_2023.csv")
DeerID.Data <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_29MAY2024.csv")

#select current year
timing <- timing %>% filter(bioYearFull == 2023)
head(timing); tail(timing)

#reshape table
dateFormat <- "%m/%d/%Y" #"%Y-%m-%d" # ATTN: this can differ depending on the settings

MigTab <- timing %>% mutate("Year" = bioYearFull, "AID" = newUid,		
                            "SpringMS" = format(as.POSIXct(mig1start, format = dateFormat, origin = "1970-01-01"),paste(dateFormat," %0:%0:%0", sep = "")),	
                            "SpringMSDate" = format(as.Date(mig1start, format = dateFormat, origin = "1970-01-01"),dateFormat),	
                            "SpringMSTime" = as.character("00:00:00"),	
                            "SpringME" = format(as.POSIXct(mig1end, format = dateFormat, origin = "1970-01-01"),paste(dateFormat," %0:%0:%0", sep = "")),
                            "SpringMEDate" = format(as.Date(mig1end, format = dateFormat, origin = "1970-01-01"),dateFormat),	
                            "SpringMETime"= format(as.character("00:00:00")),
                            "FallMS" = format(as.POSIXct(mig2start, format = dateFormat, origin = "1970-01-01"),paste(dateFormat," %0:%0:%0", sep = "")),	
                            "FallMSDate" = format(as.Date(mig2start, format = dateFormat, origin = "1970-01-01"),dateFormat),	
                            "FallMSTime" = as.character("00:00:00"),	
                            "FallME" = format(as.POSIXct(mig2end, format = dateFormat),paste(dateFormat," %0:%0:%0", sep = "")),	
                            "FallMEDate" = format(as.Date(mig2end, format = dateFormat, origin = "1970-01-01"),dateFormat),	
                            "FallMETime"= as.character("00:00:00"), 
                            "Notes" = notes
)

MigTab <- MigTab %>% select(Year,AID,	SpringMS,	SpringMSDate,	SpringMSTime,	SpringME,	SpringMEDate,	SpringMETime,	FallMS,	FallMSDate,	FallMSTime,	FallME,	FallMEDate,	FallMETime,	Notes) %>% mutate(SpringMS = paste(SpringMSDate, sep=" ", SpringMSTime), SpringME = paste(SpringMEDate, sep=" ", SpringMETime), FallMS = paste(FallMSDate, sep=" ", FallMSTime), FallME= paste(FallMEDate, sep=" ", FallMETime))

head(MigTab)


#Format date and time columns from Master list
DeerID.Data$StartDate <- as.POSIXct(DeerID.Data$StartDate, format="%m/%d/%Y", origin = "1970-01-01")
DeerID.Data$StopDate <- as.POSIXct(DeerID.Data$StopDate,format="%m/%d/%Y", origin = "1970-01-01")

DeerID.Data$Startdt <- paste(as.character(DeerID.Data$StartDate), DeerID.Data$StartTime, sep=" ")
DeerID.Data$StartDateTime <- as.POSIXct(strptime(DeerID.Data$Startdt, format="%Y-%m-%d %H:%M:%S"), origin = "1970-01-01", tz="MST")

DeerID.Data$Stopdt <- paste(as.character(DeerID.Data$StopDate), DeerID.Data$StopTime, sep=" ")
DeerID.Data$StopDateTime <- as.POSIXct(strptime(DeerID.Data$Stopdt, format="%Y-%m-%d %H:%M:%S"), origin = "1970-01-01", tz="MST")
names(DeerID.Data)

#Remove unncessary columns
DeerID.Data<- DeerID.Data %>% select(AID, Mgtry)
head(DeerID.Data)

#join MigTable and part of DeerID
MigTab_info <- MigTab %>%  left_join(DeerID.Data ,by = "AID")
MigTab_info <- MigTab_info %>% rename("MigratoryStrategy" = "Mgtry") %>% select(Year, AID, MigratoryStrategy, SpringMS, SpringMSDate, SpringMSTime, SpringME, SpringMEDate, SpringMETime, FallMS, FallMSDate, FallMSTime, FallME, FallMEDate, FallMETime, Notes)
MigTab_info <- MigTab_info [ !duplicated(MigTab_info[2:3]), ]
head(MigTab_info)

write.csv(MigTab_info, "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/2023/AllSpringFallMigrationTiming_2023_clean.csv", row.names = F)
###-----------------------------###
## ATTN!!! ## 

#--------now, go into the output file and replace all NA 00:00:00 with NA
#-------- then copy and paste it to the 2014_to_YYYY, double checking that the dates are all OK

