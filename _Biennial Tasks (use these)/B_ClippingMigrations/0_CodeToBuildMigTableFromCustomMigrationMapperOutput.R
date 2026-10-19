# Code to combine mig sequences and export migtab like MigrationMapper
## Requires that there are extracted sequences using my custom functions

#05.13.2025
#LRW

rm(list = ls())

#--------------------#
# Load and prep environment ####
# List of packages
packages <- c(
  "tidyverse", "sf", "lubridate")


# Install missing packages
installed_packages <- installed.packages()
for (pkg in packages) {
  if (!(pkg %in% installed_packages[, "Package"])) {
    install.packages(pkg)
  }
}

# Load all packages
lapply(packages, library, character.only = T)

library(devtools)
#install_github("ropensci/prism")

#correct select so that dplyr is preferred
select <- dplyr::select
summarise <- dplyr::summarise
rename <- dplyr::rename
filter <- dplyr::filter

#-------------------------#
## user inputs here ####
#-------------------------#

#bring in what they used
load("G:/My Drive/FileTransfer/Spring2025_PBennett_FissFus/MigrationMapping/ShinyAppImage_RDHdata_All_v2.RData")

# Set the directory containing the .RData files
data_dir <- "G:/My Drive/FileTransfer/Spring2025_PBennett_FissFus/MigrationMapping/Outs_Full_redo"

#meta info
DEERID_new <- "C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_15MARCH2026.csv"

#load MigMapp example 
example <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/2023/RDH_SpringAndFallMigrations_2023.csv")

#-------------------------#
#-------------------------#


#--------------------#
# combine extracted data into one object ####

rm(rdh_migtab)

# Get a list of all .RData files
rdata_files <- list.files(data_dir, pattern = "\\.RData$", full.names = T)

# Function to load .RData and extract the object
load_rdata <- function(file) {
  env <- new.env()
  load(file, envir = env)
  get(ls(env)[1], env)  # Assuming each .RData file contains a single data frame
}

# Load and combine all data frames
rdh_migtab <- map_dfr(rdata_files, load_rdata)

# View the result
head(rdh_migtab); tail(rdh_migtab)


rdh_migtab <- rdh_migtab %>% mutate(rowID = row_number())
# dup <- rdh_migtab %>%
#   count(BioYear) %>%
#   filter(n > 1) %>% as.data.frame()

# #commented out 05.26.2025
# #pull out duplicates
# rdh_migtab1 <- rdh_migtab %>% filter(!BioYear %in% dup$BioYear)
# rdh_migtab2 <- rdh_migtab %>% filter(BioYear %in% dup$BioYear) %>% arrange(BioYear)
# 
# 
# #correct with those that I redid
# redo_LRW_05132025 <- c("260_2017", "280_2020", "284_2019", "311_2017", "370_2018", "410_2021", "413_2019" ,"436_2020" ,"446_2021" ,"446_2022" ,"456_2020", "463_2021", "464_2021","503_2021", "509_2021" ,"514_2021", "521_2023" ,"525_2024", "537_2021" ,"543_2021" ,"547_2022", "601_2024", "608_2023", "609_2022", "657_2023", "682_2024")
# 
#
#keep the latter for those I redid and visually inspect those I did not
# rdh_migtab3 <- rdh_migtab2 %>% filter(BioYear %in% redo_LRW_05132025) %>% group_by(BioYear) %>% slice_tail(n=1)
# rdh_migtab2 <- rdh_migtab2 %>% filter(!BioYear %in% redo_LRW_05132025)
#
# #after visual inspection
# rdh_migtab2 <- rdh_migtab2[c(1,4,6,8,10,11,14,15),]
# 
# #manual fixes (one time only -- 05.14.2025)
# rdh_migtab2[2,"SpringStartDate"] <- date("2023-05-23") #taken from old Mig Mapp table
# rdh_migtab2[2,"SpringEndDate"] <- date("2023-07-25") #taken from old Mig Mapp table
# 
# rdh_migtab <- bind_rows(rdh_migtab1, rdh_migtab2, rdh_migtab3)

# rdh_migtab1 <- rdh_migtab %>% filter(!BioYear %in% dup$BioYear)
# rdh_migtab2 <- rdh_migtab %>% filter(BioYear %in% dup$BioYear) %>% group_by(BioYear) %>% slice_tail(n=1)
# 
# rm(rdh_migtab)
# 
# rdh_migtab <- bind_rows(rdh_migtab1, rdh_migtab2)

#these ones are still missing, once finished should be close to 0
ids <- unique(RDH_nsd$burst)

setdiff(ids, rdh_migtab$BioYear) #missing any?



rm(elev, xy, RDH_nsd, RDH_lt, RDH_m_sp, all_rdh_data.sf, bioyr, date, id, rdata_files)


#--------------------#
# reformat to match migtim ####

## first format and prep metadata ####

DeerID.Data <- read.csv(DEERID_new)


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


# ## then, reshape mig table ####
# dateFormat <- "%m/%d/%Y" #"%Y-%m-%d" # ATTN: this can differ depending on the settings
# 
# MigTab <- example %>% mutate("Year" = bioYearFull, "AID" = newUid,		
#                             "SpringMS" = format(as.POSIXct(mig1start, format = dateFormat, origin = "1970-01-01"),paste(dateFormat," %0:%0:%0", sep = "")),	
#                             "SpringMSDate" = format(as.Date(mig1start, format = dateFormat, origin = "1970-01-01"),dateFormat),	
#                             "SpringMSTime" = as.character("00:00:00"),	
#                             "SpringME" = format(as.POSIXct(mig1end, format = dateFormat, origin = "1970-01-01"),paste(dateFormat," %0:%0:%0", sep = "")),
#                             "SpringMEDate" = format(as.Date(mig1end, format = dateFormat, origin = "1970-01-01"),dateFormat),	
#                             "SpringMETime"= format(as.character("00:00:00")),
#                             "FallMS" = format(as.POSIXct(mig2start, format = dateFormat, origin = "1970-01-01"),paste(dateFormat," %0:%0:%0", sep = "")),	
#                             "FallMSDate" = format(as.Date(mig2start, format = dateFormat, origin = "1970-01-01"),dateFormat),	
#                             "FallMSTime" = as.character("00:00:00"),	
#                             "FallME" = format(as.POSIXct(mig2end, format = dateFormat),paste(dateFormat," %0:%0:%0", sep = "")),	
#                             "FallMEDate" = format(as.Date(mig2end, format = dateFormat, origin = "1970-01-01"),dateFormat),	
#                             "FallMETime"= as.character("00:00:00"), 
#                             "Notes" = notes
# )
# 
# MigTab <- MigTab %>% select(Year,AID,	SpringMS,	SpringMSDate,	SpringMSTime,	SpringME,	SpringMEDate,	SpringMETime,	FallMS,	FallMSDate,	FallMSTime,	FallME,	FallMEDate,	FallMETime,	Notes) %>% mutate(SpringMS = paste(SpringMSDate, sep=" ", SpringMSTime), SpringME = paste(SpringMEDate, sep=" ", SpringMETime), FallMS = paste(FallMSDate, sep=" ", FallMSTime), FallME= paste(FallMEDate, sep=" ", FallMETime))
# 
# head(MigTab)
# names(MigTab)

#--------------------#
# SKIPPED for now: join MigTable and part of DeerID ####
# MigTab_info <- MigTab %>%  left_join(DeerID.Data ,by = "AID")
# MigTab_info <- MigTab_info %>% rename("MigratoryStrategy" = "Mgtry") %>% select(Year, AID, MigratoryStrategy, SpringMS, SpringMSDate, SpringMSTime, SpringME, SpringMEDate, SpringMETime, FallMS, FallMSDate, FallMSTime, FallME, FallMEDate, FallMETime, Notes)
# MigTab_info <- MigTab_info [ !duplicated(MigTab_info[2:3]), ]
# head(MigTab_info)

# old write.csv(MigTab_info, "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/2023/AllSpringFallMigrationTiming_2023_clean.csv", row.names = F)



#--------------------#
# Redo with the new data ####

str(rdh_migtab)
rdh_migtab <- rdh_migtab %>%  mutate(across(everything(), as.character))%>%  mutate(across(everything(), ~replace(., . == "1899-01-01", NA))) %>% as_tibble()

rdh_migtab <- rdh_migtab %>% mutate(across(c(SpringStartDate,SpringEndDate,FallStartDate,FallEndDate,SpringFalsestartStartDate,SpringFalsestartEndDate,FallFalsestartStartDate,FallFalsestartEndDate,SummerWalkaboutStartDate,SummerWalkaboutEndDate), ~ ymd(.)))  %>% as_tibble() #%>%
 # mutate(across(c(CompleteSpring, SpringFalsestart, SummerWalkabout, CompleteFall, FallFalsestart), ~ replace_na(.x, "No"))) %>% as_tibble()
head(rdh_migtab)

table(rdh_migtab$SpringFalsestart, rdh_migtab$MigrationStrategy)

rdh_migtab <- rdh_migtab %>% separate(BioYear, into = c("AID", "Year"), sep = "_", remove = F) %>%  mutate(across(c(AID, Year), as.numeric)) %>% select(-c(BioYear, Year)) %>% as_tibble()



# #commented out 05.26.2025
# #----------------------#
# # One time fix of the NAs to check them ####
# 
# 
# checkSpring <- rdh_migtab %>% filter(is.na(SpringStartDate) | is.na(SpringEndDate)) %>% distinct(id_yr)
# checkSpring <- checkSpring$id_yr
# 
# save(checkSpring, file = "C:/Users/lwilde2/Downloads/SpringCheck_NAsReal_MigMapp.RData")
# 

nm <- names(rdh_migtab)

#----------------------------#
## remove duplicates throughout ####

### spring ####
spring_migtab <- rdh_migtab %>% select(nm[c(1:10,23:25)]) %>% mutate(Year = year(SpringStartDate)) %>% filter(CompleteSpring %in% c("Yes", "Partial"))

accidental.fall <- spring_migtab %>% mutate(bioyr = paste0(AID, "_", Year)) %>% filter(month(SpringStartDate) > 8)

spring_migtab <- spring_migtab %>% filter(!rowID %in% c(accidental.fall$rowID))

count <- dplyr::count
#check for duplicates
dups <- spring_migtab %>%
  dplyr::count(AID, Year) %>%
  filter(n > 1) %>% mutate(bioyr = paste0(AID, "_", Year))

spring_migtab1 <- spring_migtab %>% mutate(bioyr = paste0(AID, "_", Year)) %>% filter(!bioyr %in% dups$bioyr)
spring_migtab2 <- spring_migtab %>% mutate(bioyr = paste0(AID, "_", Year)) %>% filter(bioyr %in% dups$bioyr) %>% arrange(bioyr, rowID) 
rm(spring_migtab)
spring_migtab2 %>% View()

#manual selection
spring_migtab3 <- spring_migtab2 %>% group_by(bioyr) %>% filter(rowID == max(rowID))

spring_migtab <- bind_rows(spring_migtab1,spring_migtab3)

### fall ####
fall_migtab <- rdh_migtab %>% select(nm[c(1,14:25)]) %>% mutate(Year = year(FallStartDate)) %>% filter(CompleteFall %in% c("Yes", "Partial"))

accidental.spring <- fall_migtab %>% filter(month(FallStartDate) < 7 & month(FallStartDate) > 3) %>% mutate(bioyr = paste0(AID, "_", Year))

fall_migtab <- fall_migtab %>% filter(!rowID %in% c(accidental.spring$rowID))


#check for duplicates
dups <- fall_migtab %>%
  count( AID, Year) %>%
  filter(n > 1) %>% mutate(bioyr = paste0(AID, "_", Year))

fall_migtab1 <- fall_migtab %>% mutate(bioyr = paste0(AID, "_", Year)) %>% filter(!bioyr %in% dups$bioyr)
fall_migtab2 <- fall_migtab %>% mutate(bioyr = paste0(AID, "_", Year)) %>% filter(bioyr %in% dups$bioyr) %>% arrange(bioyr, rowID)
rm(fall_migtab)
fall_migtab2 %>% View()

#manual selection
fall_migtab3 <- fall_migtab2 %>% group_by(bioyr) %>% filter(rowID == max(rowID))

fall_migtab <- bind_rows(fall_migtab1,fall_migtab3)


### summer ####
summer_migtab <- rdh_migtab %>% select(nm[c(1,11:13,23:25)]) %>% mutate(Year = year(SummerWalkaboutStartDate)) %>% filter(SummerWalkabout == "Yes")

#check for duplicates
dups <- summer_migtab %>%
  count( AID, Year) %>%
  filter(n > 1) %>% mutate(bioyr = paste0(AID, "_", Year))

summer_migtab1 <- summer_migtab %>% mutate(bioyr = paste0(AID, "_", Year)) %>% filter(!bioyr %in% dups$bioyr)
summer_migtab2 <- summer_migtab %>% mutate(bioyr = paste0(AID, "_", Year)) %>% filter(bioyr %in% dups$bioyr) %>% arrange(bioyr, rowID)
rm(summer_migtab)
summer_migtab2 %>% View()

# #manual selection - NOT RUN IN THIS CASE
# summer_migtab3 <- summer_migtab2 %>% group_by(bioyr) %>% filter(rowID == max(rowID))

summer_migtab <- bind_rows(summer_migtab1,summer_migtab2)
summer_migtab %>% View()

### replace accidentals - not needed in 2026 ####
accidental.fall %>% filter(!bioyr %in% fall_migtab$bioyr)

#----------------------------#
## rejoin and remove duplicates ####

full_migtab <- spring_migtab %>% select(-c(rowID)) %>% full_join(.,fall_migtab  %>% select(-c(rowID, MigrationStrategy, BehaviorFlags)), by = c("bioyr", "AID", "Year")) %>% left_join(.,summer_migtab  %>% select(-c(rowID, MigrationStrategy, BehaviorFlags)), by = c("bioyr", "AID", "Year")) %>% distinct(.keep_all=T) %>% arrange(AID, Year)

setdiff(fall_migtab$id_yr, spring_migtab$id_yr) %>% length()
setdiff(spring_migtab$id_yr, fall_migtab$id_yr) %>% length()




#check for duplicates
dup <- full_migtab %>%
  count(bioyr, AID, Year) %>%
  filter(n > 1)

full_migtab %>% filter(bioyr %in% dup$bioyr) %>% arrange(bioyr) %>% View()

setdiff(fall_migtab$id_yr, spring_migtab$id_yr) %>% length()
setdiff(spring_migtab$id_yr, fall_migtab$id_yr) %>% length()


#--------------------#
# Save and move on ####




save(full_migtab, file = "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/AllSpringFallMigrationTiming_2011to2025_05122026.RData")


#--------------------#
# Go in and manually fix incorrect missing data ####




#--------------------#
# ####
#--------------------#
# ####
#--------------------#
# ####