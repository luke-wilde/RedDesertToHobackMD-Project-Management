#Code to extract migrations based on MigrationMapper mig table and the complete dataset
#Authro: Luke Wilde
#Created: 06/14/22
#Major Edits: 01/23/23
# last use 04 14 2023


rm(list = ls())
# ATTN For Fall Migration, need to figure out how to allow for migrations that end in the next year since it uses the id_yr



#-----------------------------#
# Prepare Environment ####
library(sf)
library(sp)
library(tidyverse)
library(fuzzyjoin) #install.packages("fuzzyjoin")
library(lubridate)
library(foreach)
library(doParallel)
library(ranger)
library(kableExtra)
library(snowfall)
library(usethis) 
library(mapview)
library(furrr)

select <- dplyr::select

#load GPS dataset
setwd("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData") # # want to drop Bischof? Then make this "C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData", include it use:  C:/Users/lwilde2/Desktop/RDH Database/Processed Data

load("C:/Users/lwilde2/Documents/Collaborations/RobbMerkle_FenceCrossings/RDH_GPSdata_HS+AC+LW+KM+MK_05.26.2025.RData")

range(all_rdh_data.sf$Date)
#range(rdh_data.sf$Date)

#load migration table
load("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/AllSpringFallMigrationTiming_2011to2024_05262025.RData")
mig <- full_migtab

#quick checks
str(mig %>% filter(AID == 255))
str(all_rdh_data.sf %>% filter(AID == 255))
all_rdh_data.sf %>% dplyr::filter(id_yr == "255_2023") %>% summarise(min = min(POSIXct), max = max(POSIXct))

#--------------------------#
# Prep Data ####

data <- all_rdh_data.sf

# proj <- "+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs+towgs84=0,0,0"
# data <- st_as_sf(data, coords = c("UTM_E", "UTM_N"), crs=proj)

#data <- data %>% st_transform("EPSG:32612")

str(mig)
str(data)

#data %>% group_by(Complete) %>% summarise(n.id_yr = length(unique(id_yr)), n.AID = length(unique(AID))) 



#### ATTN ONLY DO THIS IF PREPPRING FOR BBMM ####
#---------------------------------------------#

#keep only the complete, ie 2 hr
data <- data %>% mutate(Complete = ifelse(diff < (2.2*3600),1,0))
data <- data %>% filter(Complete == 1|id_yr == "255_2016")

#IF ALL YOU WANT IS MIGRATIONS, DONT NEED TO DO THAT!
#---------------------------------------------#

range(data$diff, na.rm = T)



#convert to correct formats
# data$POSIXct <- as.POSIXct(data$POSIXct, "%Y-%m-%d %H:%M:%S", tz = "MST")

#data <- data %>% filter(POSIXct > "2021-01-01 01:00:00")
#data


str(mig)
str(data)

data$Year <- as.character(data$Year)
mig$Year <- as.character(mig$Year)
data$AID <- as.character(data$AID)
mig$AID <- as.character(mig$AID)

unique(mig$Year)

str(mig)
str(data)


data$id_yr <- paste(data$AID,"_",data$Year,sep="")

range(data$Date)
range(na.omit(mig$FallEndDate))


data %>% filter(AID == 255) %>% reframe(n = unique(id_yr))
mig %>% filter(AID == 255) %>% as.data.frame()

#-----------------------#
# Spring ####





#prep spring
mig.SpringMigration <- mig %>% dplyr::filter(!is.na(SpringStartDate))
mig.SpringMigrationOnly <- mig.SpringMigration %>% dplyr::filter(!is.na(SpringEndDate)) 
mig.SpringMigrationPartial <- setdiff(mig.SpringMigration, mig.SpringMigrationOnly)

mig.SpringMigrationOnly <- mig.SpringMigrationOnly %>% arrange(id_yr)
mig.SpringMigrationOnly <- mig.SpringMigrationOnly[!duplicated(c(mig.SpringMigrationOnly$id_yr)),]

mig.SpringMigrationPartial <- mig.SpringMigrationPartial %>% arrange(id_yr)
mig.SpringMigrationPartial <- mig.SpringMigrationPartial[!duplicated(c(mig.SpringMigrationPartial$id_yr)),]


process <- function(df, gps = all_rdh_data.sf, partial = F){

  
  if(partial == F) {
    
    sel <- gps %>% filter(AID == df$AID)
    sel <- sel %>% filter(POSIXct >= df$SpringStartDate & POSIXct <= df$SpringEndDate)
    sel$id_yr <- df$id_yr
    
    return(sel)
    
  } else {
    
    # Separate the event and date for clarity
    tryCatch({df$EventType <- str_extract(df$SpringNotes, "(Died|died|dead|death|End|end|ended|ends|fell)")
    df$DateExtracted <- ifelse(is.na(df$EventType), NA, str_extract(df$SpringNotes, "\\d{4}-\\d{2}-\\d{2}"))}, error = function(e) {df$EventType <- NA
    df$DateExtracted <- NA})
    
    
    sel <- gps %>% filter(AID == df$AID)
    
    sel <- sel %>% filter( case_when(
      is.na(df$DateExtracted) ~ POSIXct >= df$SpringStartDate,
      !is.na(df$DateExtracted) ~ POSIXct >= df$SpringStartDate & POSIXct <= date(df$DateExtracted)
    ))
    
    sel$id_yr <- df$id_yr
    
    return(sel)
  
  }
  
}

#process(df = mig.SpringMigrationOnly[2,], gps = all_rdh_data.sf, partial = F)

plan()
plan(sequential)
plan(multicore, workers= detectCores() - 1)

options(future.globals.maxSize = 15 * 1024^3)  # Set to 15 GiB

start <- Sys.time()
mig.SpringMigrationOnly.sf <- mig.SpringMigrationOnly %>% mutate(rownum = row_number()) %>% group_by(rownum) %>%  group_split() %>% 
  future_map_dfr(~ process(df = ., gps = all_rdh_data.sf, partial = F), .progress = T)
stop <- Sys.time()

print(stop - start)

options(future.globals.maxSize = NULL)  # Unsets any custom value

plan(sequential)
plan()


plan()
plan(sequential)
plan(multicore, workers= detectCores() - 1)

options(future.globals.maxSize = 15 * 1024^3)  # Set to 15 GiB
start <- Sys.time()
mig.SpringMigrationPartial.sf <- mig.SpringMigrationPartial %>% mutate(rownum = row_number()) %>% group_by(rownum) %>%  group_split() %>% 
  future_map_dfr(~ process(df = ., gps = all_rdh_data.sf, partial = T), .progress = TRUE)
stop <- Sys.time()

print(stop - start)

options(future.globals.maxSize = NULL)  # Unsets any custom value

plan(sequential)
plan()

str(mig.SpringMigrationPartial.sf)


### summarise the migrations ####

# keep NAs for those where data ended or animal died
Spring.summary1 <- mig.SpringMigrationOnly.sf %>% mutate(Date = date(Date)) %>% st_drop_geometry() %>% group_by(id_yr) %>% summarise(start = range(Date)[1], stop = range(Date)[2], length = as.numeric(stop-start)) %>% ungroup() 
Spring.summary2 <- mig.SpringMigrationPartial.sf %>% mutate(Date = date(Date))  %>% st_drop_geometry() %>% group_by(id_yr) %>% summarise(start = range(Date)[1], stop = NA, length = NA) %>% ungroup() 

Spring.summary <- bind_rows(Spring.summary1, Spring.summary2) %>% arrange(id_yr)
rm(Spring.summary1, Spring.summary2)

mig.SpringMigration.sf <- bind_rows(mig.SpringMigrationOnly.sf, mig.SpringMigrationPartial.sf) %>% arrange(AID, POSIXct)
rm(mig.SpringMigration,mig.SpringMigrationOnly.sf, mig.SpringMigrationPartial.sf,mig.SpringMigrationOnly, mig.SpringMigrationPartial)

#-----------------------#
# Fall ####


#prep Fall
mig.FallMigration <- mig %>% dplyr::filter(!is.na(FallStartDate))
mig.FallMigrationOnly <- mig.FallMigration %>% dplyr::filter(!is.na(FallEndDate)) 
mig.FallMigrationPartial <- setdiff(mig.FallMigration, mig.FallMigrationOnly)

mig.FallMigrationOnly <- mig.FallMigrationOnly %>% arrange(id_yr)
mig.FallMigrationOnly <- mig.FallMigrationOnly[!duplicated(c(mig.FallMigrationOnly$id_yr)),]

mig.FallMigrationPartial <- mig.FallMigrationPartial %>% arrange(id_yr)
mig.FallMigrationPartial <- mig.FallMigrationPartial[!duplicated(c(mig.FallMigrationPartial$id_yr)),]


processF <- function(df, gps = all_rdh_data.sf, partial = F){
  
  
  if(partial == F) {
    
    sel <- gps %>% filter(AID == df$AID)
    sel <- sel %>% filter(POSIXct >= df$FallStartDate & POSIXct <= df$FallEndDate)
    sel$id_yr <- df$id_yr
    
    return(sel)
    
  } else {
    
    # Separate the event and date for clarity
    tryCatch({df$EventType <- str_extract(df$FallNotes, "(Died|died|dead|death|End|end|ended|ends|fell)")
    df$DateExtracted <- ifelse(is.na(df$EventType), NA, str_extract(df$FallNotes, "\\d{4}-\\d{2}-\\d{2}"))}, error = function(e) {df$EventType <- NA
    df$DateExtracted <- NA})
    
    
    sel <- gps %>% filter(AID == df$AID)
    
    sel <- sel %>% filter( case_when(
      is.na(df$DateExtracted) ~ POSIXct >= df$FallStartDate,
      !is.na(df$DateExtracted) ~ POSIXct >= df$FallStartDate & POSIXct <= date(df$DateExtracted)
    ))
    
    sel$id_yr <- df$id_yr
    
    return(sel)
    
  }
  
}

#process(df = mig.FallMigrationOnly[2,], gps = all_rdh_data.sf, partial = F)

plan()
plan(sequential)
plan(multicore, workers= detectCores() - 1)

options(future.globals.maxSize = 15 * 1024^3)  # Set to 15 GiB

start <- Sys.time()
mig.FallMigrationOnly.sf <- mig.FallMigrationOnly %>% mutate(rownum = row_number()) %>% group_by(rownum) %>%  group_split() %>% 
  future_map_dfr(~ process(df = ., gps = all_rdh_data.sf, partial = F), .progress = T)
stop <- Sys.time()

print(stop - start)

options(future.globals.maxSize = NULL)  # Unsets any custom value

plan(sequential)
plan()


plan()
plan(sequential)
plan(multicore, workers= detectCores() - 1)

options(future.globals.maxSize = 15 * 1024^3)  # Set to 15 GiB
start <- Sys.time()
mig.FallMigrationPartial.sf <- mig.FallMigrationPartial %>% mutate(rownum = row_number()) %>% group_by(rownum) %>%  group_split() %>% 
  future_map_dfr(~ process(df = ., gps = all_rdh_data.sf, partial = T), .progress = TRUE)
stop <- Sys.time()

print(stop - start)

options(future.globals.maxSize = NULL)  # Unsets any custom value

plan(sequential)
plan()




### summarise the migrations ####

# keep NAs for those where data ended or animal died
Fall.summary1 <- mig.FallMigrationOnly.sf %>% mutate(Date = date(Date)) %>% st_drop_geometry() %>% group_by(id_yr) %>% summarise(start = range(Date)[1], stop = range(Date)[2], length = as.numeric(stop-start)) %>% ungroup() 
Fall.summary2 <- mig.FallMigrationPartial.sf %>% mutate(Date = date(Date))  %>% st_drop_geometry() %>% group_by(id_yr) %>% summarise(start = range(Date)[1], stop = NA, length = NA) %>% ungroup() 

Fall.summary <- bind_rows(Fall.summary1, Fall.summary2) %>% arrange(id_yr)
rm(Fall.summary1, Fall.summary2)

mig.FallMigration.sf <- bind_rows(mig.FallMigrationOnly.sf, mig.FallMigrationPartial.sf) %>% arrange(AID, POSIXct)
rm(mig.FallMigration,mig.FallMigrationOnly.sf, mig.FallMigrationPartial.sf,mig.FallMigrationOnly, mig.FallMigrationPartial)




#-----------------------#
# Summer ####

#------------------------#
# Winter ####






#-------------------------------------------------------------#


# 
# data2122 <- data %>% filter(Year %in% c(2021,2022))
# data23 <- data %>% filter(Year %in% c(2023)) %>% mutate(id_yr = paste(AID,sep="_","2022"))
# 
# data <- rbind(data2122, data23)


data[,c("x","y")] <- st_coordinates(data)
proj <- st_crs(data)$input


data.df <- data %>%  st_drop_geometry() %>% as.data.frame()

data.df <- data.df %>% arrange(id_yr, Mgtry)
mig <- mig %>% arrange(id_yr)

head(data.df)
head(mig)

#--------------------------------------#
# prepare the initial datasets ####

# ATTN: The join has problems with collars that do not have a StopDate, so split into active and inactive. 

# first those that completed both
vars <- c("SpringMSDate", "SpringMEDate", "FallMSDate", "FallMEDate")
# mig.SpringAndFallMigration <- mig %>% dplyr::filter(!is.na(SpringMSDate) & !is.na(SpringMEDate) & !is.na(FallMSDate) & !is.na(FallMEDate))
#next animals that died on summer or remained there overwinter
mig.SpringMigration <- mig %>% dplyr::filter(!is.na(SpringMSDate))
mig.SpringMigrationOnly <- mig.SpringMigration %>% dplyr::filter(!is.na(SpringMEDate)) 
mig.SpringMigrationPartial <- setdiff(mig.SpringMigration, mig.SpringMigrationOnly)

mig.SpringMigrationOnly <- rbind(mig.SpringMigrationOnly, mig.SpringMigrationPartial)

#finally, animals that were captured on summer or fall range
mig.FallMigration <- mig %>% dplyr::filter(!is.na(FallMSDate))
mig.FallMigrationOnly <- mig.FallMigration %>% dplyr::filter(!is.na(FallMEDate)) 
mig.FallMigrationPartial <- setdiff(mig.FallMigration, mig.FallMigrationOnly)

mig.FallMigrationOnly <- rbind(mig.FallMigrationOnly, mig.FallMigrationPartial)

# head(mig.SpringAndFallMigration); nrow(mig.SpringAndFallMigration); str(mig.SpringAndFallMigration)
# head(mig.SpringMigrationOnly);  nrow(mig.SpringMigrationOnly); str(mig.SpringMigrationOnly)

mig.SpringMigrationOnly <- mig.SpringMigrationOnly %>% arrange(id_yr)
mig.SpringMigrationOnly <- mig.SpringMigrationOnly[!duplicated(c(mig.SpringMigrationOnly$id_yr)),]

# table(is.na(mig.SpringMigrationOnly$SpringMSDate))
# table(is.na(mig.SpringMigrationOnly$SpringMEDate))
# length(unique(mig.SpringMigrationOnly$id_yr))
# unique(mig.SpringMigrationOnly$id_yr)
# 
# str(mig.SpringMigrationOnly %>% filter(AID == 255))

mig.FallMigrationOnly <- mig.FallMigrationOnly %>% arrange(id_yr)
mig.FallMigrationOnly <- mig.FallMigrationOnly[!duplicated(c(mig.FallMigrationOnly$id_yr)),]

# table(is.na(mig.FallMigrationOnly$FallMSDate))
# table(is.na(mig.FallMigrationOnly$FallMEDate))
# length(unique(mig.FallMigrationOnly$id_yr))
#unique(mig.FallMigrationOnly$id_yr)


unique(mig.SpringMigrationOnly$id_yr)

#clear space and set memory limits
gc()



#Parallel loops to clip migrations#

#---------------------------------#
## complete spring at least ####
library(parallel)
library(snow)
library(doParallel)
library(doSNOW)
library(doRNG)

#define loop variable
ids <- unique(mig.SpringMigrationOnly$AID)
names(ids) <- NULL
row.names(ids) <- NULL

#Loop over animals and extract greenscape metrics
#First, set up parallel processing
no_cores <- detectCores() - 1
cat(no_cores, " cores detected.")  

cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl) #Use doSNOW instead of doParallel to create progress bar

#Set up progress bar parameters and function
#Progress bar will clarify the percentage of complete BBMMs
pb <- txtProgressBar(min=1, max=length(ids), style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

mig.SpringMigration <- foreach(i = ids, #
                               .packages = c('raster', 'terra','sf','stringr','lubridate','dplyr','tidyverse'),
                               .combine = 'rbind',
                               .errorhandling='remove',
                               .options.multicore=list(set.seed=TRUE),
                               .options.snow=opts
                               
) %dorng% {
  i = 242
  
  t <- dplyr::filter(data.df, data.df$AID == i)
  tt <- dplyr::filter(mig.SpringMigrationOnly, mig.SpringMigrationOnly$AID == i)
  for(j in 1:nrow(tt)){
j=1
  s <- tt[j,]
  
  if (is.na(s$SpringMEDate)) {
    dplyr::filter(t, t$Date >= s$SpringMSDate)
  } else {
    dplyr::filter(t, t$Date >= s$SpringMSDate & t$Date <= s$SpringMEDate)
  }
  
}
  
}

head(mig.SpringMigration)

#stop the cluster
stopCluster(cl)
showConnections()


Spring.summary <- mig.SpringMigration %>% group_by(id_yr) %>% summarise(start = range(Date)[1], stop = range(Date)[2], length = as.numeric(stop-start)) %>% ungroup() 






#---------------------------------#
## complete fall at least ####
library(parallel)
library(snow)
library(doParallel)
library(doSNOW)
library(doRNG)

#define loop variable
ids <- unique(mig.FallMigrationOnly$id_yr)
names(ids) <- NULL
row.names(ids) <- NULL

#Loop over animals and extract greenscape metrics
#First, set up parallel processing
no_cores <- detectCores() - 1
cat(no_cores, " cores detected.")  

cl <- makeSOCKcluster(no_cores)
registerDoSNOW(cl) #Use doSNOW instead of doParallel to create progress bar

#Set up progress bar parameters and function
#Progress bar will clarify the percentage of complete BBMMs
pb <- txtProgressBar(min=1, max=length(ids), style=3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress=progress)

mig.FallMigration <- foreach(i = ids, #
                               .packages = c('raster', 'terra','sf','stringr','lubridate','dplyr','tidyverse'),
                               .combine = 'rbind',
                               .errorhandling='remove',
                               .options.multicore=list(set.seed=TRUE),
                               .options.snow=opts
                               
) %dorng% {
  t <- dplyr::filter(data.df, data.df$id_yr == i)
  s <- dplyr::filter(mig.FallMigrationOnly, mig.FallMigrationOnly$id_yr == i)
  
  if (is.na(s$FallMEDate)) {
    dplyr::filter(t, t$Date >= s$FallMSDate)
  } else {
    dplyr::filter(t, t$Date >= s$FallMSDate & t$Date <= s$FallMEDate)
  }
}


head(mig.FallMigration)

#stop the cluster
stopCluster(cl)
showConnections()


Fall.summary <- mig.FallMigration %>% group_by(id_yr) %>% summarise(start = range(Date)[1], stop = range(Date)[2], length = as.numeric(stop-start)) %>% ungroup()


Fall.summary$id_yr


#------------------------------#
# Partials - Extract dates and keywords ####
# 
dates <- as.vector(sapply(mig.SpringMigrationPartial$Notes, function(x) {
  date <- str_extract(x, "\\b\\d{1,2}/\\d{1,2}/\\d{4}\\b")
  ifelse(is.na(date), NA, date)
}))

keywords <- as.vector(sapply(mig.SpringMigrationPartial$Notes, function(x) {
  keyword <- str_extract(x, "\\b(Died|Collar|collar|died)\\b")
  ifelse(is.na(keyword), NA, keyword)
}))

# Create a data frame to combine the extracted information
spring.incomp <- data.frame(Notes = mig.SpringMigrationPartial$id_yr, Dates = dates, Keywords = keywords) %>% mutate(Dates = as.Date(mdy(Dates)))


# Extract dates and keywords
dates <- as.vector(sapply(mig.FallMigrationPartial$Notes, function(x) {
  date <- str_extract(x, "\\b\\d{1,2}/\\d{1,2}/\\d{4}\\b")
  ifelse(is.na(date), NA, date)
}))

keywords <- as.vector(sapply(mig.FallMigrationPartial$Notes, function(x) {
  keyword <- str_extract(x, "\\b(Died|Collar|collar|died)\\b")
  ifelse(is.na(keyword), NA, keyword)
}))


# Create a data frame to combine the extracted information
fall.incomp <- data.frame(Notes = mig.FallMigrationPartial$id_yr, Dates = dates, Keywords = keywords) %>% mutate(Dates = as.Date(mdy(Dates)))

#----------------------------#
# Process the data ####

mig.SpringMigration.sf <- st_as_sf(mig.SpringMigration, coords = c("x", "y"), crs = proj)
mig.FallMigration.sf <- st_as_sf(mig.FallMigration, coords = c("x", "y"), crs = proj)

#incorporate incomplete migration notes to summaries
Spring.summary <- Spring.summary %>% mutate(Note = NA) %>%
  left_join(spring.incomp %>% rename(id_yr = Notes), by = "id_yr") %>%
  mutate(Note = ifelse(!is.na(Dates) & !is.na(Keywords), paste(Dates, Keywords, sep = ", "), NA),length = ifelse(is.na(Dates), length, NA), stop = date(ifelse(is.na(Dates), as.character(stop), as.character(Dates)))) %>% select(!c(Dates, Keywords))


#correct for partial 255_2018
Spring.summary[which(Spring.summary$id_yr == "255_2018"),"start"] <- NA
Spring.summary[which(Spring.summary$id_yr == "255_2018"),"length"] <- NA


Fall.summary <- Fall.summary %>% mutate(Note = NA) %>%
  left_join(fall.incomp %>% rename(id_yr = Notes), by = "id_yr") %>%
  mutate(Note = ifelse(!is.na(Dates) & !is.na(Keywords), paste(Dates, Keywords, sep = ", "), NA),length = ifelse(is.na(Dates), length, NA), stop = date(ifelse(is.na(Dates), as.character(stop), as.character(Dates)))) %>% select(!c(Dates, Keywords))

#also clean up the dates in the sf files
mig.SpringMigration.sf <- mig.SpringMigration.sf %>% left_join(spring.incomp %>% rename(id_yr = Notes), by = "id_yr") %>% filter(is.na(Dates) | Date <= Dates) %>%
  select(-Dates, -Keywords)
mig.FallMigration.sf <- mig.FallMigration.sf %>% left_join(fall.incomp %>% rename(id_yr = Notes), by = "id_yr") %>% filter(is.na(Dates) | Date <= Dates) %>%
  select(-Dates, -Keywords)

#------------------------------#
# Winter ranges ####

#first have to deal with the biological year
joinedW <- rdh_data.sf %>% st_drop_geometry() %>% mutate(id_yr = ifelse(Month >= 8, paste(AID, Year + 1,sep="_"), id_yr)) %>%
  left_join(Spring.summary %>%
              mutate(id_yr_copy = id_yr) %>%
              separate(id_yr_copy, into = c("AID", "Year"), sep = "_"), by = "id_yr", suffix = c("", "_spring")) %>%
  left_join(Fall.summary %>%
              mutate(id_yr_copy = id_yr) %>%
              separate(id_yr_copy, into = c("AID", "Year"), sep = "_") %>% mutate(Year = as.numeric(Year), id_yr = paste(AID, Year + 1, sep="_")), by = "id_yr", suffix = c("", "_fall"))

WinterRange <- joinedW %>% ungroup() %>% group_by(AID_spring, Year_spring) %>% filter((is.na(stop_fall) & Date < start) | (Date > stop_fall & Date < start)) %>% mutate(id_wint = paste(AID_spring, Year_spring, sep="_")) %>% ungroup() %>% dplyr::select(-id_yr) 

WinterRange.sf <- WinterRange %>% mutate(UTM_N1 = UTM_N, UTM_E1 = UTM_E) %>% st_as_sf(coords = c("UTM_E1", "UTM_N1"), crs = st_crs(rdh_data.sf)) %>% select(names(mig.SpringMigration.sf)[1:16],id_wint,names(mig.SpringMigration.sf)[18:23])


#------------------------------#
# Summer ranges ####
joinedS <- rdh_data.sf %>% st_drop_geometry() %>%
  left_join(Spring.summary %>%
              mutate(id_yr_copy = id_yr) %>%
              separate(id_yr_copy, into = c("AID", "Year"), sep = "_"), by = "id_yr", suffix = c("", "_spring")) %>%
  left_join(Fall.summary %>%
              mutate(id_yr_copy = id_yr) %>%
              separate(id_yr_copy, into = c("AID", "Year"), sep = "_"), by = "id_yr", suffix = c("", "_fall"))

SummerRange <- joinedS %>% group_by(id_yr) %>%
  filter((is.na(start_fall) & Date > stop) |
           (Date > stop & Date < start_fall))

SummerRange.sf <- SummerRange %>% mutate(UTM_N1 = UTM_N, UTM_E1 = UTM_E) %>% st_as_sf(coords = c("UTM_E1", "UTM_N1"), crs = st_crs(rdh_data.sf)) %>% select(names(mig.SpringMigration.sf))

#------------------------------#
# save files ####

save(mig.FallMigration.sf, mig.SpringMigration.sf, Spring.summary, Fall.summary, file = "C:/Users/lwilde2/Desktop/RDH Database/Processed Data/RDH_AllMigrations_2014t2023_20241126.RData")

save(WinterRange.sf, SummerRange.sf, file = "C:/Users/lwilde2/Desktop/RDH Database/Processed Data/RDH_AllSeasonalRanges_2014t2023_20241126.RData")

#-------------------------------#
# Inspect the output ####

rm(list = ls())
load("C:/Users/lwilde2/Desktop/RDH Database/Processed Data/RDH_AllMigrations_2014t2023_20240821.RData")

View(Spring.summary)
ggplot(mig.SpringMigration.sf %>% filter(Year >= 2023)) + geom_sf()


#----------------------------------#
#### write individual id_yr files ####
# 
# #export fall
# setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/AllYears")
# 
# for(i in unique(mig.FallMigration.sf$id_yr)){
#   #i = unique(mig.SpringMigration.sf$id_yr)[1]
#   file <- paste(as.character(i),".RData",sep="")
#   folder = paste(getwd(),"/Fall/",sep="")
#   
#   t <- mig.SpringMigration.sf %>% filter(id_yr == i)
#   
#   save(t, file = paste(folder,file,sep=""))
#   
# }
# 
# #export spring
# setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/AllYears")
# 
# for(i in unique(mig.SpringMigration.sf$id_yr)){
#   #i = unique(mig.SpringMigration.sf$id_yr)[1]
#   file <- paste(as.character(i),".RData",sep="")
#   folder = paste(getwd(),"/Spring/",sep="")
#   
#   t <- mig.SpringMigration.sf %>% filter(id_yr == i)
#   
#   save(t, file = paste(folder,file,sep=""))
#   
# }
# 






