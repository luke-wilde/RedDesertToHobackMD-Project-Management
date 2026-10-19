#Code to extract migrations based on MigrationMapper mig table and the complete dataset
#Authro: Luke Wilde
#Created: 06/14/22
#Major Edits: 01/23/23
# last use 04 14 2023


#ATTN ------------------------------#
#this is actually way faster in sequential! But do need to up the max mem size still
#-----------------------------------#


rm(list = ls())
# ATTN For Fall Migration, need to figure out how to allow for migrations that end in the next year since it uses the id_yr


#devtools::install_github("stan-dev/rstanarm",ref = "feature/survival", build_vignettes = FALSE)

# List of packages
packages <- c(
  "tidyverse", "sf", "lsa", "lubridate", "mapview", "forecast",
  "MoveTools", "ggthemes", "grid", "terra", "raster", "rptR", "ggridges",
  "arm", "corrplot", "patchwork", "randtests", "stats", "merTools",
  "RColorBrewer", "DHARMa", "kohonen", "mstate", "mclust", "dbscan",
  "foreach", "doParallel", "mgcv", "MerkleLabGIS","stringr","circular","MASS", "devtools","prism","future.apply","adehabitatHR","dtw","ctmm","prism","plotly","purrr","brms","rstan","coda","tidybayes","performance","gridExtra","cowplot","ggplotify","see","qqplotr","mailR","rJava","parallel","progressr","furrr","amt","geoR","beepr","BayesSurvival"
)


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

nc <- (detectCores() - 1)
#lyr <- bucket()


#load GPS dataset
load("G:/Other computers/My Laptop/RDH Database/RD2H_GPSCollarData/RDH_GPSdata_HS+AC+LW+KM+MK_04.15.2026.RData")

range(all_rdh_data.sf$Date)


#load migration table
load("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/AllSpringFallMigrationTiming_2011to2025_05122026.RData")
mig <- full_migtab
rm(full_migtab)

# #quick checks
# str(mig %>% filter(AID == 255))
# str(all_rdh_data.sf %>% filter(AID == 255))
# all_rdh_data.sf %>% dplyr::filter(id_yr == "255_2023") %>% summarise(min = min(POSIXct), max = max(POSIXct))

#find missing ones
# load("C:/Users/lwilde2/Desktop/RDH Database/Processed Data/RDH_AllMigrations_2014t2023_20241126.RData")
# oldFall.summary <- Fall.summary
# oldSpring.summary <- Spring.summary
# oldmig.FallMigration.sf <- mig.FallMigration.sf
# oldmig.SpringMigration.sf <- mig.SpringMigration.sf
# 
# rm(Fall.summary, Spring.summary, mig.FallMigration.sf, mig.SpringMigration.sf)

#--------------------------#
# Prep Data ####

data <- all_rdh_data.sf

str(mig)
str(data)

# #check sample sizes
#data %>% group_by(Complete) %>% summarise(n.id_yr = length(unique(id_yr)), n.AID = length(unique(AID))) 





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


#fix timezone issues
mig <- mig  %>%
  mutate(across(
    where(~ inherits(.x, "Date")),
    ~ as.POSIXct(.x)
  )) %>%
  mutate(across(
    where(~ inherits(.x, "POSIXct")),
    ~ force_tz(.x, tzone = "MST")
  )) %>% 
  mutate(across(
    where(~ inherits(.x, "POSIXct")),
    ~ date(.x)
  ))

all_rdh_data.sf <- all_rdh_data.sf %>% mutate(Date = date(POSIXct), Month = month(POSIXct))
  

#-----------------------#
# Spring ####




#prep spring - find complete and incomplete
mig.SpringMigration <- mig %>% dplyr::filter(!is.na(SpringStartDate))
mig.SpringMigrationOnly <- mig.SpringMigration %>% dplyr::filter(!is.na(SpringEndDate)) 
mig.SpringMigrationPartial <- setdiff(mig.SpringMigration, mig.SpringMigrationOnly)

SpringNotes <- mig.SpringMigration[,c("id_yr", "SpringNotes")]

#process complete
mig.SpringMigrationOnly <- mig.SpringMigrationOnly %>% arrange(id_yr)
mig.SpringMigrationOnly <- mig.SpringMigrationOnly[!duplicated(c(mig.SpringMigrationOnly$id_yr)),]

#process incomplete
mig.SpringMigrationPartial <- mig.SpringMigrationPartial %>% arrange(id_yr)
mig.SpringMigrationPartial <- mig.SpringMigrationPartial[!duplicated(c(mig.SpringMigrationPartial$id_yr)),]

processS <- function(df, gps = data, partial = FALSE) {
  all_sels <- list()  # Initialize list to collect results
  
  for (i in unique(df$id_yr)) {
    t <- df %>% filter(id_yr == i)
    
    if (!partial) {
      sel <- gps %>%
        filter(AID == t$AID) %>%
        filter(POSIXct >= t$SpringStartDate & POSIXct <= t$SpringEndDate + 1)
      sel$id_yr <- t$id_yr
    } else {
      tryCatch({
        t$EventType <- str_extract(t$SpringNotes, "(Died|died|dead|death|End|end|ended|ends|fell|fall|falls|Fell|Falls|Fall|collar)")
        t$DateExtracted <- ifelse(is.na(t$EventType), NA, str_extract(t$SpringNotes, "\\d{4}-\\d{2}-\\d{2}"))
      }, error = function(e) {
        t$EventType <- NA
        t$DateExtracted <- NA
      })
      
      sel <- gps %>% filter(AID == t$AID)
      
      sel <- sel %>% filter(
        case_when(
          is.na(t$DateExtracted) ~ POSIXct >= t$SpringStartDate & POSIXct <= as.Date(paste0(lubridate::year(t$SpringStartDate), "-07-01")),
          !is.na(t$DateExtracted) ~ POSIXct >= t$SpringStartDate & POSIXct <= as.Date(t$DateExtracted),
          TRUE ~ TRUE
        )
      )
      sel$id_yr <- t$id_yr
    }
    
    all_sels[[i]] <- sel  # Store sel
  }
  
  bind_rows(all_sels)  # Return combined data
}

## Test
test <- processS(df = mig.SpringMigrationOnly %>% filter(id_yr %in% c("108_2018", "108_2019")), gps = all_rdh_data.sf, partial = F)
unique(test$id_yr)
test %>% group_by(id_yr) %>% reframe(range(POSIXct))

#set up parallel process for the spring complete


plan(sequential)
#plan()

start <- Sys.time()
mig.SpringMigrationOnly.sf <- mig.SpringMigrationOnly %>%
  mutate(chunk_id = ntile(row_number(), nc)) %>%
  group_split(chunk_id) %>% 
  future_map_dfr(~ processS(df = ., gps = data, partial = F), .progress = T)
stop <- Sys.time()

print(stop - start)




mig.SpringMigrationOnly.sf %>% group_by(id_yr) %>% reframe(range(POSIXct))


#now the spring incomplete

options(future.globals.maxSize = 15 * 1024^3)  # Set to 15 GiB
start <- Sys.time()
mig.SpringMigrationPartial.sf <- mig.SpringMigrationPartial %>%
  mutate(chunk_id = ntile(row_number(), nc)) %>%
  group_split(chunk_id) %>% 
  future_map_dfr(~ processS(df = ., gps = data, partial = T), .progress = TRUE)
stop <- Sys.time()

print(stop - start)

options(future.globals.maxSize = NULL)  # Unsets any custom value

plan(sequential)
plan()

str(mig.SpringMigrationPartial.sf)


### summarise the migrations and join ####

# keep NAs for those where data ended or animal died
Spring.summary1 <- mig.SpringMigrationOnly.sf %>% mutate(Date = date(Date)) %>% st_drop_geometry() %>% group_by(id_yr) %>% summarise(start = range(Date)[1], stop = range(Date)[2], length = as.numeric(stop-start)) %>% ungroup() 
Spring.summary2 <- mig.SpringMigrationPartial.sf %>% mutate(Date = date(Date))  %>% st_drop_geometry() %>% group_by(id_yr) %>% summarise(start = range(Date)[1], stop = NA, length = NA) %>% ungroup() 

Spring.summary <- bind_rows(Spring.summary1, Spring.summary2) %>% arrange(id_yr)
rm(Spring.summary1, Spring.summary2)

mig.SpringMigration.sf <- bind_rows(mig.SpringMigrationOnly.sf, mig.SpringMigrationPartial.sf) %>% arrange(AID, POSIXct)
rm(mig.SpringMigration,mig.SpringMigrationOnly.sf, mig.SpringMigrationPartial.sf,mig.SpringMigrationOnly, mig.SpringMigrationPartial)


#correct for partial 255_2018, captured while already started
Spring.summary[which(Spring.summary$id_yr == "255_2018"),"start"] <- NA
Spring.summary[which(Spring.summary$id_yr == "255_2018"),"length"] <- NA

#inspect
View(Spring.summary)





#-----------------------#
# Fall ####


#prep Fall
mig.FallMigration <- mig %>% dplyr::filter(!is.na(FallStartDate))
mig.FallMigrationOnly <- mig.FallMigration %>% dplyr::filter(!is.na(FallEndDate)) 
mig.FallMigrationPartial <- setdiff(mig.FallMigration, mig.FallMigrationOnly)

FallNotes <- mig.FallMigration[,c("id_yr", "FallNotes")]

#clean up the fall complete
mig.FallMigrationOnly <- mig.FallMigrationOnly %>% arrange(id_yr)
mig.FallMigrationOnly <- mig.FallMigrationOnly[!duplicated(c(mig.FallMigrationOnly$id_yr)),]

#clean up the fall incomplete
mig.FallMigrationPartial <- mig.FallMigrationPartial %>% arrange(id_yr)
mig.FallMigrationPartial <- mig.FallMigrationPartial[!duplicated(c(mig.FallMigrationPartial$id_yr)),]




processF <- function(df, gps = data, partial = FALSE) {
  all_sels <- list()  # Initialize list to collect results
  
  #df <- mig.FallMigrationPartial
  #gps = data; partial = TRUE
  #i = unique(df$id_yr)[1]
  
  for (i in unique(df$id_yr)) {
    
    t <- df %>% filter(id_yr == i)
    
    if (!partial) {
      sel <- gps %>%
        filter(AID == t$AID) %>%
        filter(POSIXct >= t$FallStartDate & POSIXct <= t$FallEndDate)
      sel$id_yr <- t$id_yr
    } else {
      tryCatch({
        t$EventType <- str_extract(t$FallNotes, "(Died|died|dead|death|End|end|ended|ends|fell|fall|falls|Fell|Falls|Fall|collar)")
        t$DateExtracted <- ifelse(is.na(t$EventType), NA, str_extract(t$FallNotes, "\\d{4}-\\d{2}-\\d{2}"))
      }, error = function(e) {
        t$EventType <- NA
        t$DateExtracted <- NA
      })
      
      sel <- gps %>% filter(AID == t$AID)
      
      start_date <- t$FallStartDate
      end_date <- ifelse(is.na(t$DateExtracted),
                         as.Date(paste0(lubridate::year(start_date) + 1, "-03-31")),
                         as.Date(t$DateExtracted))
      
      sel <- sel %>% filter(POSIXct >= start_date & POSIXct <= end_date)
      
      sel$id_yr <- t$id_yr
    }
    
    all_sels[[i]] <- sel  # Store sel
  }
  
  bind_rows(all_sels)  # Return combined data
}



options(future.globals.maxSize = 15 * 1024^3)  # Set to 15 GiB

start <- Sys.time()
mig.FallMigrationOnly.sf <- mig.FallMigrationOnly %>%
  mutate(chunk_id = ntile(row_number(), 1)) %>%
  group_split(chunk_id) %>%  
  future_map_dfr(~ processF(df = ., gps = data, partial = F), .progress = T)
stop <- Sys.time()

print(stop - start)

#run par process for incomplete Fall
start <- Sys.time()
mig.FallMigrationPartial.sf <- mig.FallMigrationPartial %>%
  mutate(chunk_id = ntile(row_number(), 1)) %>%
  group_split(chunk_id) %>%
  future_map_dfr(~ processF(df = ., gps = data, partial = TRUE))
stop <- Sys.time()

print(stop - start)





### summarise the fall migrations ####

# keep NAs for those where data ended or animal died
Fall.summary1 <- mig.FallMigrationOnly.sf %>% mutate(Date = date(Date)) %>% st_drop_geometry() %>% group_by(id_yr) %>% summarise(start = range(Date)[1], stop = range(Date)[2], length = as.numeric(stop-start)) %>% ungroup() 
Fall.summary2 <- mig.FallMigrationPartial.sf %>% mutate(Date = date(Date))  %>% st_drop_geometry() %>% group_by(id_yr) %>% summarise(start = range(Date)[1], stop = date(NA), length = NA) %>% ungroup() 

Fall.summary <- bind_rows(Fall.summary1, Fall.summary2) %>% arrange(id_yr)

mig.FallMigration.sf <- bind_rows(mig.FallMigrationOnly.sf, mig.FallMigrationPartial.sf) %>% arrange(AID, POSIXct)
rm(mig.FallMigration,mig.FallMigrationOnly.sf, mig.FallMigrationPartial.sf,mig.FallMigrationOnly, mig.FallMigrationPartial);rm(Fall.summary1, Fall.summary2)


View(Fall.summary)

#----------------------#
# Add in comments as notes ####
Spring.summary <- Spring.summary %>% left_join(., SpringNotes, by = "id_yr") %>% rename(Note = SpringNotes)
Fall.summary <- Fall.summary %>% left_join(., FallNotes, by = "id_yr") %>% rename(Note = FallNotes)


Spring.summary %>% mutate(Year = year(start)) %>% group_by(Year) %>% reframe(n = n())
Fall.summary %>% mutate(Year = year(start)) %>% group_by(Year) %>% reframe(n = n())


#-------------------------------#
# Finding missing ones from past times ####
grab.spring <- setdiff(oldSpring.summary$id_yr,Spring.summary$id_yr)
grab.fall <- setdiff(oldFall.summary$id_yr,Fall.summary$id_yr)

#append missing:
Spring.summary <- bind_rows(Spring.summary, oldSpring.summary %>% filter(id_yr %in% grab.spring))
Fall.summary <- bind_rows(Fall.summary, oldFall.summary %>% filter(id_yr %in% grab.fall))

#and for sf tracks
mig.SpringMigration.sf <- bind_rows(mig.SpringMigration.sf %>% st_transform(., crs = "EPSG:32612"), oldmig.SpringMigration.sf %>% filter(id_yr %in% grab.spring) %>% mutate(Date = as.character(Date), diff = as.numeric(diff)) %>% st_transform(., crs = "EPSG:32612"))
mig.FallMigration.sf <- bind_rows(mig.FallMigration.sf %>% st_transform(., crs = "EPSG:32612"), oldmig.FallMigration.sf %>% filter(id_yr %in% grab.fall)  %>% mutate(Date = as.character(Date), diff = as.numeric(diff)) %>% st_transform(., crs = "EPSG:32612"))






#------------------------------#
# Winter ranges ####
joinedW.sum <- Spring.summary %>% select(-c(length, Note)) %>% separate(id_yr, into = c("AID", "Year"), sep = "_", remove = F) %>% mutate(newidyr = paste0(AID, "_", as.numeric(Year) - 1))  %>% left_join(., Fall.summary %>% select(-c(length, Note)), by = c("newidyr" = "id_yr"), suffix = c("Spring", "Fall")) %>% select(-c(newidyr))

data <- data  %>% st_transform(., crs = "EPSG:32612")

processW <- function(df, gps = data){

  # df <- joinedW.sum[3,]
  # gps = data
  
  tryCatch({
  sel <- gps %>% filter(AID == df$AID) 
  sel <- sel %>% filter( case_when(
    is.na(df$stopFall) ~ POSIXct >= date(paste0(year(df$startSpring),"-02-01")) & POSIXct <= df$startSpring ,
    !is.na(df$stopFall) ~ POSIXct >= df$stopFall & POSIXct <= date(df$startSpring)))}, error = function(e) {sel <- gps[0,]
    print(paste("issue with ", df$id_yr))})  
  
  sel$id_yr <- df$id_yr

  return(sel)
 rm(sel)
  
}

#test
processW(df = joinedW.sum[44,], gps = data)

plan()
plan(sequential)
#plan(multisession, workers= detectCores() - 1)

options(future.globals.maxSize = 15 * 1024^3)  # Set to 15 GiB
start <- Sys.time()
WinterRanges.sf <- joinedW.sum %>% mutate(rownum = row_number()) %>% group_by(rownum) %>%  group_split() %>% 
  future_map_dfr(~ processW(df = ., gps = data), .progress = TRUE)
stop <- Sys.time()

print(stop - start)

options(future.globals.maxSize = NULL)  # Unsets any custom value

plan(sequential)
plan()

#------------------------------#
# Summer ranges ####
joinedS.sum <- Spring.summary %>% select(-c(length, Note))  %>% left_join(., Fall.summary %>% select(-c(length, Note)), by = "id_yr", suffix = c("Spring", "Fall")) %>% separate(id_yr, into = c("AID", "Year"), sep = "_", remove = F) %>% filter(!is.na(stopSpring))


processSum <- function(df, gps = data){
  
  
  tryCatch({sel <- gps %>% filter(AID == df$AID) %>% filter( case_when(
    is.na(df$startFall) ~ POSIXct >= df$stopSpring & POSIXct <= date(paste0(year(df$stopSpring),"-10-15")) ,
    !is.na(df$startFall) ~ POSIXct >= df$stopSpring & POSIXct <= date(df$startFall),
    TRUE ~ TRUE
  ))}, error = function(e) {sel <- gps[0,]
  print(paste("issue with ", df$id_yr))})  
  
  sel$id_yr <- df$id_yr
  
  return(sel)
  rm(sel)
  
}



plan()
plan(sequential)
#plan(multisession, workers= detectCores() - 1)

options(future.globals.maxSize = 15 * 1024^3)  # Set to 15 GiB
start <- Sys.time()
SummerRanges.sf <- joinedS.sum %>% mutate(rownum = row_number()) %>% group_by(rownum) %>%  group_split() %>% 
  future_map_dfr(~ processSum(df = ., gps = data), .progress = TRUE)
stop <- Sys.time()

print(stop - start)

options(future.globals.maxSize = NULL)  # Unsets any custom value

plan(sequential)
plan()


mapview(WinterRanges.sf)

#------------------------------#
# save files ####

save(mig.FallMigration.sf, mig.SpringMigration.sf, Spring.summary, Fall.summary, file = "C:/Users/lwilde2/Desktop/RDH Database/Processed Data/RDH_AllMigrations_2011t2024_06232025.RData")

save(WinterRanges.sf, SummerRanges.sf, file = "C:/Users/lwilde2/Desktop/RDH Database/Processed Data/RDH_AllSeasonalRanges_2011t2024_06232025.RData")


#save(WinterRanges.sf, file = "C:/Users/lwilde2/Downloads/RDH_WinterRanges_20250618.RData")


# #-------------------------------#
# # Inspect the output ####
# 
# rm(list = ls())
# load("C:/Users/lwilde2/Desktop/RDH Database/Processed Data/RDH_AllMigrations_2014t2023_20240821.RData")
# 
# View(Spring.summary)
# ggplot(mig.SpringMigration.sf %>% filter(Year >= 2023)) + geom_sf()


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






