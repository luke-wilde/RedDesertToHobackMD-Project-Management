# Code to extract information about stopovers
# Author Luke Wilde
# version 4/2/24


#----------------------------#
# Load libraries ####


library(tidyverse)
library(sf)
library(lsa)
library(lubridate)
library(mapview)
library(forecast)
library(MoveTools)
library(ggthemes)
library(grid)
library(terra)
library(raster)
library(rptR)
library(ggridges)
library(arm)
library(corrplot)
library(patchwork)
library(randtests)
library(stats) 
library(merTools)
library(RColorBrewer)
library(DHARMa)
library(kohonen)
library(mstate)
library(mclust)
library(dbscan)
select <- dplyr::select

#--------------------------#
# load data ####


load("C:/Users/lwilde2/Desktop/full backup/PhD_Chapters/Datasets/RDH_DistanceAlongCorridor.RData")#"~/PhD_Chapters/Datasets/RDH_DistanceAlongCorridor.RData")
load("G:/Shared drives/wyo-coop-wilde-shared/RD2H_DataArchive/05182026/RDH_SpringStopovers_11t25_MergedAt5k_20260516.RData")#"C:/Users/lwilde2/Desktop/RDH Database/Processed Data/RDH_SpringStopovers_14t24_MergedAt5k_20250623.RData")
load("G:/Shared drives/wyo-coop-wilde-shared/RD2H_DataArchive/05182026/RDH_FallStopovers_11t25_MergedAt5k_20260516.RData")#"C:/Users/lwilde2/Desktop/RDH Database/Processed Data/RDH_FallStopovers_14t24_MergedAt5k_20250623.RData")
load("G:/Other computers/My Laptop/RDH Database/Processed Data/RDH_AllMigrations_2011t2025_06012026.RData")#"C:/Users/lwilde2/Desktop/RDH Database/Processed Data/RDH_AllMigrations_2011t2024_06232025.RData")
# writes a series of for loops to go through create a blank dataframe and iterate through each id to create a matrix, then can extract what proportion of each of the years points on its own migration stopovers are in the other ones too



#inserted it here 
#overwrite data
data <- mig.FallMigration.sf

SO_all_sep <- SO_F_all_sep_comb %>% group_by(AID, id_yr, grp1.c) %>% summarize(geometry = st_union(geometry)) %>% ungroup() %>% group_by(AID, id_yr) %>% mutate(stop.n = row_number(), year = as.numeric(str_sub(id_yr,5,8))) %>% ungroup() 

SO_all_sep <- SO_all_sep %>% st_cast(., "MULTIPOLYGON")

length(unique(SO_all_sep$AID)); length(unique(SO_all_sep$id_yr))
length(unique(data$AID)); length(unique(data$id_yr))


table(unique(data$id_yr) %in% unique(SO_all_sep$id_yr))
table(unique(SO_all_sep$id_yr) %in% unique(data$id_yr))

#should be 0's
check <- SO_all_sep[which(is.na(SO_all_sep$stop.n)),]
length(unique(check$AID)); length(unique(check$id_yr))

#get rid of animals missing SO info
missingSO <- data %>% dplyr::filter(!id_yr %in% SO_all_sep$id_yr) %>% arrange(id_yr) %>% st_drop_geometry() %>% dplyr::select(id_yr)
unique(missingSO$id_yr)





data1 <- data %>% filter(!id_yr %in% missingSO$id_yr) %>% arrange(id_yr)


# data1 <- data1[!duplicated(data1[, c("POSIXct", "AID")]), ]
# nrow(data1); length(unique(data1$AID)); length(unique(data1$id_yr))


SO_all_sep

# Step 1: Extract points on stops within same year, for loop

onstop <- data.frame()

ids <- unique(data1$id_yr)

for(i in 1:length(ids)){ #
  # i = 7
  #
  pt <- data1 %>% dplyr::filter(id_yr == ids[i])
  st <- SO_all_sep %>% dplyr::filter(id_yr == ids[i])

  #can keep this or not...
  st <- st %>% group_by(stop.n) %>% dplyr::summarise(id_yr = unique(id_yr), year = unique(year)) #%>% st_convex_hull()
  st_crs(pt) == st_crs(st)

  #extract points that are inside a stopover at the time
  pt1 <- pt %>% st_filter(., st, join = st_intersects)
  
  inter <- st_intersects(pt1, st, sparse = TRUE)
  
  
  pt1$stop.n  <- st$stop.n[vapply(inter, function(x) if (length(x)) x[1L] else NA_integer_, integer(1L))]
  
  
  onstop <- rbind(onstop, pt1)


} #i

head(onstop)

names(onstop)

# #rename the columns and shed useless
# onstop_yr <- onstop %>% rename("id_yr" = id_yr.x) %>% dplyr::select(-id_yr.y)


nrow(onstop); length(unique(onstop$AID)); length(unique(onstop$id_yr))

#remove duplicates, NOT SURE WHERE THESE COME FROM
dupcheck <- onstop %>% distinct(POSIXct, AID, .keep_all = T)
nrow(dupcheck) == nrow(onstop) # True means none

#missing stop.n info
nacheck <- onstop %>% drop_na(stop.n)
nrow(nacheck) == nrow(onstop) # True means none

#first prep the stopovers, need to isolate to the AID, then for previous years, calculate the IYD between a given stopover and the stopovers in another year

onstop_yr_fin <- onstop  %>% mutate(stop.n.c = paste(id_yr,"_",stop.n,sep="")) #
nrow(onstop_yr_fin); length(unique(onstop_yr_fin$AID)); length(unique(onstop_yr_fin$id_yr))


onstop_yr_fin$km_mark <- round(extract(RDH_DistanceAlongCorridor, as(onstop_yr_fin,"Spatial")),0)/1000

onstop_yr_fin <- onstop_yr_fin %>% group_by(id_yr) %>% mutate(km_mark_corr = round(km_mark - min(km_mark),0))

onstop_yr_fin <- onstop_yr_fin %>% group_by(stop.n.c) %>% mutate(minT = min(POSIXct), maxT = max(POSIXct), TimeStopped = (maxT - minT)/3600, km_mark = mean(km_mark, na.rm = T)) %>% ungroup()
nrow(onstop_yr_fin); length(unique(onstop_yr_fin$AID)); length(unique(onstop_yr_fin$id_yr))

summary(as.numeric(onstop_yr_fin$TimeStopped))
summary(as.numeric(onstop_yr_fin$km_mark))

onstop_yr_fin$TimeStopped <- as.numeric(onstop_yr_fin$TimeStopped)


#repeat the below for each one of these
onhigh_yr_fin <- onstop_yr_fin %>% filter(TimeStopped >= 72)
onstop_yr_fin <- onstop_yr_fin %>% filter(TimeStopped >= 24)

onhigh_yr_F_fin <- onhigh_yr_fin
onstop_yr_F_fin <-onstop_yr_fin
onlow_yr_F_fin <- onstop

save(onhigh_yr_F_fin, onstop_yr_F_fin, onlow_yr_F_fin, file = "G:/Other computers/My Laptop/RDH Database/Processed Data/RDH_FallStopoverBundle_11to25_20260601.RData")




# start comment
# setwd("C:/Users/lwilde2/Desktop/RDH Database/Processed Data")
# load("RDH_AllStopovers_14t22_MergedAt5k_20230505.RData")
# load("RDH_AllMigrations_Bischof_2014t2022_20230425.RData")
# load("C:/Users/lwilde2/Documents/PhD_Chapters/Datasets/ChapterThree_Profiles/StopoverBundle_04022024.RData")#"C:/Users/lwilde2/Documents/PhD_AdaptiveFidelity/Data_out/Data/Stopover/RDH_StopoverBundle_14t22_20230511.RData")
# setwd("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/")
# DeerID <- read.table("RD2H_AllDeerIDs_18MARCH2024.csv", header=TRUE, sep=",")
# #Format date and time columns
# DeerID$StartDate <- as.POSIXct(DeerID$StartDate, format="%m/%d/%Y", origin = "1970-01-01")
# DeerID$StopDate <- as.POSIXct(DeerID$StopDate,format="%m/%d/%Y", origin = "1970-01-01")
# setwd("C:/Users/lwilde2/Documents/PhD_AdaptiveFidelity")

# 
# 
# 
# #----------------------------#
# # App. Stopover Preparation ####
# 
# # commented from here:
# 
# 
# DeerID_checked <- DeerID  %>% distinct(AID, Mgtry, .keep_all = T) %>% arrange(AID) #%>% filter(Mgtry == "Long")
# data <- mig.SpringMigration.sf %>% filter(AID %in% DeerID_checked$AID ) #& Mgtry == "Long"
# 
# 
# #data <- st_as_sf(x = data, coords = c("UTM_E", "UTM_N"), crs = "+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# #get sa to the same
# #sa <- st_transform(sa, crs = st_crs(data)$input)
# #data <- data %>% st_transform(crs = "+proj=utm +zone=12 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
# #create test data
# #deer108 <- data %>% filter(AID == "108") %>% mutate(id_yr = paste(AID,"_",Year,sep=""))
# #data <- st_crop(data, sa)
# 
# #which animals with >1 year, keep
# keep <- data %>% st_drop_geometry() %>% group_by(AID) %>% dplyr::summarise(n = n_distinct(Year)) %>% ungroup() #%>% filter(n>1)
# 
# table(keep$n)
# 
# length(unique(DeerID_long$AID))
# length(unique(data$AID))
# length(unique(keep$AID))
# 
# 
# # data <- data %>% filter(AID %in% keep$AID)
# t <- onstop_yr_fin
# t$onstop <- 1
#
# data2 <- data1 %>% mutate(onstop = ifelse(POSIXct %in% onstop_yr_fin$POSIXct & id_yr %in% onstop_yr_fin$id_yr, 1, 2))
# data2 <- data2[!duplicated(data2[, c("POSIXct", "id_yr")]), ]
# table(data2$onstop)
#
# produces: load("C:/Users/lwilde2/Documents/PhD_AdaptiveFidelity/Data_out/Data/Stopover/RDH_StopoverBundle_14t22_20230511.RData")

# end comment


