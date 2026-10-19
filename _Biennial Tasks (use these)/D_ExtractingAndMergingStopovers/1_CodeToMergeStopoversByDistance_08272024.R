# Code to process all stopovers so as to group those within 5 km (sensu Sawyer and Ortega) 
# Author: Luke Wilde
# Written: 04 02 2023
# Updated: 08 27 2024


library(sf)
library(tidyverse)
library(mapview)

#------------------------------------------------------#
#### Stopover Status ####
# setwd("C:/Users/lwilde2/Desktop/RDH Database/Processed Data/")
# load("RDH_AllMigrations_2011t2024_06232025.RData")
# 

load("G:/Shared drives/wyo-coop-wilde-shared/RD2H_DataArchive/05182026/RDH_FallStopovers_2011to2025_05182026.RData")
SO_F_all <- SO_all

mapview(SO_F_all, zcol = "id_yr")

#remove those that fill the entire study area as necessary
#SO_S_all <- SO_S_all %>% filter(!id_yr %in% c("515_2022","463_2021","473_2021", "467_2021", "468_2021"))

# writes a series of for loops to go through create a blank dataframe and iterate through each id to create a matrix, then can extract what proportion of each of the years points on its own migration stopovers are in the other ones too

#make sure the point layer and the SO layer within same crs
st_crs(SO_F_all) == st_crs("EPSG:32612")

#this needs to be a two step process since some migrations have only 1 stop
SO_F_all_sep <- SO_F_all %>% filter(st_geometry_type(.) == "MULTIPOLYGON") %>% mutate(AID = str_sub(id_yr, 0,3)) %>% st_cast(., "POLYGON")
#for single stops bind:
SO_F_all_sep <- rbind(SO_F_all_sep, SO_F_all %>% filter(st_geometry_type(.) == "POLYGON"))



#combine stops that are within 5 km of each other, takes two steps to get rid of overlap and dual counts
# ATTN:: This code combines stops that are within 5000 n if 
SO_F_all_sep_comb <- SO_F_all_sep[0,0]

for(i in unique(SO_F_all_sep$id_yr)){
  #i = unique(SO_F_all_sep$id_yr)[3]
  x <- SO_F_all_sep %>% filter(id_yr == i) 
  
  x1 <- x %>% st_buffer(., dist = 2500) 
  x2 <- st_union(x1) %>% st_cast(., "POLYGON") %>% st_as_sf()
  
  x2$grp1.c <- 1:nrow(x2)
  
  x <- x %>% st_join(., x2, join = st_intersects)
  rownames(x) <- NULL
  
  SO_F_all_sep_comb <- rbind(SO_F_all_sep_comb, x)
  rm(x, x1, x2)
}#i

x <- SO_F_all_sep_comb %>% filter(id_yr == "108_2021")
mapview(x, zcol = "grp1.c")


save(SO_F_all_sep_comb, file = "G:/Shared drives/wyo-coop-wilde-shared/RD2H_DataArchive/05182026/RDH_FallStopovers_11t25_MergedAt5k_20260516.RData")
