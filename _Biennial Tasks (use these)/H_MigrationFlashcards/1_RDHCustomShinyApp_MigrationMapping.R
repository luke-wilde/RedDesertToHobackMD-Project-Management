
rm(list = ls()) # ONLY RUN ONCE, clears R memory

#------------------------#
#------------------------#
# BEGIN USER ENTRY ####
#------------------------#
#------------------------#

#point to where the files are stored
setwd("G:/My Drive/FileTransfer/Spring2025_PBennett_FissFus/MigrationMapping/") #-- CHANGE THIS AS NEEDED

#set output location
outloc <- "G:/My Drive/FileTransfer/Spring2025_PBennett_FissFus/MigrationMapping/Outs_2026/LRW/" #-- CHANGE THIS AS NEEDED

data.file <- "ShinyAppImage_RDHdata_redoBigSummers_v1.RData"

start = 1
end = 25

## NOTEs ####

#1# Once you close the runApp window, be sure to view the results_df and then save it. Adjust the start and end as needed if you had to cut a session short (or start over, its only 10-30 animals!)

#2# only do 50 or so animal-years at a time

## END NOTES ####

#------------------------#
#------------------------#
# END USER ENTRY ####
#------------------------#
#------------------------#


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# BELOW HERE JUST RUN LINE BY LINE WITHOUT CHANGES
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#


# List of packages
packages <- c(
  "collar", "sf", "adehabitatLT", "adehabitatHR", "tidyverse", "plotly", "leaflet", "sp", "doremi", "htmlwidgets", "htmltools", "MerkleLabGIS", "terra", "patchwork", "ggspatial", "gridExtra", "webshot", "shiny"
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


#load("ShinyAppImage_RDHdata_2011t2024.RData")
load(data.file)

source("1.1_ShinyAppFunctions.R") #also in H_MigrationFlashcards

choice <- c("108_2014", "108_2015", "108_2016", "108_2017", "108_2018", "108_2019", "108_2020", "108_2021", "108_2022", "108_2023", "108_2024", "129_2016", "129_2017", "129_2018", "225_2019", "225_2020", "225_2021", "233_2016", "233_2017", "235_2016", "235_2017", "242_2022", "242_2023", "242_2024", "242_2025", "255_2018", "255_2019", "255_2020", "255_2021", "255_2022", "255_2023", "255_2024", "257_2016", "257_2017", "257_2018", "257_2019", "260_2016", "260_2017", "260_2018", "263_2016", "263_2017", "284_2022", "284_2023", "286_2017", "286_2018", "287_2017", "287_2018", "287_2019", "287_2020", "292_2017", "292_2018", "297_2017", "297_2018", "299_2017", "299_2018", "300_2017", "300_2018", "300_2019", "300_2020", "300_2021", "303_2019", "303_2020", "304_2018", "304_2019", "308_2018", "308_2019", "308_2020", "309_2017", "309_2018", "309_2019", "314_2022", "314_2023", "314_2024", "314_2025", "317_2017", "317_2018", "320_2017", "320_2018", "327_2017", "327_2018", "327_2019", "327_2020", "327_2021", "327_2022", "330_2022", "330_2023", "331_2017", "331_2018", "331_2019", "331_2020", "331_2021", "331_2022", "331_2023", "332_2017", "332_2018", "336_2017", "336_2018", "336_2019", "344_2018", "344_2019", "359_2018", "359_2019", "359_2020", "359_2021", "369_2018", "369_2019", "444_2021", "444_2022", "447_2020", "447_2021", "447_2022", "447_2024", "447_2025", "451_2021", "451_2022", "451_2023", "454_2020", "454_2021", "454_2022", "454_2023", "463_2021", "463_2022", "463_2023", "466_2020", "466_2021", "515_2022", "515_2023", "516_2021", "516_2022", "516_2023", "541_2021", "541_2022", "541_2023", "543_2021", "543_2022", "543_2023", "547_2021", "547_2022", "665_2022", "665_2023", "665_2024", "665_2025", "680_2022", "680_2023", "680_2024", "680_2025", "681_2022", "681_2023", "681_2024", "681_2025", "702_2024", "702_2025", "711_2023", "711_2024", "711_2025", "712_2023", "712_2024", "718_2023", "718_2024", "718_2025", "736_2023", "736_2024", "736_2025", "744_2023", "744_2024", "759_2023", "759_2024") #unique(RDH_nsd$burst)

choice <- choice[start:end] 

#now just run this empty function
runApp()

#review output
results_df 

#save output
#save(results_df, file = paste0(outloc, "MigrationsMapped_",99998, " ", 99999,".RData")); rm(results_df)
save(results_df, file = paste0(outloc, "MigrationsMapped_",start, " ", end,".RData")); rm(results_df)


#-----------------------#
# Outdated section ####

# redo_LRW_05132025 <- c("260_2017", "280_2020", "284_2019", "311_2017", "370_2018", "410_2021", "413_2019" ,"436_2020" ,"446_2021" ,"446_2022" ,"456_2020", "463_2021", "464_2021","503_2021", "509_2021" ,"514_2021", "521_2023" ,"525_2024", "537_2021" ,"543_2021" ,"547_2022", "601_2024", "608_2023", "609_2022", "657_2023", "682_2024")

# load("C:/Users/lwilde2/Downloads/SpringCheck_NAsReal_MigMapp.RData")


# checkSpring <- checkSpring[!grepl("2025", checkSpring)]

#this is bc checkSpring came from one that started in June, now its switched to August
# checkSpring <- checkSpring[checkSpring %in% RDH_nsd$burst]



#-----------------------#
# Preparing data for this year's effort ####


# #ATTN == can leave commented out if using save image - LRW, 03.14.2025
# #NOT RUN
# 
# 
# 
# #load full dataset
# load("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_CleanData.RData")
# 
# #load elevation
# elev <- rast("/vsicurl/https://merklelab.s3.arcc.uwyo.edu/DEM/etopocompressed.tif") #
# 
# #july first is cutoff
# rdh_data.sf <- rdh_data.sf %>% mutate(Date = date(POSIXct)) %>% group_by(AID, Date) %>% slice_tail(n=1) %>% ungroup() %>% st_transform(., crs = "+proj=longlat +datum=WGS84") %>% mutate(bioyr = paste0(AID,"_",ifelse(Month >= 8 & Day >= 15, Year + 1, Year))) #%>% filter(Year %in% c(2025, 2026))
# 
# # Prepare data for ltraj
# RDH_m_sp <- as(rdh_data.sf, "Spatial")
# 
# xy <- coordinates(RDH_m_sp)
# date <- as.POSIXct(RDH_m_sp$POSIXct)
# id <- as.factor(RDH_m_sp$AID)
# bioyr <- as.factor(RDH_m_sp$bioyr)
# 
# # Make a trajectory stack
# RDH_lt <- as.ltraj(xy = xy, date = date, id = id, burst = bioyr, typeII = TRUE,
#                    proj4string = CRS(st_crs(rdh_data.sf)$proj4string))
# 
# # Convert to dataframe
# RDH_nsd <- ld(RDH_lt)
# 
# RDH_nsd <- RDH_nsd %>%
#   st_as_sf(coords = c("x","y"), crs = st_crs(st_crs(rdh_data.sf)$proj4string)) %>%
#   mutate(month = month(date))
# 
# RDH_nsd$elev <- extract(elev, st_transform(RDH_nsd, crs = st_crs(elev)))[,2]
# 
# sumtab <- rdh_data.sf %>% st_drop_geometry() %>% group_by(AID) %>% summarise(gpsStart = first(Date), gpsEnd = last(Date))
# 
# 
# 
# save.image("G:/My Drive/FileTransfer/Spring2025_PBennett_FissFus/MigrationMapping/ShinyAppImage_RDHdata_redoBigSummers_v1.RData")







