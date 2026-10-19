library(sf); library(tidyverse); library(lubridate)


load("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RDH_GPSdata_HS+AC+LW+KM+MK_04.15.2026.RData")



rdh_25t26 <- all_rdh_data.sf %>% filter(POSIXct >= ydm("2025-01-01") & POSIXct <= ydm("2026-04-01") ) %>% mutate(idyr = str_replace(id_yr,"_", "."), across(.cols = c(AID, id_yr, Mgtry), as.character) ) %>% dplyr::select(AID, idyr, POSIXct, Mgtry, diff, Complete ) 

complete <- rdh_25t26 %>% filter(Complete == 1) %>% distinct(idyr)
  
rdh_25t26 <- rdh_25t26 %>% filter(idyr %in% complete$idyr)
  
sapply(rdh_25t26, function(x) sum(is.na(x)))
str(rdh_25t26)

st_write(rdh_25t26, "C:/Users/lwilde2/Downloads/RDH_Data_2025t2026_04282026.shp",append=F)
