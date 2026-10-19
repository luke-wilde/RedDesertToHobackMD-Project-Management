# Code to estimate life expectancy of collars
# 10/31/2025

#---------------------#
# 1. Download reports ####

# 1. Sync and report the collars. Be sure that the Options>Report Settings indicates "Complete only"
# 2. Also, set the Report Data at the bottom is set to "All data"

#---------------------#
# Load libraries in env ####

# install.packages("gdata")
# install.packages("tidyverse")
library(gdata)
library(tidyverse)


#---------------------#
# Settings ####

## Set up paths ####
path_to_reports <- "C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_Telonics_Data/IndividualRawGlobalstarDataFromTDC/" #<Change this>#

path_to_out <- "C:/Users/lwilde2/Downloads/" #<Change this>#

## Next capture date ####

next_cap <- date("2026-03-10")

#---------------------#
# Build custom function ####

CalculateLife_estimates <- function(csv.name = "715877A Complete", wd = "C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_Telonics_Data/IndividualRawGlobalstarDataFromTDC/") {

  tab <- read.csv(paste(wd, csv.name,".csv",sep=""), skip = 22)
  
  max = 1440
  conservative = (365*3+90)
  
  require(lubridate)
  require(tidyverse)
  require(stringr)
  
  num_Morts <- tab %>% filter(Mortality %in% c("Yes", "Yes*")) %>% mutate(GPS.Fix.Time = as.POSIXct(GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S"), day = date(GPS.Fix.Time)) %>% summarise(n = n_distinct(day)) * 1.25 %>% as.vector()
  num_Active <- tab %>% filter(!Mortality %in% c("Yes", "Yes*")) %>% mutate(GPS.Fix.Time = as.POSIXct(GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S"), day = date(GPS.Fix.Time)) %>% summarise(n = n_distinct(day)) %>% as.vector()
  num_Active_tries <- tab %>% filter(GPS.Fix.Attempt %in% c("Failed", "Unresolved QFP")) %>% mutate(GPS.Fix.Time = as.POSIXct(GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S"), day = date(GPS.Fix.Time)) %>% summarise(n = n_distinct(day)) * 1.1 %>% as.vector()
  
  
                    
  
  x0 <- as.character(tab$Acquisition.Start.Time)
  x0 <- gsub("\\.", "-", x0)
  x0 <- x0[x0 != ""]
  first <- date(x0)[1]
  
  
  cons_rem <- (conservative - (num_Active + num_Active_tries + num_Morts))$n
  avg_rem <- (max - (num_Active + num_Active_tries + num_Morts) )$n
  cons_end <- cons_rem + next_cap
  avg_end <- avg_rem + next_cap
  
  data.frame("Collar" = sub(" .*", "", csv.name), "Projection.For.Capture" = next_cap,"First.Deployment" = first, "Days.Remaining.Conservative" = cons_rem, "Days.Remaining.Typical" = avg_rem, "Day.To.Fail.Conservative" = cons_end, "Day.To.Fail.Typical" = avg_end)
  
}


#---------------------#
# List collars to estimate ####

collars <- str_remove(list.files(path = path_to_reports, pattern = ".csv"), ".csv")


#---------------------#
# output estimates ####

result <- data.frame("Collar" = character(),"Projection.For.Capture" = character(),"First.Deployment" = character(), "Days.Remaining.Conservative" = numeric(), "Days.Remaining.Typical" = numeric(), "Day.To.Fail.Conservative" = character(), "Day.To.Fail.Typical" = character())[,]

for(i in 1:length(collars)){
out <- CalculateLife_estimates(csv.name = collars[i], wd = path_to_reports)

result <- rbind(result, out)
}

result


#---------------------#
# save to file ####

write.csv(result, paste0(path_to_out,"CollarLifeEstimates_",gsub("-","",Sys.Date()),".csv"))

#---------------------#
# ####
#---------------------#
# ####
