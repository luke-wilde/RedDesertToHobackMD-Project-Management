# code to estimate days remaining on a collar
# written by Luke Wilde
# date = 02/03/2023




library(gdata)
library(tidyverse)
#call the function'

source("C:/Users/lwilde2/Documents/RDH Database/RedDesertToHobackMuleDeerResearchProject/RCode/other RCode/CollarCode/CalculateLifeEstimatesOnly_function.R")

#csv.name = the name of the file you are calling
#wd = is the working directory from which you are pulling csv.name
#ATTN: Default is Telonics settings and battery estimate calculations


# 1: Collar lifespan check -------------------------------


### enter the SNs to check --------------------
check = c("715877A", "719046A", "727575A", "727820A", "727823A", "727824A", "727915A", "728181A", "728183A", "728185A", "728198A", "728226A", "728227A", "736663A", "736665A", "736672A", "736675A", "736676A", "736678A", "736679A", "736681A", "736683A", "736687A", "744566A", "744567A", "744583A", "745335A", "745337A", "745338A", "745339A", "745340A", "745342A", "745345A", "745348A", "745349A", "745350A", "745351A", "745352A", "745353A", "745355A", "745360A", "745361A", "751712A", "751713A", "752056A", "752060A", "752061A", "752069A", "752071A", "752072A", "752073A", "752075A", "752077A", "752079A", "752080A", "752081A", "752084A", "752086A", "754390A", "754391A", "754392A", "754393A", "754395A", "754397A", "754398A", "754400A", "754402A", "754406A", "754408A", "754411A", "754412A", "754413A", "754419A")





out <- data.frame(collar = NA, n = NA, n.1 = NA, kb = NA)[0,]
wd = "C:/Users/lwilde2/Documents/RDH Database/RD2H_GPSCollarData/RD2H_Telonics_Data/IndividualRawGlobalstarDataFromTDC/"

for(i in 1:length(check)){
  #i=10
  tryCatch({
    
    
  tab <- read.csv(paste(wd, check[i], " Complete",".csv",sep=""), skip = 22)
  
 kb <- file.info(paste(wd, check[i], " Complete",".csv",sep=""))$size / 1024
  
  max = 1440
  conservative = (365*3+90)
  
  require(lubridate)
  require(tidyverse)
  require(stringr)
  num_Morts <- tab %>% filter(Mortality %in% c("Yes", "Yes*")) %>% mutate(GPS.Fix.Time = as.POSIXct(GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S"), day = date(GPS.Fix.Time)) %>% summarise(n = n_distinct(day)) * 1.25
  num_Active <- tab %>% filter(!Mortality %in% c("Yes", "Yes*")) %>% mutate(GPS.Fix.Time = as.POSIXct(GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S"), day = date(GPS.Fix.Time)) %>% summarise(n = n_distinct(day))
  num_Active_tries <- tab %>% filter(GPS.Fix.Attempt %in% c("Failed", "Unresolved QFP")) %>% mutate(GPS.Fix.Time = as.POSIXct(GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S"), day = date(GPS.Fix.Time)) %>% summarise(n = n_distinct(day))
  
  # df$collar[i] <- str_sub(csv.name,1,7)
  # df$conservative[i] <- (conservative - (num_Morts+num_Active_tries+num_Active))$n
  # df$max[i] <- (max - (num_Morts+num_Active_tries+num_Active))$n
  

  
  collar <- check[i]
  cons <- (conservative - (num_Morts+num_Active_tries+num_Active))
  max <- (max - (num_Morts+num_Active_tries+num_Active))
  out <- rbind(out, data.frame(collar, cons, max, kb))
  rm(collar, cons, max)
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

out$consdat <- out$n + Sys.Date()
out$avgdat <- out$n.1 + Sys.Date()

out |> arrange(consdat) |> View()



# t <- as.vector(paste(month(as.Date(out$consdat)),day(as.Date(out$consdat)), year(as.Date(out$consdat)),sep="/"))
# tt <- as.vector(paste(month(as.Date(out$avgdat)),day(as.Date(out$avgdat)), year(as.Date(out$avgdat)),sep="/"))

# 2: Check for recent Tx ----------------------------------------

### Requires CodeToProduceFawnRecruitmentOnxFiles is ran first
final.sf <- st_as_sf(final.gps, 
                     coords = c("Long", "Lat"), # note x goes first
                     crs = "+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs", # projection, this is NAD83
                     remove = FALSE) # don't remove lat/lon cols from dataframe

final.sf |> st_drop_geometry() |> filter(CllrSrN %in% check) |> group_by(CllrSrN) |> arrange(CllrSrN, POSIXct) |> summarise(max = max(POSIXct)) |> View()


