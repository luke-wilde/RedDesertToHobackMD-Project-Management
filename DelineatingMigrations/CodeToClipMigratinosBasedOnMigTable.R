#script to clip down to migrations or season of interest
library(sf)
library(dplyr)
library(mapview)
library(adehabitatHR)
library(adehabitatLT)
install.packages("bcp")
library(bcp)


load("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllData.RData")
read.csv()

finalshape <- st_as_sf(finalshape)

daily <- finalshape %>% group_by(AID, Date) %>% sample_n(6)

daily <- daily %>% filter(DayOfYear > 50)

#mapView(daily[daily$AID == "416",])
#range(daily[daily$AID == "416",]$"POSIXct")

daily21 <- as.data.frame(daily[daily$Year > 2020,])

str(daily21$POSIXct)

daily21$POSIXct <- as.POSIXct(as.character(daily21$POSIXct), format = "%Y-%m-%d %H:%M:%0S", tz = "UTC")

daily21lt <- as.ltraj(xy = daily21[, c("Long","Lat")], date = daily21$POSIXct, id = daily21$AID, typeII = F)

head(daily21lt[[47]])
lapply(attributes(daily21lt[[47]]), head)

b <- bcp(daily21lt[[47]]$R2n)
plot(b)


daily21lt[[99]][c(which(b$posterior.prob == 1)),"date"]



#now bring in the mig timing table
tab <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/AllSpringFallMigrationTiming_2014_thru_2021.csv")

head(tab)

tab$id_yr <- paste(tab$AID, tab$Year, sep="_")

short <- finalshape %>% filter(Mgtry == "Short")

short$id_yr <- paste(short$AID, short$Year, sep="_")

for(i in unique(tab$AID)){
  tmp_tab <- tab %>% filter(AID == i)
  
  for(j in unique(tmp_tab$Year)){
  tmp2_tab <- tmp_tab %>% filter(Year == j)
  
  
tmp <- short %>% group_by(AID) #%>% filter(POSIXct > )

  }
}


