# For Monday Meeting on March Captures

gc()
rm(list=ls())

#RDH only
#exclude Vehicle and Capture related mortalities
Wint <- c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)
CapM <- c(1,0,3,4,0,3,4,3,5,2)
VehM <- c(0,0,0,0,0,2,1,1,0,0)

exclude <- data.frame(Wint = Wint, CapM = CapM, VehM = VehM)
exclude


#-------------------------------#
#set up env ####

library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(patchwork)
library(dplyr)


# NEED INFO on:
# Number of mortalities this winter compared to past 5 winters
# Snowdepth and temperature this winter and last two weeks compared to past 5
# Capture mortalities in more severe winters
# 

# Method
# Use a subset of where the deer are to extract SNODAS and DAYMET info

#-----------------------------------#
# mortalities in past winters ####
DeerID <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_24MARCH2023.csv") %>% filter(AID <= 699)
str(DeerID)
DeerID$StopDate <- as.POSIXct(DeerID$StopDate, format = "%m/%d/%Y", tz = "MST")
DeerID$StartDate <- as.POSIXct(DeerID$StartDate, format = "%m/%d/%Y", tz = "MST")


# first time
first <- DeerID %>% arrange(AID, StartDate) %>% group_by(AID) %>% slice(1)

# last time 
last <- DeerID %>% arrange(AID, desc(StartDate)) %>% group_by(AID) %>% slice(1)

#last <- last[which(!is.na(last$StopDate)),]
Dead <- last[which(!is.na(last$StopDate)),] %>% mutate(year = year(StopDate)) 

winter_morts <- Dead %>% mutate(Month = month(StopDate), Year = year(StopDate), J = yday(StopDate)) %>% filter(Month %in% c(12,1,2,3))


winter_morts <- winter_morts %>% arrange(Year, Month)

winter_morts$Year <- ifelse(winter_morts$J > 343, winter_morts$Year + 1, winter_morts$Year)

t <- winter_morts %>% filter(J >= 90 & J <= 343) #70 is March 9 or 10, depending on leap year 

winter_mort_data <- winter_morts %>% filter(!AID %in% t$AID) %>% group_by(Year) %>% summarise(n = n()) 
winter_mort_data


#how many in each year
deaths <- Dead %>% filter(year < 2023)  %>% group_by(year) %>% summarise(deaths = n()); deaths

#number of collars per year
DeerID2 <- DeerID
#DeerID2$StopDate <- ifelse(is.na(DeerID2$StopDate), as.Date("03/10/2023", format = "%m/%d/%Y", origin = "1970-01-01"), as.Date(DeerID2$StopDate, origin = "1970-01-01"))

range <- DeerID2 %>% filter(!AID > 699) %>% group_by(AID) %>% summarise(min = min(StartDate), max = max(StopDate)) 


alive <- range[which(is.na(range$max)),]
died <- range[which(!is.na(range$max)),]

alive$max <- rep(Sys.Date(), nrow(alive))

sumA <- alive %>% mutate(minY = year(min), maxY = year(max)) %>% arrange(minY, maxY)
sumD <- died %>% mutate(minY = year(min), maxY = year(max)) %>% arrange(minY, maxY)

sum <- rbind(sumA,sumD)

sum %>% print(n = 500)

#add
year <- c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)

collarsYearly <- data.frame()
for(i in year){
  #i=2019
  xx <- data.frame(year = NA, collars = NA)
xx$year <- i
t <- sum %>% filter(minY <= i & maxY >= i)
xx$collars <- nrow(t)
collarsYearly <- rbind(collarsYearly,xx)
}

#2014 fix
collarsYearly$collars[1] <- 14
winter_mort_data$n[1] <- .1 #fix that collars dropped, not dead


#morts in the winter
winter <- c("14-15","15-16","16-17","17-18","18-19","19-20","20-21","21-22","22-23")
txt <- paste("2023 data current as of:")
morts <- ggplot(winter_mort_data)  + geom_bar(aes(x = as.character(winter), y = n), stat = "identity", fill = "blue") + geom_hline(yintercept = mean(winter_mort_data$n), linewidth = 1.4, linetype = "dashed") + labs(x = "Winter (Dec 1 - Mar 10)", y = "Number of Deer", title = "Red Desert Mule Deer, Total Mortalities") + theme_classic() + coord_cartesian(ylim = c(-.25,20.25))+ scale_y_continuous(breaks = seq(0,20,2)) + theme(title = element_text(size = 10)) + geom_text(x=2.5, y=20, label=txt) + geom_text(x=5.2, y=20, label=Sys.Date());morts

winter_mort_data$perc <- winter_mort_data$n / collarsYearly$collars[2:10]
winter_mort_data$surv <- (1 - winter_mort_data$perc) * 100

perc.morts <- ggplot(winter_mort_data)  + geom_bar(aes(x = as.character(winter), y = perc), stat = "identity", fill = "blue") + geom_hline(yintercept = mean(winter_mort_data$perc ), linewidth = 1.4, linetype = "dashed") + labs(x = "Winter (Dec 1 - Mar 10)", y = "Percent of Sample", title = "Red Desert Mule Deer, % Sample Lost") + theme_classic() + coord_cartesian(ylim = c(-.001,.2))+ scale_y_continuous(breaks = seq(0,.2,.05)) + theme(title = element_text(size = 10)) + geom_text(x=2.2, y=.18, label=txt) + geom_text(x=4.7, y=.18, label=Sys.Date());perc.morts

perc.surv <- ggplot(winter_mort_data)  + geom_bar(aes(x = as.character(winter), y = surv), stat = "identity", fill = "blue") + geom_hline(yintercept = mean(winter_mort_data$surv ), linewidth = 1.4, linetype = "dashed") + labs(x = "Winter Survival since Dec 1", y = "Percent of Sample", title = "Red Desert Mule Deer, % Adult Survival") + theme_classic() + coord_cartesian(ylim = c(69,100))+ scale_y_continuous(breaks = seq(70,100,5)) + theme(title = element_text(size = 10)) + geom_text(x=2.2, y=.88, label=txt) + geom_text(x=2.2, y=.86, label=Sys.Date());perc.surv

# #snowdepth - winter range
# RedDes_Snodep <- ggplot(desert_snow %>% filter(Year != 2015)) + geom_jitter(aes(x = as.character(Year), y = Snodas_SnowDepth), color = "grey", alpha = .2) + geom_pointrange(data = desert_snow %>% filter(Year != 2015) %>% group_by(Year) %>% summarise(meandep = mean(Snodas_SnowDepth), sd = sd(Snodas_SnowDepth)),aes(x = as.character(Year), y = meandep, ymin = meandep - sd, ymax = meandep + sd), color = "black", size = 1.1) + theme_classic() + labs(x = "Year", y = "Snowdepth (m)", title = "Rock Springs, WY & Superior, WY")
# 
# #snowdepth - prospects and N of Hwy 28
# Prosp_Snodep <- ggplot(prosp_snow %>% filter(Year != 2015)) + geom_jitter(aes(x = as.character(Year), y = Snodas_SnowDepth), color = "grey", alpha = .2) + geom_pointrange(data = prosp_snow %>% filter(Year != 2015) %>% group_by(Year) %>% summarise(meandep = mean(Snodas_SnowDepth), sd = sd(Snodas_SnowDepth)),aes(x = as.character(Year), y = meandep, ymin = meandep - sd, ymax = meandep + sd), color = "black", size = 1.1) + theme_classic() + labs(x = "Year", y = "Snowdepth (m)", title = "North of Hwy 28")
# 
# #tmax - red des
# RedDes_Tmin <- ggplot(tmean_test) + geom_jitter(data = weather %>% filter(Month %in% c(12,1,2)), aes(x = as.character(Year), y = TMIN), color = "grey", alpha = .4) + geom_pointrange(aes(x = as.character(Year), y = mean, ymin = mean-sd, ymax = mean+sd), color = "black", size = 1.1) + theme_classic() + labs(x = "Year", y = "Daily Min Temp (c)", title = "Rock Springs, WY & Superior, WY"); RedDes_Tmin


#------------------------------#
# Export plots ####
# ggsave(RedDes_Snodep + Prosp_Snodep, file = "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/WGFD_Mar2023_CaptureMeeting/RedDesert_snowdepth.png")
# ggsave(RedDes_Tmin, file = "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/WGFD_Mar2023_CaptureMeeting/RedDesert_tmin.png")
ggsave(morts, file = "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/WGFD_Mar2023_CaptureMeeting/RedDesert_WinterMorts_v3_new.png", width = 6, height = 4)
ggsave(perc.morts, file = "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/WGFD_Mar2023_CaptureMeeting/RedDesert_WinterMortsPercentOfSample_v3_new.png", width = 6, height = 4)
