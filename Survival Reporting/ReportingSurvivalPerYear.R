# annual survival for wgfd 2023

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
#library(patchwork)
library(dplyr)

#need to account for collar malfunctions at some point

#load in datasets
DeerID <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_24MARCH2023.csv") %>% filter(AID <= 649)# 
str(DeerID)
DeerID$StopDate <- as.Date(DeerID$StopDate, format = "%m/%d/%Y", origin = "1970-01-01", tz = "MST")
DeerID$StartDate <- as.Date(DeerID$StartDate, format = "%m/%d/%Y", origin = "1970-01-01", tz = "MST")



# first time
first <- DeerID %>% arrange(AID, StartDate) %>% group_by(AID) %>% slice(1)

# last time 
last <- DeerID %>% arrange(AID, desc(StartDate)) %>% group_by(AID) %>% slice(1)

#last <- last[which(!is.na(last$StopDate)),]
Dead <- last[which(!is.na(last$StopDate)),] %>% mutate(year = year(StopDate)) %>% filter(!AID %in% c(500,501,503,504,506,508,509,519,521, 523,527,529,532,533,540,545,448,449,450,456,459)) 


#head(first)

#how many in each year
deaths <- Dead %>% filter(year < 2023)  %>% group_by(year) %>% summarise(deaths = n()); deaths

#number of collars per year
DeerID2 <- DeerID
#DeerID2$StopDate <- ifelse(is.na(DeerID2$StopDate), as.Date("03/10/2023", format = "%m/%d/%Y", origin = "1970-01-01"), as.Date(DeerID2$StopDate, origin = "1970-01-01"))

range <- DeerID2 %>% filter(!AID %in% c(500,501,503,504,506,508,509,519,521, 523,527,529,532,533,540,545,448,449,450,456,459)) %>% group_by(AID) %>% summarise(min = min(StartDate), max = max(StopDate)) 


alive <- range[which(is.na(range$max)),]
died <- range[which(!is.na(range$max)),]

alive$max <- rep(Sys.Date(), nrow(alive))

sumA <- alive %>% mutate(minY = year(min), maxY = year(max)) %>% arrange(minY, maxY)
sumD <- died %>% mutate(minY = year(min), maxY = year(max)) %>% arrange(minY, maxY)
  
sum <- rbind(sumA,sumD)
  


year <- c(2014,2015,2016,2017,2018,2019,2020,2021,2022) #,2023

collarsYearly <- data.frame()
for(i in year){
  #i=2019
  xx <- data.frame(year = NA, collars = NA)
  xx$year <- i
  t <- sum %>% filter(minY <= i & maxY >= i)
  xx$collars <- nrow(t)
  collarsYearly <- rbind(collarsYearly,xx)
}
collarsYearly

#process
data <- data.frame(perc.surv = (1-(deaths$deaths / collarsYearly$collars) )*100, year = year)


#plot
winter <- c("14-15","15-16","16-17","17-18","18-19","19-20","20-21","21-22","22-23")
txt <- paste("2023 data current as of:")

perc.surv <- ggplot(data)  + geom_bar(aes(x = as.character(year), y = perc.surv), stat = "identity", fill = "blue") + geom_hline(yintercept = mean(data$perc.surv), linewidth = 1.4, linetype = "dashed", alpha = .75, color = "grey") + labs(x = "Year", y = "Percent of Sample", title = "Red Desert (Sublette) Adult Survival") + theme_classic()  + scale_y_continuous(limits = c(0,100), breaks = seq(0,100,25)) + coord_cartesian(ylim = c(0,100)) + theme(title = element_text(size = 10)) + geom_text(aes(x = as.character(year), y = perc.surv, label=paste(round(perc.surv,0),"%",sep="")), position=position_dodge(width=0.9), vjust=-2.5) ;perc.surv

ggsave(perc.surv, file = "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/WGFD_Mar2023_CaptureMeeting/RedDesert_AnnualSurv.png", dpi = 400, width = 9, height = 6)
