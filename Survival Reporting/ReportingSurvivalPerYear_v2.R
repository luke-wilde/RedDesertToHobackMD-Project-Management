# Code to export graph of Annual Survival for all completed years
# Author: Luke Wilde
# Created: 3/15/2023
# last edit: 4/13/2023


#-------------------------------#
#set up env ####

#clean environment
gc()
rm(list=ls())


library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(patchwork)
library(dplyr)

#--------------------------------------#
# Survival metadata ####
meta <- "C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_16MAY2023.csv" # this is where your Ortega style metadata file is, make sure its the most current


#--------------------------------------#
# Additional information ####

#exclude Vehicle and Capture related mortalities as these are not natural mortalities
Year <- c(2014,2015,2016,2017,2018,2019,2020,2021,2022)
CapM <- c(1,0,3,4,0,3,4,3,5)
VehM <- c(0,0,0,0,0,2,1,1,0)


RDHexclude <- data.frame(Year=Year, CapM = CapM, VehM = VehM)
RDHexclude

fawns <- c(c(448,449,450,456,459,500,501,503,504,506,508,509,519,521, 523,527,529,532,533,540,545),seq(650,699,1),seq(750,799,1))

#-----------------------------------#
# Load and prep metadata ####
DeerID <- read.csv(meta) 
str(DeerID) #check structure

#reformat Dates
DeerID$StopDate <- as.POSIXct(DeerID$StopDate, format = "%m/%d/%Y", tz = "MST")
DeerID$StartDate <- as.POSIXct(DeerID$StartDate, format = "%m/%d/%Y", tz = "MST")

#remove fawns
DeerID <- DeerID %>% filter(!AID %in% fawns)

#------------------------------#
# Extract relevant info ####

# identify the first time a deer was monitored
first <- DeerID %>% arrange(AID, StartDate) %>% group_by(AID) %>% slice(1) %>% mutate(FirstYear = year(StartDate))

# identify the last time a deer was monitored
last <- DeerID %>% arrange(AID, desc(StartDate)) %>% group_by(AID) %>% slice(1)

# find which deer are dead, all those without an NA for stopdate
Dead <- last[which(!is.na(last$StopDate)),] %>% mutate(Year = year(StopDate)) 

# # now, which of those dead were after Dec 1 but before todays date
# winter_morts <- Dead %>% mutate(Month = month(StopDate), Year = year(StopDate), J = yday(StopDate)) %>% filter(J > yday("2001-12-01") | J < yday(Sys.Date())) %>% arrange(Year, Month)
# 
# #make december morts be the winter of that year and the next
# winter_morts$Year <- ifelse(winter_morts$J > yday("2001-12-01"), winter_morts$Year + 1, winter_morts$Year)
# 
# #join with first, and filter out all who were collared March of that winter
# winter_morts <- winter_morts %>% left_join(first %>% select(AID, FirstYear), by = "AID") %>% filter(Year > FirstYear)
# str(winter_morts)
# 
# 
# winter_mort_data <- winter_morts %>% group_by(Year) %>% summarise(n = n())  %>% mutate(wint = Wint)
# 
# 
# winter_mort_data$n <- ifelse(winter_mort_data$n - (RDHexclude$CapM) < 0, 0, winter_mort_data$n - (RDHexclude$CapM))

# #finally, add up all dead in that year
deaths <- Dead %>% group_by(Year) %>% summarise(deaths = n()); deaths

#remove 2023 for now
deaths <- deaths[-10,]

deaths$deaths <- deaths$deaths - RDHexclude$CapM

#----------------------------------#
# Now define each year's sample ####
#how many collars in each year

range <- DeerID %>% group_by(AID) %>% summarise(min = min(StartDate), max = max(StopDate)) 


alive <- range[which(is.na(range$max)),]
died <- range[which(!is.na(range$max)),]

alive$max <- rep(Sys.Date(), nrow(alive))

sumA <- alive %>% mutate(minY = year(min), maxY = year(max)) %>% arrange(minY, maxY)
sumD <- died %>% mutate(minY = year(min), maxY = year(max)) %>% arrange(minY, maxY)

sum <- rbind(sumA,sumD)

sum %>% print(n = nrow(.))

#add
Year <- c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)

collarsYearly <- data.frame()
for(i in Year){
  #i=2019
  xx <- data.frame(Year = NA, collars = NA)
  xx$Year <- i
  t <- sum %>% filter(minY <= i & maxY >= i)
  xx$collars <- nrow(t)
  collarsYearly <- rbind(collarsYearly,xx)
}

collarsYearly <- collarsYearly[-10,]

#2014 fix
#collarsYearly$collars[1] <- 14

#
data <- collarsYearly %>% left_join(deaths, by = "Year")
data$perc <- data$deaths / data$collars
data$surv <- (1- data$perc)
data



#plot

#txt <- paste("2023 data current as of:")

perc.surv <- ggplot(data[-10,])  + geom_bar(aes(x = as.character(Year), y = surv*100), stat = "identity", fill = "grey", color = "black") + geom_hline(yintercept = mean(data$surv*100), linewidth = 1.4, linetype = "dashed", alpha = .75, color = "black") + labs(x = "Year", y = "Percent of Sample", title = "Red Desert (Sublette) Adult Female Survival") + theme_bw()  + scale_y_continuous(limits = c(0,100), breaks = seq(0,100,25)) + coord_cartesian(ylim = c(0,100)) + theme(title = element_text(size = 10)) + geom_text(aes(x = as.character(Year), y = surv*100, label=paste(round(100*surv,0),"%",sep="")), position=position_dodge(width=0.9), vjust=1.15) ;perc.surv 

ggsave(perc.surv, file = "C:/Users/lwilde2/Desktop/RedDesert_AnnualSurv_04142023.png", dpi = 400, width = 9, height = 6)
