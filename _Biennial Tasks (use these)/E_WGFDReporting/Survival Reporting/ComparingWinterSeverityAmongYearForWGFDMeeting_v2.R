# Code to export graph of Dec 1 to Current Day Winter Mortality
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
meta <- "C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_10DECEMBER2023.csv" # this is where your Ortega style metadata file is, make sure its the most current


#--------------------------------------#
# Additional information ####

#exclude Vehicle and Capture related mortalities as these are not natural mortalities
Wint <- c("14-15","15-16","16-17","17-18","18-19","19-20","20-21","21-22","22-23")
CapM <- c(0,3,4,2,0,5,1,3,4)
#c(0,3,4,2,0,5,1,3,4) #without fawns
#c(0,3,4,2,0,5,2,4,5) #with fawns

VehM <- c(0,0,0,0,0,2,1,1,0)

RDHexclude <- data.frame(Wint = Wint, CapM = CapM, VehM = VehM)
RDHexclude

fawns <- c(c(448,449,450,456,459,500,501,503,504,506,508,509,519,521, 523,527,529,532,533,540,545),seq(650,699,1),seq(750,799,1))

#-----------------------------------#
# Load and prep metadata ####
DeerID <- read.csv(meta) 
str(DeerID) #check structure

#reformat Dates
DeerID$StopDate <- as.POSIXct(DeerID$StopDate, format = "%m/%d/%Y", tz = "MST")
DeerID$StartDate <- as.POSIXct(DeerID$StartDate, format = "%m/%d/%Y", tz = "MST")

#fawns with spring mig
x <- DeerID %>% filter(AID %in% fawns) %>% filter(is.na(StopDate) | StopDate - StartDate > 120) %>% distinct(AID)
x

#remove fawns
DeerID <- DeerID %>% filter(!AID %in% fawns)
length(unique(DeerID$AID))
#------------------------------#
# Extract relevant info ####

# identify the first time a deer was monitored
first <- DeerID %>% arrange(AID, StartDate) %>% group_by(AID) %>% slice(1) %>% mutate(FirstYear = year(StartDate))

# identify the last time a deer was monitored
last <- DeerID %>% arrange(AID, desc(StartDate)) %>% group_by(AID) %>% slice(1)

last %>% group_by(Mgtry) %>% summarise(n = n())
first %>% group_by(Mgtry) %>% summarise(n = n())
# find which deer are dead, all those without an NA for stopdate
Dead <- last[which(!is.na(last$StopDate)),] %>% mutate(Year = year(StopDate)) 

# now, which of those dead were after Dec 1 but before todays date
winter_morts <- Dead %>% mutate(Month = month(StopDate), Year = year(StopDate), J = yday(StopDate)) %>% filter(J > yday("2001-12-01") | J < yday(Sys.Date())) %>% arrange(Year, Month)

#make december morts be the winter of that year and the next
winter_morts$Year <- ifelse(winter_morts$J > yday("2001-12-01"), winter_morts$Year + 1, winter_morts$Year)

#join with first, and filter out all who were collared March of that winter
winter_morts <- winter_morts %>% left_join(first %>% select(AID, FirstYear), by = "AID") %>% filter(Year > FirstYear)
str(winter_morts)


winter_mort_data <- winter_morts %>% group_by(Year) %>% summarise(n = n())  %>% mutate(wint = Wint)


winter_mort_data$n <- ifelse(winter_mort_data$n - (RDHexclude$CapM) < 0, 0, winter_mort_data$n - (RDHexclude$CapM))

# #finally, add up all dead in that year
# deaths <- Dead %>% group_by(Year) %>% summarise(deaths = n()); deaths

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
year <- c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023)

collarsYearly <- data.frame()
for(i in year){
  #i=2019
  xx <- data.frame(year = NA, collars = NA)
xx$year <- i
t <- sum %>% filter(minY < i & maxY >= i)
xx$collars <- nrow(t)
collarsYearly <- rbind(collarsYearly,xx)
}

collarsYearly

#2014 fix
collarsYearly <- collarsYearly[-1,]

collarsYearly <- collarsYearly %>% mutate(wint = Wint)



data <- collarsYearly %>% left_join(winter_mort_data, by = "wint")
data$perc <- data$n / data$collars
data$wintsurv <- (1- data$perc)
data


#-----------------------------------#
# Plot ####

#prepare labels
#morts in the winter
txt <- paste("2023 data current as of:")
x_lab <- "Winter (Jan 1 - May 23)"


morts <- ggplot(data)  + geom_bar(aes(x = as.character(wint), y = n), stat = "identity", fill = "grey", color = "black") + geom_hline(yintercept = mean(data$n), linewidth = 1.4, linetype = "dashed") + labs(x = x_lab, y = "Number of Deer", title = "Red Desert Mule Deer, Total Adult F Mortalities", caption = paste(txt, Sys.Date(),sep=" ")) + theme_bw() + coord_cartesian(ylim = c(-.25,26.25))+ scale_y_continuous(breaks = seq(0,26,2)) + theme(title = element_text(size = 10), plot.caption = element_text(size = 10,color = "grey50", face = "italic",hjust = 0.5));morts


perc.morts <- ggplot(data)  + geom_bar(aes(x = as.character(wint), y = perc*100), stat = "identity", fill = "grey", color = "black") + geom_hline(yintercept = mean(data$perc *100 ), linewidth = 1.4, linetype = "dashed") + labs(x = x_lab, y = "Percent of Sample", title = "Red Desert Mule Deer, % Adult F Sample Lost", caption = paste(txt, Sys.Date(),sep=" ")) + theme_bw() + coord_cartesian(ylim = c(-.001,22))+ scale_y_continuous(breaks = seq(0,22,2)) + theme(title = element_text(size = 10), plot.caption = element_text(size = 10,color = "grey50", face = "italic",hjust = 0.5)); perc.morts

perc.surv <- ggplot(data)  + geom_bar(aes(x = as.character(wint), y = wintsurv *100), stat = "identity", fill = "grey", color = "black") + geom_hline(yintercept = mean(data$wintsurv *100 ), linewidth = 1.4, linetype = "dashed") + labs(x = x_lab, y = "Percent of Sample", title = "Red Desert Mule Deer, % Adult F Survival", caption = paste(txt, Sys.Date(),sep=" ")) + theme_bw() + coord_cartesian(ylim = c(49,100))+ scale_y_continuous(breaks = seq(50,100,5)) + theme(title = element_text(size = 10), plot.caption = element_text(size = 10,color = "grey50", face = "italic",hjust = 0.5));perc.surv


#------------------------------#
# Export plots ####
ggsave(morts, file = "C:/Users/lwilde2/Desktop/RedDesert_WinterMorts_04242023_new.png", width = 6, height = 4)
ggsave(perc.morts, file = "C:/Users/lwilde2/Desktop/RedDesert_WinterMortsPercentOfSample_04242023_new.png", width = 6, height = 4)
ggsave(perc.surv, file = "C:/Users/lwilde2/Desktop/RedDesert_AdultWinterSurvival_05232023_new.png", width = 6, height = 4)
