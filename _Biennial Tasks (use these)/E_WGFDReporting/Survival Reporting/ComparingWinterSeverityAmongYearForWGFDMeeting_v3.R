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
meta <- "C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_AllDeerIDs_21SEPTEMBER2024.csv" # this is where your Ortega style metadata file is, make sure its the most current


#--------------------------------------#
# Additional information ####

#exclude Vehicle and Capture related mortalities as these are not natural mortalities
Wint <- c("14-15","15-16","16-17","17-18","18-19","19-20","20-21","21-22","22-23","23-24")
CapM <- c(0,3,4,2,0,5,1,3,4,2)
#c(0,3,4,2,0,5,1,3,4) #without fawns
#c(0,3,4,2,0,5,2,4,5) #with fawns

VehM <- c(0,0,0,0,0,2,1,1,0,0)

RDHexclude <- data.frame(Wint = Wint, CapM = CapM, VehM = VehM)
RDHexclude

fawns <- c(seq(850,899,1))

#-----------------------------------#
# Load and prep metadata ####
DeerID <- read.csv(meta) 
str(DeerID) #check structure

#reformat Dates
DeerID$StopDate <- as.POSIXct(DeerID$StopDate, format = "%m/%d/%Y", tz = "MST")
DeerID$StartDate <- as.POSIXct(DeerID$StartDate, format = "%m/%d/%Y", tz = "MST")

#fawns with spring mig
x <- DeerID %>% filter(Fawn.Sex != "M") %>% filter(is.na(StopDate) | StopDate - StartDate > 120) %>% distinct(AID)
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
Dead <- last[which(!is.na(last$StopDate) & last$CollarMalfDate == ""),] %>% mutate(Year = year(StopDate)) 

# now, which of those dead were after Dec 1 but before todays date
winter_morts <- Dead %>% mutate(Month = month(StopDate), Year = year(StopDate), J = yday(StopDate)) %>% filter(J > yday("2001-12-01") | J < yday("2001-04-24")) %>% arrange(Year, Month) #Sys.Date()

#make december morts be the winter of that year and the next
winter_morts$Year <- ifelse(winter_morts$J >= yday("2001-12-01"), winter_morts$Year + 1, winter_morts$Year)

#join with first, and filter out all who were collared March of that winter
winter_morts <- winter_morts %>% left_join(first %>% select(AID, FirstYear), by = "AID") %>% filter(Year > FirstYear)
str(winter_morts)


winter_mort_data <- winter_morts %>% group_by(Year) %>% summarise(n = n())  %>% mutate(wint = Wint)


winter_mort_data$n <- ifelse(winter_mort_data$n - (RDHexclude$CapM) < 0, 0, winter_mort_data$n - (RDHexclude$CapM))

# #finally, add up all dead in that year
deaths <- Dead %>% group_by(Year) %>% summarise(deaths = n()); deaths

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

#-------------------------------------#
# AIDs in this FY for USGS report ####
#-------------------------------------#

AID_Y <- sum %>% filter(minY <= 2023) %>% filter(maxY == 2024) %>% select(AID)

DeerID %>% filter(AID %in% AID_Y$AID) %>% group_by(Fawn.Sex) %>% summarise(n = n())

DeerID %>% filter(Fawn.Sex %in% c("", "F")) %>% distinct(AID, Mgtry)  %>% group_by(Mgtry) %>% summarise(n = n())



#add
year <- c(2014,2015,2016,2017,2018,2019,2020,2021,2022,2023,2024)

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
txt <- paste("2024 data current as of:")
x_lab <- paste("Winter (Dec 1 - Apr 24)",sep="")


morts <- ggplot(data)  + geom_bar(aes(x = as.character(wint), y = n), stat = "identity", fill = "grey", color = "black") + geom_hline(yintercept = mean(data$n), linewidth = 1.4, linetype = "dashed") + labs(x = x_lab, y = "Number of Deer", title = "Red Desert Mule Deer, Total Adult F Mortalities", caption = paste(txt, Sys.Date(),sep=" ")) + theme_bw() + coord_cartesian(ylim = c(-.25,26.25))+ scale_y_continuous(breaks = seq(0,26,2)) + theme(title = element_text(size = 10), plot.caption = element_text(size = 10,color = "grey50", face = "italic",hjust = 0.5));morts


perc.morts <- ggplot(data)  + geom_bar(aes(x = as.character(wint), y = perc*100), stat = "identity", fill = "grey", color = "black") + geom_hline(yintercept = mean(data$perc *100 ), linewidth = 1.4, linetype = "dashed") + labs(x = x_lab, y = "Percent of Sample", title = "Red Desert Mule Deer, % Adult F Sample Lost", caption = paste(txt, Sys.Date(),sep=" ")) + theme_bw() + coord_cartesian(ylim = c(-.001,22))+ scale_y_continuous(breaks = seq(0,22,2)) + theme(title = element_text(size = 10), plot.caption = element_text(size = 10,color = "grey50", face = "italic",hjust = 0.5)); perc.morts

perc.surv <- ggplot(data)  + geom_bar(aes(x = as.character(wint), y = wintsurv *100), stat = "identity", fill = "grey", color = "black") + geom_hline(yintercept = mean(data$wintsurv *100 ), linewidth = 1.4, linetype = "dashed") + labs(x = x_lab, y = "Percent of Sample", title = "Red Desert Mule Deer, % Adult F Survival", caption = paste(txt, Sys.Date(),sep=" ")) + theme_bw() + coord_cartesian(ylim = c(49,100))+ scale_y_continuous(breaks = seq(50,100,5)) + theme(title = element_text(size = 10), plot.caption = element_text(size = 10,color = "grey50", face = "italic",hjust = 0.5));perc.surv


#------------------------------#
# Export plots ####
ggsave(morts, file = "C:/Users/lwilde2/Downloads/RedDesert_WinterMorts_04242023_new.png", width = 6, height = 4)
ggsave(perc.morts, file = "C:/Users/lwilde2/Downloads/RedDesert_WinterMortsPercentOfSample_04242023_new.png", width = 6, height = 4)
ggsave(perc.surv, file = "C:/Users/lwilde2/Downloads/RedDesert_AdultWinterSurvival_05232023_new.png", width = 6, height = 4)




#---------------------------#
# survival rates for Dean ####

# now, which of those dead were after Dec 1 but before todays date
winter_morts23 <- Dead %>% mutate(Month = month(StopDate), Year = year(StopDate), J = yday(StopDate)) %>% filter(Year %in% 2023) %>% filter(J >= yday("2023-01-01") | J <= yday("2023-06-01")) %>% arrange(Year, Month)
winter_morts22 <- Dead %>% mutate(Month = month(StopDate), Year = year(StopDate), J = yday(StopDate)) %>% filter(Year %in% 2022) %>% filter(J >= yday("2022-12-01") | J <= yday("2022-12-31")) %>% arrange(Year, Month)


#survival rates for dean
nrow(rbind(winter_morts23, winter_morts22))


#how many collars in each year

range <- DeerID %>% filter(!Fawn.Sex %in% c("M")) %>% group_by(AID) %>% summarise(min = min(StartDate), max = max(StopDate)) %>% print(n = nrow(.))


alive <- range[which(is.na(range$max)),]
died <- range[which(!is.na(range$max)),]

alive$max <- rep(Sys.Date(), nrow(alive))

sumA <- alive %>% mutate(minY = year(min), maxY = year(max)) %>% arrange(minY, maxY)
sumD <- died %>% mutate(minY = year(min), maxY = year(max)) %>% arrange(minY, maxY)

sum <- rbind(sumA,sumD)

# sum %>% filter(minY <= 2022) %>% filter(max >= as.Date("2022-12-01")) %>% arrange(max) %>% print(n = nrow(.)) %>% summarise(dead = ifelse(max <= as.Date("2023-06-01"), 1, 0)) %>% print(n = nrow(.)) %>% summarise(rate = sum(dead)/nrow(.))
# 
# sum %>% dplyr::filter(min < as.POSIXct(ydm("2022-12-01"))) %>% filter(max >= as.Date(ydm("2022-12-01"))) %>% arrange(max) %>% print(n = nrow(.)) %>% summarise(dead = ifelse(max >= as.Date(ydm("2022-12-01")), ifelse(max <= as.Date(ydm("2023-06-01")), 1, 0),0)) %>% print(n = nrow(.)) %>% summarise(rate = sum(dead)/nrow(.))

# x <- sum %>% as.data.frame() %>% filter(max >= as.Date("2022-12-01") & max <= as.Date("2023-06-01")) %>% dplyr::filter(min < as.POSIXct("2022-11-30 00:00:00")) %>% arrange(max) %>% mutate(dead = ifelse(max >= as.Date(ydm("2022-12-01")), ifelse(max <= as.Date(ydm("2023-06-01")), 1, 0),0)) #%>% print(n = nrow(.))

x <- sum %>% mutate(death = ifelse(max == Sys.Date(), 0, 1)) %>% rename(animalid = AID, capdate = min, lastobs = max) %>% arrange(capdate, lastobs); x

x <- sum %>% as.data.frame() %>% dplyr::filter(min < as.POSIXct("2022-11-30 00:00:00") & max >= as.POSIXct("2022-11-30 00:00:00")) %>% mutate(DiedWinter22t23 = ifelse(max >= as.Date("2022-12-01") & max <= as.Date("2023-06-01"), 1, 0)) %>% arrange(DiedWinter22t23) %>% as.data.frame();x



# write.csv(x[c(1:90),], "C:/Users/lwilde2/Downloads/RDH_AdultFemaleSurvival_DEC12022toJUN12023.csv", row.names = F)
write.csv(x, "C:/Users/lwilde2/Downloads/RDH_AdultFemaleSurvival_Mar2014toFeb2024.csv", row.names = F)

