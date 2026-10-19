#Code for fitting cleanVals with spline function
#Written by Anna Ortega, Wyoming Cooperative Fish and Wildlife Research Unit
#Anna C. Ortega, Jerod A. Merkle, and Matthew J. Kauffman conceived the idea
#October 3, 2022

#Load required libraries
library(dplyr)
library(lubridate)
library(raster)
library(rgdal)
library(tidyr)
library(stringr)

#Clean working environment
rm(list=ls())

#Import and prepare data for extraction ####
#Import GPS data of autumn migrations (clean and ready for analyses)
# setwd("D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/DissertationChapters/Chapter2_FrostWaveHypothesis/Data")
# load("RD2H_AutumnMigrations_2011_thru_2020_CleanForAnalyses.RData")
load("Data/GeometricAnalysesAndFunctionality_07282023.RData")
d <- allF

#Ensure that data is ordered by id_yr and date
d<-d[order(d$id_yr,d$POSIXct),] #d is dataframe with GPS points

#Import cleanVals
source("Z:/MODIS_NDVI/info/ndviEXTRACT.R")
source("Z:/MODIS_NDVI/info/ndviEXTRACT2.R")

names(d)

ndvi.wd<-"Z:/MODIS_NDVI/Bischof_calculations"
setwd(ndvi.wd)

#Stack the 964 rasters with cleanVals
cleanVals<-stack("cleanVals.grd")
names(cleanVals)

#Load R data with date info and create separate dataframe
#setwd(ndvi.wd)
load("ndvi8dayDates.RData")
ndvi.dates<-as.data.frame(ndvi8dayDates)
range(ndvi.dates$ndvi8dayDates)

#Now, rename raster layers with corresponding NDVI dates
#Must begin raster layer with a letter...not a number...
#otherwise default is that each layer begins with 'X'
library(tidyr)
ndvi.dates<-tibble(ndvi.dates)
ndvi.dates <- ndvi.dates %>% separate(ndvi8dayDates, into = c("Year", "Month", "Day"))
ndvi.dates$date<-as.Date(paste(ndvi.dates$Year,ndvi.dates$Month,ndvi.dates$Day,sep="-"),format="%Y-%m-%d")
ndvi.dates <- ndvi.dates %>% mutate(Month = month.abb[as.numeric(Month)])
ndvi.dates$date.name<-paste(ndvi.dates$Month,ndvi.dates$Day,ndvi.dates$Year,sep=".")
head(ndvi.dates)

names(cleanVals)<-ndvi.dates$date.name
names(cleanVals)

#Temporarily subset one unique year to work through code
d.20<-subset(ndvi.dates,ndvi.dates$Year=="2020")
n<-as.numeric(nrow(d.20)) #Determine number of data points per year #should be 46 for non-leap years

#Create column to represent a unique point
d$temp<-1

f<-d %>%
  group_by(id_yr) %>%
  mutate(pointid = cumsum(temp))
f$id_yr_point<-paste(f$id_yr,f$pointid,sep="_")

#Save GPS data to extract fitted NDVI values later on
gps.data<-f
head(gps.data)

#Create list of unique dates
d.list<-unique(d.20$date.name)
date.list<-unique(d.20$date)

#For each unique spatial point, replicate by number of unique datapoints
names(f)
s<-f %>% group_by(id_yr_point) %>%
  slice(rep(1:n(), each = n))

#For each unique point in the dataframe, generate a date 
#based on the cleanVals dates for 46 timestamps

#Use replicate...way faster than looping through each point ID
obs<-nrow(f); obs
d1<-rep(d.list,obs)
d2<-rep(date.list,obs)

s$date.long<-d1
s$date<-d2

#Make dataframe spatially aware
df<-as.data.frame(s)
head(df)
proj <- "+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs+towgs84=0,0,0"   #name a new proj
coordinates(df) <- c("x","y")
proj4string(df) <- CRS(proj)

#Extract NDVI data for all GPS relocations ####
#This will extract NDVI values for 8-day resolution
projNDVI<-cleanVals[[1]]@crs
t<-spTransform(df, projNDVI)
head(t)

t$fittedVals<-NA

for(i in 1:length(d.list)){
  a<-t[t$date.long==d.list[i],]
  l<-subset(cleanVals,d.list[i])
  t$fittedVals[t$date.long==d.list[i]]<-raster::extract(l,a)
}

t<-as.data.frame(t)

#Create day of year column
t$jul<-yday(t$date)
head(t)

#Remove any cleanVals with NA
#Note, it's important to keep track of how many values you remove 
#for methods...it shouldn't be large (i.e., <1% of all GPS points)
table(is.na(t$fittedVals))
range(t$fittedVals,na.rm = TRUE)
names(t)

u1<-unique(t$id_yr_point)
t<-t[!is.na(t[,12]),] #This needs to be the column with NDVI values
table(is.na(t$fittedVals))
u2<-unique(t$id_yr_point)
head(t)

#Clean working environment
rm(a,cleanVals,d,d.20,df,l,ndvi.dates,projNDVI,s,
   d.list,d1,d2,date.list,i,n,ndvi.wd,ndvi8dayDates,proj,f)

#Now fit NDVI values with spline
library(splines)
library(ggplot2)

#Create empty dataframe to store fitted NDVI values
final<-data.frame(jul=NA,
                  fit=NA,
                  se.fit=NA,
                  lower=NA,
                  upper=NA,
                  id_yr_point=NA)
final<-final[0,]
head(final)

p.list<-unique(t$id_yr_point)
l<-unique(t$id_yr_point)

p<-length(p.list)

library(foreach)
library(doParallel)

foreach(i = 1:length(p.list),.combine=rbind) %do% {
  temp.1<-t[t$id_yr_point==p.list[i],]
  sm <- smooth.spline(temp.1$jul, temp.1$fittedVals, cv=TRUE,spar=.4)
  k<-as.numeric(round(sm$df))
  fit <- lm(fittedVals~ns(jul,df=k), data=temp.1)
  pred = predict(fit, newdata = list(jul = 1:365), se = TRUE)
  jul<-seq(1,365,1) #Must change '365' here and on previous line to 366 if working with leap years
  se_bands = with(pred, cbind("upper" = fit+(1.96*se.fit), 
                              "lower" = fit-(1.96*se.fit)))
  new<-as.data.frame(cbind(jul,pred$fit,pred$se.fit,se_bands))
  colnames(new)<-c("jul","fit","se.fit","upper","lower")
  new$id_yr_point<-unique(temp.1$id_yr_point)  
  final<-rbind(final,new)
  #Following lines of code will give you an estimated percent of points completed
  h1<-as.data.frame(unique(final$id_yr_point))
  h2<-as.numeric(nrow(h1))
  h3<-paste(round(((h2/p)*100),digits=2),"% complete",sep=" ")
  plot(sm,main=h3)
  rm(sm,k,fit,pred,jul,se_bands,new)
}

final.list<-unique(final$id_yr_point)

length(final.list)==length(p.list) #Must be TRUE

out<-subset(final,is.na(final$se.fit)==TRUE) #Should be 0
u<-unique(final$id_yr_point)

#Rename dataframe
f<-final

#Are there any values below zero?
#If so, replace negative values with zero...most likely masked by snow
range(f$fit)
f$fit<-ifelse(f$fit<0,0,f$fit)
range(f$fit)

library(stringr)
f$id <- str_split_fixed(f$id_yr_point, "_", 3)[,1]
f$year <- str_split_fixed(f$id_yr_point, "_", 3)[,2]
f$id_yr<-paste(f$id,f$year,sep="_")
head(f)

#Clean working environment
rm(out,t,p.list,u,l,obs,p,h1,final,temp.1,final.list,h2,h3,i,id.list,u1)

#Now, extract fitted NDVI values to the GPS points used in your analyses
gps.data<-as.data.frame(d)
gps.data<-gps.data[(gps.data$id_yr_point %in% f$id_yr_point),]
gps.data$jul<-yday(gps.data$date)
gps.data$NDVI<-NA
p<-gps.data[0,]
head(p)

point.list<-unique(gps.data$id_yr_point)

for(i in 1:length(point.list)){
  a<-subset(gps.data,gps.data$id_yr_point==point.list[[i]])
  b<-subset(f,f$id_yr_point==point.list[[i]])
  b$diff<-abs(a$jul-b$jul)
  sub<-b[which.min(b$diff),]
  a$NDVI<-sub$fit
  p<-rbind(p,a)
}


head(p)
tail(p)
table(is.na(p$NDVI))
range(p$NDVI)

#Clean working environment
rm(a,b,sub,gps.data,i,point.list)

