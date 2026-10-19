
rm(list=ls())

require(raster)
require(lubridate)
require(sp)
require(sf)
require(geoR)
require(dplyr)
require(rgeos)
require(tidyverse)

#import data
#set your working drive
setwd("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData")

load("RD2H_CleanAllData.RData")
data <- findata
data <- data %>% mutate(id_yr = paste(AID,"_",Year,sep="")) %>%  filter(id_yr %in% c("108_2019", "255_2019") & POSIXct <= "2019-07-01 00:00:00" & POSIXct >= "2019-03-01 00:00:00" )


#mule deer data
proj_md <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
datamd <- st_as_sf(data, coords=c("geometry"), dim="XY", crs=proj_md)

datamd$species <- "deer"
names(datamd)[names(datamd) == 'AID'] <- 'id'
names(datamd)[names(datamd) == 'POSIXct'] <- 'date'

# #change AID column
# datamd<- datamd %>%
#  separate(id, into = c("A", "id"), sep = "_", remove = TRUE) %>%
#  dplyr::mutate(id_ = "BHD_") %>%
#  tidyr:: unite('id', id_,id, sep="", remove = TRUE)

datamd <- datamd[,c("id_yr","date","Year","geometry","species", "id")]



proj_new <- "+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs+towgs84=0,0,0"
#st_crs(data)
datamd <- st_transform(datamd, proj_new)   #this is the function to reproject an sf object

#rm(proj_new)

# add in long lat column
datamd[,c('x','y')] <- st_coordinates(datamd)



d <- datamd %>% st_drop_geometry()

head(d)

table(is.na(d))
d<- as.data.frame(d)

d <- d %>% select(id_yr, date, x, y, id, Year, species)

# get rid of extra columns
names(d) <- c("id_yr", "date", "x", "y", "id", "year", "species")

# order by id_yr
d<-d[order(d$id_yr,d$date),]
head(d)

#take out data that we don't have NDVI for
#d <- d[d$year %in% c("2016","2017","2018","2019","2020", "2021"),]

#d<- st_as_sf(d, coords = c("x", "y"), crs = proj_new )



mapview::mapview(datamd, zcol = "id_yr")


#Import bischof calculations with dlc parameters used to calculate greenscape metrics
dlcparams.wd<-"Z:/MODIS_NDVI/Bischof_calculations"
setwd(dlcparams.wd)

irg.params<-stack("dlcparams.grd")
indvi<-stack("csumNDVImax.grd")


names(irg.params)


setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/RCode/Surfing/Greenscape_SurfingScores")
source("Surfing_Greeenscape_functions.R")

#data is here
d

#IRG NDVI are here
irg.params
indvi


#-----------------------------------------#
# Begin the functions - CKyle ####


# #UW computer 
#username<-"eaikens"
# #laptop
# username<-"Ellen O Aikens"
# #UiO computer 
# username<-"ellenoa"


# START #

 colPalIRG<-colorRampPalette(c("#8c510a", "#bf812d", "#dfc27d", 
                               "#f6e8c3", "#f5f5f5", "#c7eae5",
                               "#80cdc1", "#35978f", "#01665e"))(210)
 
 metaSurf<-data.frame(AnimalID="", 
                      Year=2017, 
                      AY_ID="", 
                      IRG_120=0.9, 
                      DFP_120=5.5, 
                      IRG_90=0.9, 
                      DFP_90=5.5, 
                      IRG_60=0.9, 
                      DFP_60=5.5, 
                      svSlope=1, 
                      greenUpDur=34.5, 
                      springScale=15.5,
                      SVmax="",
                      midpoint=100)[0, ]
 
 
 
 par(mfrow=c(2, 2))
 
     
       
#setwd(rdata.wd)
#spPop<-substr(as.character(file.names[l]), 0, nchar(as.character(file.names[l]))-8)
 
#load(paste0(file.names[l], ".RData"))

#Clarify projected coordinate system
str(d)
#d <- as.data.frame(d)
coordinates(d)<-c("x", "y")
proj4string(d)<-CRS("+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs+towgs84=0,0,0")
projIRG<-irg.params[[1]]@crs
gps.data<-spTransform(d, projIRG)
gps.data$Timestamp<-gps.data$date

# colPalIRG<-colorRampPalette(c("#8C510A", "#BF812D", "#DFC27D",
#                               "#F6E8C3", "#F5F5F5", "#C7EAE5",
#                               "#80CDC1", "#35978F", "#01665E"))(210)
# surfing.data<-data.frame(AnimalID="", Year=2010, AY_ID="", strategy="",
#                          IRG_120=0.9, DFP_120=5.5,
#                          IRG_90=0.9, DFP_90=5.5,
#                          IRG_60=0.9, DFP_60=5.5,
#                          IRG_custom=0.9, DFP_custom=5.5,
#                          svSlope=1,
#                          greenUpDur=34.5, springScale=15.5, midpoint=100,
#                          lowerQuant=100, upperQuant=180,
#                          n.cells="", iNDVI="", iNDVi_uniquePixels="")[0, ]
# par(mfrow=c(2, 2))
# 
# head(surfing.data)

#set cut-off
cut.off<-20

gps.data$AY_ID <- gps.data$id_yr
AY_ID<-unique(gps.data$AY_ID)

#
# sps<-c("elk", "deer", "moose") #max meters travelled, max displacement
# max.meters<-c(37500.000, 18935.625, 37500.876)
# names(max.meters)<-sps

max.meters <-37500.000


for(i in 1:length(AY_ID)){
  
  year<-strsplit(as.character(AY_ID[1]), "_")[[1]][2]
  
  if(year>=2001 & year<=2021){
    #get IRG data
    irg.max.day<-irg.params[[paste0("xmidS_", year)]]
    spring.scale<-irg.params[[paste0("scalS_",  year)]]
    sd.irgDate<-irg.params[[paste0("xmidS_SD_",  year)]]
    ndvi<-indvi[[paste0("X",  year)]]
    names(ndvi)
   
    #subset gps data 
    gps.data.ay<-gps.data[gps.data$AY_ID==AY_ID[i], ]
    
    #extract the phenology data 
    
    sd.vals<-raster::extract(x=sd.irgDate, gps.data.ay)
    irg.vals<-raster::extract(x=irg.max.day, gps.data.ay)
    scale.vals<-raster::extract(x=spring.scale, gps.data.ay)
    indvi.vals<-raster::extract(x=ndvi, gps.data.ay)
    
    #filter out really poor fitting IRG curves
    idx<-which(sd.vals>cut.off)
    if(length(idx)>0){
      gps.data.ay<-gps.data.ay[-idx, ]
      sd.vals<-sd.vals[-idx]
      irg.vals<-irg.vals[-idx]
      scale.vals<-scale.vals[-idx]
      indvi.vals<-indvi.vals[-idx]
    }
    
    
    #calculate greenscape 
    greenscapeVals<-getSvGreenscapeVals(spDF=gps.data.ay, 
                                        irg.vals=irg.vals, 
                                        scale.vals=scale.vals,
                                        PlotVariogram=T, 
                                        saveJPEG=F, 
                                        dir=paste0("C:/Users/ckyle/My Drive/CAK/Projects/Bighorns/Bighorns_Data/Surfing_Data/GWH_visualizaions"))
    
    #calculate surfing 
    mid.i<-greenscapeVals[6]
    #used points greenscape 
    
    surfingVals120<-getSurfingMetricsVals(spDF=gps.data.ay, 
                                          irg.vals=irg.vals, 
                                          scale.vals=scale.vals, 
                                          startJDay=mid.i-60, endJDay=mid.i+60)
    
    
    #make "availability" buffer
    # sp.buffer<-max.meters[which(names(max.meters)==species)]
    sp.buffer<-37500.000
    centroid<-apply(coordinates(gps.data.ay), MARGIN=2, FUN=mean, na.rm=T)
    xy<-data.frame(x=centroid[1], y=centroid[2])
    row.names(xy)<-AY_ID[i]
    centroid.sp<-SpatialPointsDataFrame(SpatialPoints(xy), data.frame(ID=as.character(AY_ID[i])))
    proj4string(centroid.sp)<-CRS("+proj=longlat +ellps=WGS84")
    centroid.sp<-spTransform(centroid.sp, CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
    circ.sp<-gBuffer(centroid.sp, width=sp.buffer, byid=F)
    circ.sp<-spTransform(circ.sp, CRS("+proj=longlat +ellps=WGS84"))
    
    #landscape-scale greenscape
    sp.greensscape<-getSvGreenscapeCirle(circles=circ.sp, 
                                         irgRaster=irg.max.day, 
                                         scaleRaster=spring.scale,
                                         sdRaster=sd.irgDate, sdCutoff=cut.off,
                                         PlotVariogram=T, plotColors=colPalIRG, saveJPEG=F, 
                                         dir=paste0("C:/Users/ckyle/My Drive/CAK/Projects/Bighorns/Bighorns_Data/Surfing_Data/GreenscapeAvailabilityPlots"))
    
    
    
    
    #put it all together and save it 
    # add in available greenscape metrics in the dataframe, once species-specific bufers are calculated 
    metaSurf.i<-data.frame(AnimalID=gps.data.ay$id[1], 
                           Year=gps.data.ay$year[1], 
                           AY_ID=AY_ID[i], 
                           IRG_120=surfingVals120[1], 
                           DFP_120=surfingVals120[2], 
                           svSlope=greenscapeVals[1], 
                           greenUpDur=greenscapeVals[4], 
                           springScale=greenscapeVals[5], 
                           SVmax=greenscapeVals[14], 
                           midpoint=mid.i)
    
    metaSurf<-rbind(metaSurf, metaSurf.i)
  }
}

head(metaSurf)

#save.image("C:/Users/lwilde2/Documents/AdaptiveFidelity/Data/GreenscapeOutput_02152023.RData")

#------------------------------------#
# Begin functinos A Ortega ####

#Import key functions
setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/RCode/Surfing/")
source("keyFunctions_greenscape.R")


colPalIRG<-colorRampPalette(c("#8c510a", "#bf812d", "#dfc27d",
                                       "#f6e8c3", "#f5f5f5", "#c7eae5",
                                       "#80cdc1", "#35978f", "#01665e"))(210)
                                       

#this is the structure of the dataframe that it will ouptut
surfing.data<-data.frame(AnimalID="", Year=2010, AY_ID="", strategy="",
                         IRG_120=0.9, DFP_120=5.5, #this is where she standardized a period of spring green up, to allow you to characterize what everone is experiencing. Median date of peak IRG across all routes -60 and +60 days. This works for RDH
                         IRG_90=0.9, DFP_90=5.5,
                         IRG_60=0.9, DFP_60=5.5,
                         IRG_custom=0.9, DFP_custom=5.5,
                         svSlope=1, # svSlope is order, higher is more consecutive
                         greenUpDur=34.5, #duration
                         springScale=15.5 #inverse of greenup rate - time to reach peak green, lower?
                         , midpoint=100,
                         lowerQuant=100, upperQuant=180,
                         n.cells="", iNDVI="", iNDVi_uniquePixels="")[0, ] #integrated NDVI, number of cells they interacted with. Every row is an animal surfing score 

#NOTES 
#data you want the majority of animal year -- needs to be able to be flexible to find it
#some animals look like they have a spring dataset, mid May or early June morts will not work -- so clean to only have March to July
#semivariance is what calcs the rate and order
#as you run it you see the semivariograms
#Carrie had some issues

#integrated NDVI, cumulative sum (integrated, unscaling the fitted vals than subtracting the min)




par(mfrow=c(2, 2))

AY_ID<-unique(gps.data$id_yr)

for(i in 1:length(AY_ID)){
  
  year<-strsplit(as.character(AY_ID[i]), "_")[[1]][2]
  
  #get IRG data
  irg.max.day<-irg.params[[paste0("xmidS_", year)]]
  spring.scale<-irg.params[[paste0("scalS_", year)]]
  sd.irgDate<-irg.params[[paste0("xmidS_SD_", year)]]
  
  #get INDVI data
  indvi.y<-indvi[[paste0("X", year)]]
  
  #subset gps data
  gps.data.ay<-gps.data[gps.data$id_yr==AY_ID[i], ]
  
  #extract the phenology data
  sd.vals<-extract(x=sd.irgDate, y=gps.data.ay)
  irg.vals<-extract(x=irg.max.day, y=gps.data.ay)
  scale.vals<-extract(x=spring.scale, y=gps.data.ay)
  indvi.vals<-extract(x=indvi.y, y=gps.data.ay)
  n.cells<-length(unique(extract(x=spring.scale, y=gps.data.ay, cellnumbers=T)[, 1]))
  
  View(indvi.vals)
  head(indvi.vals)
  #filter out really poor fitting IRG curves
  # idx<-which(sd.vals>cut.off)
  # if(length(idx)>0){
  #   gps.data.ay<-gps.data.ay[-idx, ]
  #   irg.vals<-irg.vals[-idx]
  #   scale.vals<-scale.vals[-idx]
  #   indvi.vals<-indvi.vals[-idx]
  # }
  #
  #calculate greenscape
  greenscapeVals<-getSvGreenscapeVals(spDF=gps.data.ay,
                                      irg.vals=irg.vals,
                                      scale.vals=scale.vals,
                                      PlotVariogram=T,
                                      saveJPEG=F,
                                      dir=paste0("D:/WYO_COOP/GWHFigures"))
  
  head(greenscapeVals)
  
  #calculate surfing
  mid.i<-greenscapeVals[6]
  surfingVals120<-getSurfingMetricsVals(spDF=gps.data.ay,
                                        irg.vals=irg.vals,
                                        scale.vals=scale.vals,
                                        startJDay=mid.i-60, endJDay=mid.i+60)
  
  surfingVals90<-getSurfingMetricsVals(spDF=gps.data.ay,
                                       irg.vals=irg.vals,
                                       scale.vals=scale.vals,
                                       startJDay=mid.i-45, endJDay=mid.i+45)
  
  surfingVals60<-getSurfingMetricsVals(spDF=gps.data.ay,
                                       irg.vals=irg.vals,
                                       scale.vals=scale.vals,
                                       startJDay=mid.i-30, endJDay=mid.i+30)
  
  surfingVals.custom<-getSurfingMetricsVals(spDF=gps.data.ay,
                                            irg.vals=irg.vals,
                                            scale.vals=scale.vals,
                                            startJDay=round(greenscapeVals[7]),
                                            endJDay=round(greenscapeVals[8]))
  head(getSurfingMetricsVals)
  
  #put it all together and save it
  GlobalID.i<-as.character(gps.data.ay$id_yr[1])
  splitID<-unlist(strsplit(GlobalID.i, "_"))
  
  surfing.data.i<-data.frame(AnimalID=splitID[1],
                             Year=splitID[2],
                             AY_ID=AY_ID[i],
                             strategy=gps.data.ay$migration[1],
                             IRG_120=surfingVals120[1],
                             DFP_120=surfingVals120[2],
                             IRG_90=surfingVals90[1],
                             DFP_90=surfingVals90[2],
                             IRG_60=surfingVals60[1],
                             DFP_60=surfingVals60[2],
                             IRG_custom=surfingVals.custom[1],
                             DFP_custom=surfingVals.custom[2],
                             svSlope=greenscapeVals[1],
                             greenUpDur=greenscapeVals[4],
                             springScale=greenscapeVals[5],
                             midpoint=mid.i,
                             lowerQuant=greenscapeVals[7],
                             upperQuant=greenscapeVals[8],
                             n.cells=n.cells,
                             iNDVI=mean(indvi.vals, na.rm=T),
                             iNDVi_uniquePixels=mean(unique(indvi.vals), na.rm=T)
  )
  
  surfing.data<-rbind(surfing.data, surfing.data.i)
}

