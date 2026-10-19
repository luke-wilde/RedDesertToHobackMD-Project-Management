
rm(list=ls())

require(raster)
require(lubridate)
require(sp)
require(sf)
require(geoR)
require(dplyr)
require(rgeos)
require(tidyr)

#import data
#set your working drive
setwd("C:/Users/ckyle/My Drive/CAK/Projects/Bighorns/Bighorns_Data/FinalCleaned_RDS")

data <- readRDS("BH_Elk_14OCT2022.rds")
datamd <- st_read("C:/Users/ckyle/My Drive/CAK/Projects/Bighorns/Bighorns_GIS/Shapefiles/DirectDownload/Mule Deer/16JAN2023","NBH_MuleDeer_2023-01-16")
datamo <- st_read("C:/Users/ckyle/My Drive/CAK/Projects/Bighorns/Bighorns_GIS/Shapefiles/DirectDownload/Moose/08AUG2022", "clean_BHM_8AUG2022")

#moose data organize
proj <- "+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs+towgs84=0,0,0"
# turn into sf object
datamo <- st_as_sf(datamo, coords=c("geometry"), dim="XY", crs=proj)

datamo$species <- "moose"
names(datamo)[names(datamo) == 'AID'] <- 'id'

#add Bighorn Elk part of id and add 20 to front of year
datamo <- datamo %>% dplyr::mutate(id_ = "BHM_") %>% 
                     tidyr:: unite('id', id_,id, sep="", remove = TRUE)

datamo <- datamo[,c("id","date","species")] 

#mule deer data
proj_md <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
datamd <- st_as_sf(datamd, coords=c("geometry"), dim="XY", crs=proj_md)

datamd$species <- "deer"
names(datamd)[names(datamd) == 'AID'] <- 'id'
names(datamd)[names(datamd) == 'LocalDT'] <- 'date'

#change AID column
datamd<- datamd %>%
 separate(id, into = c("A", "id"), sep = "_", remove = TRUE) %>%
 dplyr::mutate(id_ = "BHD_") %>%
 tidyr:: unite('id', id_,id, sep="", remove = TRUE)

datamd <- datamd[,c("id","date","geometry","species")]

data <- data[,c("id","date","species")]

proj_new <- "+proj=utm +zone=13 +ellps=WGS84 +datum=WGS84 +units=m +no_defs+towgs84=0,0,0"
st_crs(data)
datamd <- st_transform(datamd, proj_new)   #this is the function to reproject an sf object
data <- st_transform(data, proj_new)
datamo <-st_transform(datamo, proj_new)
st_crs(datamo)
rm(proj_new)

# add in long lat column
datamd[,c('x','y')] <- st_coordinates(datamd)
data[,c('x','y')] <- st_coordinates(data)
datamo[,c('x','y')] <- st_coordinates(datamo)

#combine data
data <-rbind(data,datamd,datamo)
rm(proj,proj_md,datamd,datamo)

# add month and day columns
data$year <- format(data$date,'%Y')
data <- data %>% tidyr:: unite('id_yr', id, year, sep="_", remove = FALSE)

# I'm keeping only the IDs with a significant number of points
ids2keep <- data %>% 
  group_by(id) %>% 
  summarise(n=n()) %>% 
  filter(n > 200) %>%   # keep only ids with more than 1000 points  # YOU'LL WANT TO CAHNGE THIS!
  pull(id)
length(ids2keep) # how many are you keeping?
length(unique(data$id)) # how many originally?

data <- data %>% # now reduce your data
  filter(id %in% ids2keep)
rm(ids2keep)


d <- data

table(is.na(d))
d<- as.data.frame(d)

# get rid of extra columns
d <- d[c("id_yr", "date", "x", "y", "id", "year", "species")]

# order by id_yr
d<-d[order(d$id_yr,d$date),]
head(d)

#take out data that we don't have NDVI for
d <- d[d$year %in% c("2016","2017","2018","2019","2020", "2021"),]

d<- st_as_sf(d, coords = c("x", "y"), crs = proj_new )
setwd("C:/Users/ckyle/My Drive/CAK/Projects/Bighorns/Bighorns_Data/FinalCleaned_RDS/AllData_16JAN2023.rds")
getwd()
saveRDS(d, "AllData_16JAN2023.rds")
#mapview::mapview(d)
#Import bischof calculations with dlc parameters used to calculate greenscape metrics
dlcparams.wd<-"C:/Users/ckyle/My Drive/CAK/Projects/Bighorns/RCode/MODIS_NDVI/Bischof_calculations"
setwd(dlcparams.wd)
irg.params<-stack("dlcparams.grd")
names(irg.params)
indvi<-stack("csumNDVImax.grd")

# #UW computer 
#username<-"eaikens"
# #laptop
# username<-"Ellen O Aikens"
# #UiO computer 
# username<-"ellenoa"

setwd("C:/Users/ckyle/My Drive/CAK/Projects/Bighorns/RCode/Surfing")
source("Surfing_Greeenscape_functions.R")



 colPalIRG<-colorRampPalette(c("#8c510a", "#bf812d", "#dfc27d", 
                               "#f6e8c3", "#f5f5f5", "#c7eae5",
                               "#80cdc1", "#35978f", "#01665e"))(210)
 
 metaSurf<-data.frame(AnimalID="", 
                      Year=2010, 
                      AY_ID="", 
                      IRG_120=0.9, 
                      DFP_120=5.5, 
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
  
  year<-strsplit(as.character(AY_ID[i]), "_")[[1]][3]
  
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
