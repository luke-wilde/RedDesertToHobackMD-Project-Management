#Code for analyzing greenscape parameters in the Sublette Mule Deer Herd
#Written by Ellen Aikens, Wyoming Cooperative Fish and Wildlife Research Unit

#Load required libraries
require(raster)
require(lubridate)
require(sp)
require(geoR)
library(ggplot2)
library(ggridges)

#Import key functions
setwd("D:/R-code/Functions")
source("keyFunctions.R")

#Import bischof calculations with dlc parameters used to calculate greenscape metrics
dlcparams.wd<-"D:/R-code/NDVI_Data/Bischof_calculations"
setwd(dlcparams.wd)
irg.params<-stack("dlcparams.grd")
indvi<-stack("csumNDVImax.grd")


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
  gps.data.ay<-gps.data.latlong[gps.data.latlong$id_yr==AY_ID[i], ]
  
  #extract the phenology data
  sd.vals<-extract(x=sd.irgDate, gps.data.ay)
  irg.vals<-extract(x=irg.max.day, gps.data.ay)
  scale.vals<-extract(x=spring.scale, gps.data.ay)
  indvi.vals<-extract(x=indvi.y, gps.data.ay)
  n.cells<-length(unique(extract(x=spring.scale, gps.data.ay, cellnumbers=T)[, 1]))
  
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

#Save surfing data with greenscape metrics
write.csv(surfing.data,"D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/DataAnalyses/Chapter1_SurfingTheGreenWave/Results/GreenwaveSurfing_GreenscapeMetrics/GreenScapeMetrics_2011_to_2019_01MAR2021.csv")
