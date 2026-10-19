require(raster)
require(lubridate)
require(sp)
require(geoR)

# #UW computer 
username<-"eaikens"
# #laptop
# username<-"Ellen O Aikens"
# #UiO computer 
# username<-"ellenoa"

setwd(paste0("C:/Users/", username ,"/Box Sync/"))
source("Surfing_Greeenscape_functions.R")


rdata.wd<-paste0("C:/Users/", username, "/Box Sync/Multispecies GWH/Data/Collar Data/cleaned/RData")

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

    
      
setwd(rdata.wd)
spPop<-substr(as.character(file.names[l]), 0, nchar(as.character(file.names[l]))-8)

load(paste0(file.names[l], ".RData"))

#set cut-off
cut.off<-20

AY_ID<-unique(gps.data$AY_ID)

for(i in 1:length(AY_ID)){
  
  year<-strsplit(as.character(AY_ID[i]), "_")[[1]][2]
  
  if(year>=2001 & year<=2020){
    #get IRG data
    irg.max.day<-irg.params[[paste0("xmidS_", year)]]
    spring.scale<-irg.params[[paste0("scalS_", year)]]
    sd.irgDate<-irg.params[[paste0("xmidS_SD_", year)]]
    
    
    #subset gps data 
    gps.data.ay<-gps.data[gps.data$AY_ID==AY_ID[i], ]
    
    #extract the phenology data 
    sd.vals<-raster::extract(x=sd.irgDate, gps.data.ay)
    irg.vals<-raster::extract(x=irg.max.day, gps.data.ay)
    scale.vals<-raster::extract(x=spring.scale, gps.data.ay)
    
    
    #filter out really poor fitting IRG curves
    idx<-which(sd.vals>cut.off)
    if(length(idx)>0){
      gps.data.ay<-gps.data.ay[-idx, ]
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
                                        dir=paste0("C:/Users/", username, "/Box Sync/Multispecies GWH/Figures/IndividualVisualizations/cleaned"))
    
    #calculate surfing 
    mid.i<-greenscapeVals[6]
    #used points greenscape 
    surfingVals120<-getSurfingMetricsVals(spDF=gps.data.ay, 
                                          irg.vals=irg.vals, 
                                          scale.vals=scale.vals, 
                                          startJDay=mid.i-60, endJDay=mid.i+60)
    
    
    
    #landscape-scale greenscape
    sp.greensscape<-getSvGreenscapeCirle(circles=circ.sp, 
                                         irgRaster=irg.max.day, 
                                         scaleRaster=spring.scale,
                                         sdRaster=sd.irgDate, sdCutoff=cut.off,
                                         PlotVariogram=T, plotColors=colPalIRG, saveJPEG=F, 
                                         dir=paste0("C:/Users/", username, "/Box Sync/Multispecies GWH/Figures/GreenscapeAvailabilityPlots"))
    
    
    #put it all together and save it 
    
    metaSurf.i<-data.frame(AnimalID=splitID[4], 
                           Year=splitID[5], 
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
