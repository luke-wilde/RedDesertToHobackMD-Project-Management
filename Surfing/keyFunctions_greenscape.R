
#this function standardizes datasets, in terms of format, time zone, and projection
standardizeData<-function(data.raw=data.raw, #raw data as a csv
                          col.names=col.names, #col names for final format
                          col.names.raw = col.names.raw, # col names in data.raw, that correspond to: 
                          #AnimalID, Timestamp,  X.coord, Y.coord, and DOP in that order. 
                          regionID = regionID, 
                          popID = popID, #the final popnames that you want, 
                          #in the same order as unique(data.raw$Population) would produce, after sorting data.raw by AnimalID and timestamp
                          #if any of the column names are missing (i.e. there is no DOP column), just put NA
                          SpeciesID = SpeciesID, 
                          TimeLocal=T, 
                          LatLongProj=T, 
                          NativeProj=CRS("+proj=longlat +ellps=WGS84 +no_defs"), 
                          NativeTimeZone="", 
                          wd.csv=wd.csv #the working directory where csv should be saved
){
  
  
  
  #cases were certain columns are missing 
  
  #if DOP is missing 
  if(is.na(col.names.raw[5])){
    data.raw$DOP<-NA
    col.names.raw[5]<-"DOP"
  }
  
  #case where col.names.raw don't match data.raw column names 
  if(!all(col.names.raw %in% names(data.raw), na.rm=T)){
    stop(paste0("ERROR: col.name ", "'", col.names.raw[which(!(col.names.raw %in% names(data.raw)) ) ], "'", " is not in data.raw" ))
  }
  
  #convert x and y coords to Lat Long 
  if(LatLongProj==F){
    data.raw.sp<-data.raw
    coordinates(data.raw.sp)<-col.names.raw[3:4]
    proj4string(data.raw.sp)<-NativeProj
    data.raw.ll<-spTransform(data.raw.sp, CRS("+proj=longlat +ellps=WGS84 +no_defs"))
    data.raw$Lat_WGS84<-coordinates(data.raw.ll)[, 2]
    data.raw$Long_WGS84<-coordinates(data.raw.ll)[, 1]
    col.names.raw[3:4]<-c("Lat_WGS84",  "Long_WGS84")
  }  
  
  #add AY-ID column
  #check that timestamp is in POSIX format
  if(class(data.raw[, c(col.names.raw[2])])[1]!="POSIXct"){
    stop("ERROR: Convert your timestamp to POSIXct")
  }
  
  #convert to local time 
  #NOTE - I will build this as needed 
  if(TimeLocal==F & NativeTimeZone!="UTC"){
    data.raw[, c(col.names.raw[2])]<-format(data.raw[, c(col.names.raw[2])], 
                                            tz=NativeTimeZone, usetz=T)
  }
  
  data.raw$AY_ID <-paste(data.raw[, c(col.names.raw[1])], year(data.raw[, c(col.names.raw[2])]), sep="_") 
  
  #add pop column 
  #NOTE - might want to add functionality to add multiple population columns and check for underscores, 
  #but for right now, I'll just leave as is
  data.raw$Population<-popID
  
  #add global ID
  data.raw$GlobalID<-paste(SpeciesID, regionID, popID, data.raw$AY_ID, sep="_")
  
  #re-arrange and rename dataframe
  data.raw<-data.raw[order(data.raw[, c(col.names.raw[1])], data.raw[, c(col.names.raw[2])]), ]
  data.final<-data.raw[, c(
    which(names(data.raw)==col.names.raw[1]), 
    which(names(data.raw)==col.names.raw[2]),
    which(names(data.raw)==col.names.raw[3]),
    which(names(data.raw)==col.names.raw[4]),
    which(names(data.raw)==col.names.raw[5]),
    which(names(data.raw)=="Population"),
    which(names(data.raw)=="AY_ID"),
    which(names(data.raw)=="GlobalID"))]
  names(data.final)<-col.names
  
  #save standardized formats
  setwd(wd.csv)
  write.csv(data.final, paste0(SpeciesID, "_", regionID, "_", popID, ".csv"))
  
  return(data.final)
}

#this function summarizes and cleans the data and saves the data as a .RData file
#input to this funtion must match the format of data.final output from the function standardizeData
summarizeData<-function(data.df=data.df, 
                        wd.rdata=wd.rdata, #the working directory where RData should be saved (as a shapefile)
                        wd.csv.sub= wd.csv.sub, # the working directory where the subsetted CSV data should be saved
                        regionID = regionID, 
                        popID = popID, 
                        SpeciesID = SpeciesID, 
                        VisualizeResults=F){
  par.def<-par()
  par(mfrow=c(5, 2))
  ay.id<-unique(data.df$AY_ID)
  ay.sub<-c()
  total.count<-0
  sum.animal.years<-0
  for(i in 1:length(ay.id)){
    total.count<-total.count+1
    d.i.y<-data.df[data.df$AY_ID==ay.id[i], ]
    
    d.i.y$Timestamp.simp<-ymd(paste(year(d.i.y$Timestamp), month(d.i.y$Timestamp), day(d.i.y$Timestamp), sep="_"))
    
    min.day<-ymd(paste(year(d.i.y$Timestamp[1]), "04", "15",  sep="_"))
    max.day<-ymd(paste(year(d.i.y$Timestamp[1]), "10", "31",  sep="_"))
    #number of days between min and max cutt-off date is 200 
    
    d.i.y<-d.i.y[d.i.y$Timestamp.simp>=min.day & d.i.y$Timestamp.simp<=max.day, ]
    if(nrow(d.i.y)==0){
      #print(paste(ay.id[i],  "did not meet minimum requirements"))
      next
    }
    #dates.df<-data.frame(Timestamp.simp=seq(min.day, max.day, by="day"))
    #merged.df<-merge(d.i.y, dates.df, by="Timestamp.simp")
    n.ay<-sum(aggregate(AnimalID~Timestamp.simp, data=d.i.y, FUN=function(x){length(x)>0})$AnimalID)
    if(n.ay>=190){
      sum.animal.years<-sum.animal.years+1
      # print(paste(ay.id[i],  "did meet minimum requirements"))
      # print(paste("current count =", sum.animal.years, "; total count =", total.count))
      ay.sub<-c(ay.sub, ay.id[i])
    }else{
      #print(paste(ay.id[i],  "did not meet minimum requirements"))
    }
    if(total.count<50){
      if(VisualizeResults==T & nrow(d.i.y)>10){
        hist(d.i.y$Timestamp.simp, 
             breaks=seq(d.i.y$Timestamp.simp[1], d.i.y$Timestamp.simp[nrow(d.i.y)]+days(7), by="week"), 
             freq=T, xlab="Time", main=popID, xlim=c(min.day, max.day))
        plot(d.i.y$Timestamp[-1], as.numeric(diff(d.i.y$Timestamp), units="days"), 
             type="l", xlab="Time", ylab="Time diff in days")
      }
    }
    
    
  }
  
  if(length(ay.sub)!=0){
    #subset data to include only those that meet the minimum requiement
    setwd(wd.csv.sub)
    data.sub<-data.df[data.df$AY_ID %in% ay.sub, ]
    data.sub<-data.sub[!is.na(data.sub$Lat_WGS84), ]
    write.csv(data.sub, paste0(SpeciesID, "_", regionID, "_", popID, "_subset.csv"))
    
    #create and save spatial points dataframe of 
    
    setwd(wd.rdata)
    coordinates(data.sub)<-c("Long_WGS84", "Lat_WGS84")
    proj4string(data.sub)<-CRS("+proj=longlat +ellps=WGS84 +no_defs")
    save(data.sub, file=paste0(SpeciesID, "_", regionID, "_", popID, "_subset.RData"))
  }
  
  par(mfrow=c(par.def$mfrow))
  return(c(sum.animal.years, total.count))
}


#This function plots the relationship between 2 variables in a nice format and 
#provides summary stats
plot.nice.lm<-function(x, y, legLoc, ...){
  plot(x, y, type="n", ...)
  
  lm.s<-lm(y~x)
  xrange<-seq(min(x, na.rm=T)-1, max(x, na.rm=T)+1, length.out=1000)
  lines(xrange,  lwd=6, col="grey50", 
        lm.s$coefficients[1]+xrange*lm.s$coefficients[2])
  
  prd<-predict(lm.s, newdata=data.frame(x=xrange), interval=c("confidence"), 
               level=0.95, type="response") 
  lines(xrange, prd[,2], col="grey50", lty=3, lwd=3) 
  lines(xrange, prd[,3], col="grey50", lty=3, lwd=3) 
  
  points(x, y, ...)
  
  r2<-summary(lm.s)$r.squared
  
  p<-anova(lm.s)$'Pr(>F)'[1]
  labs <-vector("expression", 3)
  
  labs[2] <- substitute(expression(italic(r)^2 == MYVALUE), 
                        list(MYVALUE = format(r2,dig=2)))[2]
  labs[1] <- substitute(expression(italic(p) == MYOTHERVALUE), 
                        list(MYOTHERVALUE = format(p, digits = 2)))[2]
  
  labs[3]<- substitute(expression(slope == MYVALUE), 
                       list(MYVALUE = format(lm.s$coefficients[2], dig=2)))[2]
  
  legend(legLoc, legend=labs,
         cex=2, bty="n", inset=c(0, 0)) 
}


#this function calculates the IRG curve
IRG<-function(midS, scaleS, t, bandwidthNorm=F, minMax=T){
  numerator<-exp((t+midS)/scaleS)
  demoninator<-(2*scaleS*exp((t+midS)/scaleS))+
    (scaleS*exp((2*t)/scaleS))+     
    (scaleS*exp((2*midS)/scaleS))
  irg<-numerator/demoninator
  if(bandwidthNorm==T){
    irg<-irg/scaleS
  }
  if(minMax==T){
    irg<-(irg-min(irg, na.rm=T))/(max(irg, na.rm=T)-min(irg, na.rm=T))
  }
  return(irg)
}

#this function scales values to be between zero and one
minmaxStandardize<-function(val, min.val, max.val){
  val<-(val-min.val)/(max.val-min.val)
  return(val)
}


#this function calculates several variations on different greenscape metrics
getSvGreenscape<-function(spDF=spDF, #spatial points dataframe of GPS location data 
                          IRGraster=IRGraster,#raster of date of peak IRG
                          ScaleRaster=ScaleRaster, # spring scale raster
                          PlotVariogram=T, # T or F, depending on if variogram should be visualized or not 
                          plotColors=colPalIRG,  # color pallet for ploting movemement data
                          saveJPEG=F, 
                          dir = "C:/Users/eaikens/Box Sync/Multispecies GWH/Figures/IndividualVisualizations/JPEGs" 
){
  coords.deer<-coordinates(spDF)

  #calculate SV and associated metrics
  alldist<- dist(coords.deer, method = "euclidean")
  max.dist.lag<-max(alldist)
  
  irg.vals<-extract(x=IRGraster, spDF)
  mean.scale<-mean(extract(x=ScaleRaster, spDF), na.rm=T)
  
  #remove NAs because SV can't handle it...
  idx.na<-which(is.na(irg.vals))
  if(length(idx.na)!=0){
    irg.vals<-irg.vals[-idx.na]
    coords.deer<-coords.deer[-idx.na, ]
  }
  
  #variogram 
  #0.00225 is used because it is the resolution of the IRG pixels
  v <- variog(coords=coords.deer, data=irg.vals, uvec=seq(0, max.dist.lag, by=0.00225))
  idx.min.n<-which(v$n<50)
  if(length(idx.min.n)!=0){
    log.dist.lag<-log10(v$u[-idx.min.n])
    log.sv<-log10(v$v[-idx.min.n])
  }else{
    log.dist.lag<-log10(v$u)
    log.sv<-log10(v$v)
  }
  
  #log-log plot
  
  indx<-which(log.dist.lag==-Inf |log.dist.lag==Inf)
  indx<-union(indx, which(log.dist.lag==-Inf |log.dist.lag==Inf))
  if(length(indx)!=0){
    log.dist.lag<- log.dist.lag[-indx]
    log.sv<- log.sv[-indx]
  }
  cutoff<-round(length(log.sv)*(3/4))
  
  lm.varlog<-lm(log.sv~log.dist.lag)
  slope.ll<-lm.varlog$coefficients[2]
  rsquared<-summary(lm.varlog)$r.squared
  multiplyScore<-rsquared * minmaxStandardize(val=slope.ll, min.val=-0.7708, max.val=2.0331)
  zScore<-rsquared + minmaxStandardize(val=slope.ll, min.val=-0.7708, max.val=2.0331)
  
  lm.varlog_3_4<-lm(log.sv[1:cutoff]~log.dist.lag[1:cutoff])
  slope.ll_3_4<-lm.varlog_3_4$coefficients[2]
  #rsquared.3_4<-summary(lm.varlog_3_4)$r.squared
  
  #green-up duration and mid-point of green-up date
  quants<-quantile(irg.vals, c(0.025, 0.975))
  duration.quants<-diff(quants)
  midpoint<-round(median(irg.vals, na.rm=T))
  
  #green-up duration and mid-point of green-up date, only unique values
  quants.uni<-quantile(unique(irg.vals), c(0.025, 0.975))
  duration.quants.uni<-diff(quants.uni)
  midpoint.uni<-round(median(unique(irg.vals), na.rm=T))
  
  #visualize everything 
  if(PlotVariogram==T){
    pars<-par()
    par(mfrow=c(2, 2), mar=c(5, 4, 0.5, 0.5))
    plot(spDF, asp=1, pch=21, col="black",
         bg=colPalIRG[as.numeric(cut(irg.vals, breaks = 100))])
    
    hist(irg.vals)
    abline(v=quantile(irg.vals, c(0.025, 0.975)), col="red", lwd=2)
    abline(v=midpoint, col="blue", lwd=2)
    plot(v, type="l")
    abline(v=10^(log.dist.lag[cutoff]), col="red")
    if(length(log.sv)>2){
      plot.nice.lm(log.dist.lag, log.sv, legLoc = "bottom", 
                   ylab="log(semivariance)", xlab="log(distance lag)")
      abline(v=(log.dist.lag[cutoff]), col="red")
      
    }else{
      plot(1, 1)
    }
    
    par(par)
  }
  
  if(saveJPEG==T){
    orwd<-getwd()
    setwd(dir)
    jpeg(paste0(spDF$GlobalID[1], ".jpg"), height=8, width = 8, 
         res=100, quality=100, units="in")
    pars<-par()
    par(mfrow=c(2, 2), mar=c(5, 4, 0.5, 0.5))
    plot(spDF, asp=1, pch=21, col="black",
         bg=colPalIRG[as.numeric(cut(irg.vals, breaks = 100))])
    
    hist(irg.vals)
    abline(v=quantile(irg.vals, c(0.025, 0.975)), col="red", lwd=2)
    abline(v=midpoint, col="blue", lwd=2)
    plot(v, type="l")
    abline(v=10^(log.dist.lag[cutoff]), col="red")
    if(length(log.sv)>2){
      plot.nice.lm(log.dist.lag, log.sv, legLoc = "bottom", 
                   ylab="log(semivariance)", xlab="log(distance lag)")
      abline(v=(log.dist.lag[cutoff]), col="red")
      
    }else{
      plot(1, 1)
    }
    dev.off()
    par(par)
    setwd(orwd)
  }
  
  greenscape.vals<-c(slope.ll, zScore, multiplyScore, duration.quants, mean.scale, midpoint, 
                     quants[1], quants[2], slope.ll_3_4, 
                     duration.quants.uni, midpoint.uni, 
                     quants.uni[1], quants.uni[2])
  names(greenscape.vals)<-c("svSlope", "orderZscore", "orderMultiScore", 
                            "duration", "scale", "midpoint", 
                            "lower_quant", "upper_quant", "svSlope_3_4", 
                            "duration.unique", "midpoint.unique", 
                            "lower_quant.unique", "upper_quant.unique")
  return(greenscape.vals)
}

#this function calculates several variations on different greenscape metrics
getSvGreenscapeVals<-function(spDF=spDF, #spatial points dataframe of GPS location data 
                          irg.vals=irg.vals,#extracted values of date of peak IRG
                          scale.vals=scale.vals,#extracted values of spring scale raster 
                          PlotVariogram=T, # T or F, depending on if variogram should be visualized or not 
                          plotColors=colPalIRG,  # color pallet for ploting movemement data
                          saveJPEG=F, 
                          dir = "C:/Users/eaikens/Box Sync/Multispecies GWH/Figures/IndividualVisualizations/JPEGs" 
){
  coords.deer<-coordinates(spDF)
  
  #calculate SV and associated metrics
  alldist<- dist(coords.deer, method = "euclidean")
  max.dist.lag<-max(alldist)
  
  #irg.vals<-extract(x=IRGraster, spDF)
  mean.scale<-mean(unique(scale.vals), na.rm=T)
  
  #remove NAs because SV can't handle it...
  idx.na<-which(is.na(irg.vals))
  if(length(idx.na)!=0){
    irg.vals<-irg.vals[-idx.na]
    coords.deer<-coords.deer[-idx.na, ]
  }
  
  #variogram 
  #0.00225 is used because it is the resolution of the IRG pixels
  v <- variog(coords=coords.deer, data=irg.vals, uvec=seq(0, max.dist.lag, by=0.00225))
  n.lags<-length(v$u)
  idx.min.n<-which(v$n<50)
  if(length(idx.min.n)!=0){
    log.dist.lag<-log10(v$u[-idx.min.n])
    log.sv<-log10(v$v[-idx.min.n])
  }else{
    log.dist.lag<-log10(v$u)
    log.sv<-log10(v$v)
  }
  
  #log-log plot
  
  indx<-which(log.dist.lag==-Inf |log.dist.lag==Inf)
  indx<-union(indx, which(log.dist.lag==-Inf |log.dist.lag==Inf))
  sv<-v$v
  if(length(indx)!=0){
    log.dist.lag<- log.dist.lag[-indx]
    log.sv<- log.sv[-indx]
    sv<-sv[-indx]
  }
  cutoff<-round(length(log.sv)*(3/4))
  
  maxSV<-max(sv)
  maxSV3_4<-max(sv[1:cutoff])

  lm.varlog<-lm(log.sv~log.dist.lag)
  slope.ll<-lm.varlog$coefficients[2]
  rsquared<-summary(lm.varlog)$r.squared
  multiplyScore<-rsquared * minmaxStandardize(val=slope.ll, min.val=-0.7708, max.val=2.0331)
  zScore<-rsquared + minmaxStandardize(val=slope.ll, min.val=-0.7708, max.val=2.0331)
  
  lm.varlog_3_4<-lm(log.sv[1:cutoff]~log.dist.lag[1:cutoff])
  slope.ll_3_4<-lm.varlog_3_4$coefficients[2]
  #rsquared.3_4<-summary(lm.varlog_3_4)$r.squared
  
  #green-up duration and mid-point of green-up date
  quants<-quantile(irg.vals, c(0.025, 0.975))
  duration.quants<-diff(quants)
  midpoint<-round(median(irg.vals, na.rm=T))
  
  #green-up duration and mid-point of green-up date, only unique values
  quants.uni<-quantile(unique(irg.vals), c(0.025, 0.975))
  duration.quants.uni<-diff(quants.uni)
  midpoint.uni<-round(median(unique(irg.vals), na.rm=T))
  
  #visualize everything 
  if(PlotVariogram==T){
    pars<-par()
    par(mfrow=c(2, 2), mar=c(5, 4, 0.5, 0.5))
    plot(spDF, asp=1, pch=21, col="black",
         bg=plotColors[round(irg.vals)-30])
    
    hist(unique(irg.vals))
    abline(v=quantile(unique(irg.vals), c(0.025, 0.975)), col="red", lwd=2)
    abline(v=midpoint.uni, col="blue", lwd=2)
    plot(v, type="l")
    abline(h=maxSV, col="blue")
    abline(h=maxSV3_4, col="lightblue")
    abline(v=10^(log.dist.lag[cutoff]), col="red")
    if(length(log.sv)>2){
      plot.nice.lm(log.dist.lag, log.sv, legLoc = "bottom", 
                   ylab="log(semivariance)", xlab="log(distance lag)")
      abline(v=(log.dist.lag[cutoff]), col="red")
    }else{
      plot(1, 1)
    }
    
    par(par)
  }
  
  if(saveJPEG==T){
    orwd<-getwd()
    setwd(dir)
    jpeg(paste0(spDF$GlobalID[1], ".jpg"), height=8, width = 8, 
         res=100, quality=100, units="in")
    pars<-par()
    par(mfrow=c(2, 2), mar=c(5, 4, 0.5, 0.5))
    plot(spDF, asp=1, pch=21, col="black",
         bg=plotColors[round(irg.vals)-30])
    
    hist(irg.vals)
    abline(v=quantile(irg.vals, c(0.025, 0.975)), col="red", lwd=2)
    abline(v=midpoint, col="blue", lwd=2)
    plot(v, type="l")
    abline(v=10^(log.dist.lag[cutoff]), col="red")
    if(length(log.sv)>2){
      plot.nice.lm(log.dist.lag, log.sv, legLoc = "bottom", 
                   ylab="log(semivariance)", xlab="log(distance lag)")
      abline(v=(log.dist.lag[cutoff]), col="red")
      
    }else{
      plot(1, 1)
    }
    dev.off()
    par(par)
    setwd(orwd)
  }
  
  greenscape.vals<-c(slope.ll, zScore, multiplyScore, duration.quants, mean.scale, midpoint, 
                     quants[1], quants[2], slope.ll_3_4, 
                     duration.quants.uni, midpoint.uni, 
                     quants.uni[1], quants.uni[2], 
                     maxSV, maxSV3_4, n.lags)
  names(greenscape.vals)<-c("svSlope", "orderZscore", "orderMultiScore", 
                            "duration", "scale", "midpoint", 
                            "lower_quant", "upper_quant", "svSlope_3_4", 
                            "duration.unique", "midpoint.unique", 
                            "lower_quant.unique", "upper_quant.unique", 
                            "maxSV", "maxSV3_4", "n.SV.lag")
  return(greenscape.vals)
}

#this function calulates green-wave surfing
getSurfingMetrics<-function(spDF=spDF, #spatial points dataframe of GPS location data 
                            IRGraster=IRGraster,#raster of date of peak IRG
                            ScaleRaster=ScaleRaster,# spring scale raster
                            startJDay=startJDay, #Julian day to start calculating surfing metrics
                            endJDay=endJDay #Julian day to end calculating surfing metrics
){
  irg.vals<-extract(x=IRGraster, spDF)
  scale.vals<-extract(x=ScaleRaster, spDF)
  yday<-yday(spDF$Timestamp)
  yday.full<-yday
  idx.sub<-which(yday>=startJDay & yday<=endJDay)
  yday<-yday[idx.sub]
  irg.vals<-irg.vals[idx.sub]
  scale.vals<-scale.vals[idx.sub]
  exper.IRG<-c()
  for(r in 1:length(irg.vals)){
    irgVec<-IRG(irg.vals[r], scale.vals[r], 1:365)
    irgExp<-irgVec[yday[r]]
    exper.IRG<-c(exper.IRG, irgExp)
  }
  dayAveIRG<-tapply(exper.IRG, INDEX=yday, FUN=mean, na.rm=T)
  DFP<-abs(irg.vals-yday)
  
  dayAveDFP<-tapply(DFP, INDEX=yday, FUN=mean, na.rm=T)
  seasonAveDFP<-mean(dayAveDFP, na.rm=T)
  seasonAveIRG<-mean(dayAveIRG, na.rm=T)
  
  #when full date range is available 
  if(min(yday.full)<=startJDay & max(yday.full)>=endJDay){
    full.range<-T
    missing.days<-0
  }else{
    full.range<-F
    missing.days<-(endJDay-startJDay+1)-length(dayAveDFP)
  }
  
  surfing.list<-c(seasonAveIRG, seasonAveDFP, full.range, missing.days)
  names(surfing.list)<-c("IRG", "DFP", "fullRange", "missingDays")
  return(surfing.list)
}

getSurfingMetricsVals<- function(spDF=spDF, #spatial points dataframe of GPS location data 
                                 irg.vals=irg.vals,#extracted values of date of peak IRG
                                 scale.vals=scale.vals,# extracted values of the spring scale raster
                                 startJDay=startJDay, #Julian day to start calculating surfing metrics
                                 endJDay=endJDay, 
                                 migStartjDay=migStartjDay
){
  if(startJDay<1){
    startJDay<-1
  }
  if(endJDay>365){
    endJDay<-365
  }
  
  # irg.vals<-extract(x=IRGraster, spDF)
  # scale.vals<-extract(x=ScaleRaster, spDF)
  yday<-yday(spDF$Timestamp)
  yday.full<-yday
  idx.sub<-which(yday>=startJDay & yday<=endJDay)
  yday<-yday[idx.sub]
  irg.vals<-irg.vals[idx.sub]
  scale.vals<-scale.vals[idx.sub]
  exper.IRG<-c()
  for(r in 1:length(irg.vals)){
    irgVec<-IRG(irg.vals[r], scale.vals[r], 1:365)
    irgExp<-irgVec[yday[r]]
    exper.IRG<-c(exper.IRG, irgExp)
  }
  
  ave.IRG<-c()
  for(r in 1:length(irg.vals)){
    irgVec<-IRG(irg.vals[r], scale.vals[r], 1:365)
    irgAve<-mean(irgVec[startJDay:endJDay])
    ave.IRG<-c(ave.IRG, irgAve)
  }
  #reference for IRG
  IRGref<-mean(tapply(ave.IRG, INDEX=yday, FUN=mean, na.rm=T), na.rm=T)
  
  #IRG
  dayAveIRG<-tapply(exper.IRG, INDEX=yday, FUN=mean, na.rm=T)
  seasonAveIRG<-mean(dayAveIRG, na.rm=T)
  cumIRG<-sum(dayAveIRG, na.rm=T)
  
  #days-from-peak
  DFP<-abs(irg.vals-yday)
  dayAveDFP<-tapply(DFP, INDEX=yday, FUN=mean, na.rm=T)
  seasonAveDFP<-mean(dayAveDFP, na.rm=T)
  
  #reference for days-from-peak
  #I'm not sure if this makes any sense....
  # DFPref<-abs(irg.vals-mean(startJDay:endJDay))
  # DFRrefSeason<-mean(tapply(DFPref, INDEX=yday, FUN=mean, na.rm=T))
  # 
  numDays<-endJDay-startJDay+1
  missingDays<-numDays-length(dayAveIRG)
 
  
  surfing.list<-c(seasonAveIRG, seasonAveDFP, cumIRG)
  names(surfing.list)<-c("IRG", "DFP", "cumulativeIRG")
  return(surfing.list)
}


getSurfingMetricsValsHoldBack<- function(spDF=spDF, #spatial points dataframe of GPS location data 
                                  irg.vals=irg.vals,#extracted values of date of peak IRG
                                  scale.vals=scale.vals,# extracted values of the spring scale raster
                                  startJDay=startJDay, #Julian day to start calculating surfing metrics
                                  endJDay=endJDay, 
                                  migStartjDay=migStartjDay
){
  if(startJDay<1){
    startJDay<-1
  }
  if(endJDay>365){
    endJDay<-365
  }
  
  # irg.vals<-extract(x=IRGraster, spDF)
  # scale.vals<-extract(x=ScaleRaster, spDF)
  yday<-yday(spDF$Timestamp)
  yday.full<-yday
  idx.sub<-which(yday>=startJDay & yday<=endJDay)
  yday<-yday[idx.sub]
  irg.vals<-irg.vals[idx.sub]
  scale.vals<-scale.vals[idx.sub]
  exper.IRG<-c()
  for(r in 1:length(irg.vals)){
    irgVec<-IRG(irg.vals[r], scale.vals[r], 1:365)
    irgExp<-irgVec[yday[r]]
    exper.IRG<-c(exper.IRG, irgExp)
  }
  
  ave.IRG<-c()
  for(r in 1:length(irg.vals)){
    irgVec<-IRG(irg.vals[r], scale.vals[r], 1:365)
    irgAve<-mean(irgVec[startJDay:endJDay])
    ave.IRG<-c(ave.IRG, irgAve)
  }
  #reference for IRG
  IRGref<-mean(tapply(ave.IRG, INDEX=yday, FUN=mean, na.rm=T), na.rm=T)
  
  #IRG
  dayAveIRG<-tapply(exper.IRG, INDEX=yday, FUN=mean, na.rm=T)
  seasonAveIRG<-mean(dayAveIRG, na.rm=T)
  cumIRG<-sum(dayAveIRG, na.rm=T)
  
  #days-from-peak
  DFP<-abs(irg.vals-yday)
  dayAveDFP<-tapply(DFP, INDEX=yday, FUN=mean, na.rm=T)
  seasonAveDFP<-mean(dayAveDFP, na.rm=T)
  
  #reference for days-from-peak
  #I'm not sure if this makes any sense....
  # DFPref<-abs(irg.vals-mean(startJDay:endJDay))
  # DFRrefSeason<-mean(tapply(DFPref, INDEX=yday, FUN=mean, na.rm=T))
  # 
  numDays<-endJDay-startJDay+1
  missingDays<-numDays-length(dayAveIRG)
  
  #look at hold back surfing
  if(!is.na(migStartjDay) & (migStartjDay>startJDay & migStartjDay<endJDay)){
    #null IRG
    idx.sub<-which(yday>=startJDay & yday<=migStartjDay)
    irg.vals.wr<-irg.vals[idx.sub]
    scale.vals.wr<-scale.vals[idx.sub]
    ave.IRG.wr<-c()
    for(r in 1:length(irg.vals.wr)){
      irgVec<-IRG(irg.vals.wr[r], scale.vals.wr[r], 1:365)
      irgAve<-mean(irgVec[(migStartjDay+1):endJDay])
      ave.IRG.wr<-c(ave.IRG.wr, irgAve)
    }
    #reference for IRG
    IRGref.wr<-mean(tapply(ave.IRG.wr, INDEX=yday[idx.sub], FUN=mean, na.rm=T), na.rm=T)
    holdback.null<-mean(c(dayAveIRG[names(dayAveIRG)%in%startJDay:migStartjDay], 
                        rep(IRGref.wr, length(migStartjDay:endJDay)-1)))
    
    #mean parameters
    aveDatePeak.wr<-mean(tapply(irg.vals.wr, INDEX=yday[idx.sub], FUN=mean, na.rm=T))
    aveScale.wr<-mean(tapply(scale.vals.wr, INDEX=yday[idx.sub], FUN=mean, na.rm=T))
    irgVec.wr<-IRG(aveDatePeak.wr, aveScale.wr, (migStartjDay+1):endJDay)
    holdback.mean<-mean(c(dayAveIRG[names(dayAveIRG)%in%startJDay:migStartjDay], 
                          irgVec.wr))
  }else{
    holdback.null<-NA
    holdback.mean<-NA
  }
  
  surfing.list<-c(seasonAveIRG, seasonAveDFP, cumIRG, missingDays, 
                  IRGref, seasonAveIRG/IRGref,  holdback.null,  holdback.mean)
  names(surfing.list)<-c("IRG", "DFP", "cumulativeIRG", "missingDays", 
                         "IRGref", "relativeIRG", "holdback_null", "holdback_mean")
  return(surfing.list)
}

getSvGreenscapeCirle<-function(circles=circles, #circle polygons in the same prjection as the IRG data #m.cirlces.df
                               irgRaster=irgRaster,#extracted values of date of peak IRG
                               scaleRaster=scaleRaster,#extracted values of spring scale raster 
                               sdRaster=sdRaster, 
                               sdCutoff=sdCutoff,
                               PlotVariogram=T,# T or F, depending on if variogram should be visualized or not 
                               plotColors=colPalIRG, 
                               saveJPEG=F, 
                               dir=dir
){
  
  #get input values for variogram
  #IRG
  irg.circle<-crop(irgRaster, circles)
  irg.circle.plot<-irg.circle
  irg.circle<-mask(irg.circle, circles)
  irg.circle<-aggregate(irg.circle, fact=2)
  irg.vals<-values(irg.circle)
  irg.coords<-coordinates(irg.circle)
  
  #springScale
  ss.circle<-crop(scaleRaster, circles)
  ss.circle<-mask(ss.circle, circles)
  scale.vals<-values(ss.circle)
  
  #springScale SD
  sd.circle<-crop(sdRaster, circles)
  sd.circle<-mask(sd.circle, circles)
  sd.vals<-values(sd.circle)
  
  index<-which(is.na(irg.vals))
  if(length(index)>0){
    irg.vals<-irg.vals[-index]
    irg.coords<-irg.coords[-index, ]
  }
  
  #filter out really poor fitting IRG curves
  idx<-which(sd.vals>sdCutoff)
  if(length(idx)>0){
    irg.vals<-irg.vals[-idx]
    irg.coords<-irg.coords[-idx, ]
    scale.vals<-scale.vals[-idx]
  }
  
  #green-up duration 
  quants<-quantile(irg.vals, c(0.025, 0.975))
  duration.quants<-diff(quants)
  midpoint<-round(median(irg.vals, na.rm=T))    
  
  #Green-up rate 
  springScale<-mean(scale.vals, na.rm=T)
  
  #Variogram
  #Set up distance lag for variogram
  pixel.res<-res(irg.circle)
  bb<-bbox(circles)
  xdist<-bb[1, 2]-bb[1, 1]
  ydist<-bb[2, 2]-bb[2, 1]
  dist.lag<-seq(0, max(xdist, ydist), by=pixel.res[1])
  
  v <- variog(coords=irg.coords, data=irg.vals, uvec=dist.lag)
  cutoff<-round(length(v$v)*(3/4))
  
  #variogram 
  #n.lags<-length(v$u[1:cutoff])
  # idx.min.n<-which(v$n<50)
  # if(length(idx.min.n)!=0){
  #   log.dist.lag<-log10(v$u[-idx.min.n])
  #   log.sv<-log10(v$v[-idx.min.n])
  # }else{
  #   
  # }
  
  log.dist.lag<-log10(v$u[1:cutoff])
  log.sv<-log10(v$v[1:cutoff])
  #log-log plot
  indx<-which(log.dist.lag==-Inf |log.dist.lag==Inf)
  indx<-union(indx, which(log.dist.lag==-Inf |log.dist.lag==Inf))
  sv<-v$v[1:cutoff]
  if(length(indx)!=0){
    log.dist.lag<- log.dist.lag[-indx]
    log.sv<- log.sv[-indx]
    sv<-sv[-indx]
  }
  maxSV<-max(sv, na.rm=T)
  id.max<-which(sv==maxSV)
  maxSVLag<-v$u[id.max]
  
  lm.varlog<-lm(log.sv~log.dist.lag)
  slope.ll<-lm.varlog$coefficients[2]
  rsquared<-summary(lm.varlog)$r.squared
  
  
  #visualize everything 
  if(PlotVariogram==T){
    pars<-par()
    par(mfrow=c(2, 2), mar=c(5, 4, 0.5, 0.5))
    plot(irg.circle, legend="F",
         col=plotColors, breaks=seq(1, 365, length.out=length(plotColors)))
    
    hist(irg.vals)
    abline(v=quantile(irg.vals, c(0.025, 0.975)), col="red", lwd=2)
    abline(v=midpoint, col="blue", lwd=2)
    plot(v, type="l")
    abline(h=maxSV, col="blue")
    abline(v=maxSVLag, col="red")
    if(length(log.sv)>2){
      plot.nice.lm(log.dist.lag, log.sv, legLoc = "bottom", 
                   ylab="log(semivariance)", xlab="log(distance lag)")
    }else{
      plot(1, 1)
    }
    
    par(par)
  }
  
  if(saveJPEG==T){
    orwd<-getwd()
    setwd(dir)
    jpeg(paste(as.character(circles@data$ID),".jpg", sep=""), height=8, width = 14, 
         res=100, quality=100, units="in")
    pars<-par()
    par(mfrow=c(2, 2), mar=c(5, 4, 0.5, 0.5))
    plot(irg.circle, 
         col=plotColors, breaks=seq(1, 365, length.out=length(plotColors)))
    
    hist(irg.vals)
    abline(v=quantile(irg.vals, c(0.025, 0.975)), col="red", lwd=2)
    abline(v=midpoint, col="blue", lwd=2)
    plot(v, type="l")
    abline(h=maxSV, col="blue")
    abline(v=maxSVLag, col="red")
    if(length(log.sv)>2){
      plot.nice.lm(log.dist.lag, log.sv, legLoc = "bottom", 
                   ylab="log(semivariance)", xlab="log(distance lag)")
    }else{
      plot(1, 1)
    }
    
    dev.off()
    par(par)
    setwd(orwd)
  }
  summaryVec<-c(maxSV, maxSVLag, slope.ll, 
                duration.quants, springScale)
  
  names(summaryVec)<-c("maxSV", "maxSVLag", "slopeSV", 
                       "greenUpDur", "springScale")
  
  return(summaryVec)
} #close function


getINDVICirle<-function(circles=circles, #circle polygons in the same prjection as the IRG data #m.cirlces.df
                        indviRaster=indviRaster,#extracted values of date of peak IRG
                        scaleRaster=scaleRaster, 
                        sdRaster=sdRaster, 
                        sdCutoff=sdCutoff
){
  pvals<-par()
  par(mfrow=c(2, 2))
  #INDVI
  indvi.circle<-crop(indviRaster, circles)
  indvi.circle<-mask(indvi.circle, circles)
  plot(indvi.circle, main="iNDVI")
  indvi.vals<-values(indvi.circle)
  
  #springScale
  ss.circle<-crop(scaleRaster, circles)
  ss.circle<-mask(ss.circle, circles)
  plot(ss.circle, main="Spring scale")
  scale.vals<-values(ss.circle)
  
  #springScale SD
  sd.circle<-crop(sdRaster, circles)
  sd.circle<-mask(sd.circle, circles)
  sd.vals<-values(sd.circle)
  
  #get rid of masked values 
  index<-which(is.na(indvi.vals))
  if(length(index)>0){
    indvi.vals<-indvi.vals[-index]
    sd.vals<-sd.vals[-index]
    scale.vals<-scale.vals[-index]
  }
  
  #filter out really poor fitting IRG curves
  idx<-which(sd.vals>sdCutoff)
  if(length(idx)>0){
    indvi.vals<-indvi.vals[-idx]
    scale.vals<-scale.vals[-idx]
  }
  hist(indvi.vals, breaks=seq(0, 365, by=5), main="iNDVI")
  hist(scale.vals, breaks=1:30, main="Spring scale")
  
  #Green-up rate 
  springScale<-mean(scale.vals, na.rm=T)
  springScale.cov<-sd(scale.vals, na.rm=T)/springScale
  
  #iNDVI
  meanINDVI<-mean(indvi.vals, na.rm=T)
  INDVI.cov<-sd(indvi.vals, na.rm=T)/meanINDVI
  

  summaryVec<-c(meanINDVI, INDVI.cov,  
                springScale, springScale.cov)
  
  names(summaryVec)<-c("mean_INDVI", "cov_INDVI",  
                       "mean_springScale", "cov_springScale")
  suppressWarnings(par(pvals))
  return(summaryVec)
} #close function



getSurfingMetricsValsCum<-function(spDF=spDF, #spatial points dataframe of GPS location data 
                                irg.vals=irg.vals,#extracted values of date of peak IRG
                                scale.vals=scale.vals,# extracted values of the spring scale raster
                                startJDay=startJDay, #Julian day to start calculating surfing metrics
                                endJDay=endJDay #Julian day to end calculating surfing metrics
){
  # irg.vals<-extract(x=IRGraster, spDF)
  # scale.vals<-extract(x=ScaleRaster, spDF)
  if(startJDay<1){
    startJDay<-1
  }
  if(endJDay>365){
    endJDay<-365
  }
  yday<-yday(spDF$Timestamp)
  idx.sub<-which(yday>=startJDay & yday<=endJDay)
  yday<-yday[idx.sub]
  irg.vals<-irg.vals[idx.sub]
  scale.vals<-scale.vals[idx.sub]
  exper.IRG<-c()
  for(r in 1:length(irg.vals)){
    irgVec<-IRG(irg.vals[r], scale.vals[r], 1:365)
    irgExp<-irgVec[yday[r]]
    exper.IRG<-c(exper.IRG, irgExp)
  }
  dayAveIRG<-tapply(exper.IRG, INDEX=yday, FUN=mean, na.rm=T)
  DFP<-abs(irg.vals-yday)
  dayAveDFP<-tapply(DFP, INDEX=yday, FUN=mean, na.rm=T)
  seasonAveDFP<-mean(dayAveDFP, na.rm=T)
  seasonAveIRG<-mean(dayAveIRG, na.rm=T)
  cumIRG<-sum(dayAveIRG, na.rm=T)
  
  numDays<-endJDay-startJDay+1
  missingDays<-numDays-length(dayAveIRG)
  ydaySeq<-startJDay:endJDay
  whichDays<-which(!(ydaySeq%in%names(dayAveIRG)))
  if(length(whichDays)>=3){
    difdif<-diff(diff(whichDays))
    if(0%in%difdif){
      sequentialGaps<-T
      cumIRGcorr<-cumIRG+(seasonAveIRG*missingDays)
      startGap<-ydaySeq[whichDays[1]]
      endGap<-ydaySeq[whichDays[length(whichDays)]]
    }
  }else{
    sequentialGaps<-F
    cumIRGcorr<-cumIRG+(seasonAveIRG*missingDays)
    startGap<-NA
    endGap<-NA
  }
  
  surfing.list<-c(seasonAveIRG, seasonAveDFP, cumIRG, cumIRGcorr, 
                  sequentialGaps, startGap, endGap, missingDays)
  names(surfing.list)<-c("IRG", "DFP", "cumulativeIRG","cumIRGcorrected", 
                         "sequentialGaps",  "startGap", "endGap", "missingDays")
  return(surfing.list)
}

#this function displays the relationship betwee green wave surfing and the greenscape 
plot.nice.multi<-function(IRG, DFP, greenUpDur, 
                          greenUpOrder, springScale, col,
                          ylabDFP, 
                          xlimdur=c(min(greenUpDur, na.rm=T), max(greenUpDur, na.rm=T)),
                          xlimscale=c(min(springScale, na.rm=T), max(springScale, na.rm=T)),
                          xlimOrder=c(min(greenUpOrder, na.rm=T), max(greenUpOrder, na.rm=T)),
                          ylimIRG=c(min(IRG, na.rm=T), max(IRG, na.rm=T)),
                          ylimDFP=c(min(DFP, na.rm=T), max(DFP, na.rm=T)), legLoc="topleft",
                          ...){
  default.par<-par()
  par(mar=c(5, 4.5, 2.5, 0.5))
  par(mfrow=c(3, 2))
  
  plot.nice.lm(greenUpOrder, 
               IRG, ylim=ylimIRG, xlim=xlimOrder,
               legLoc = legLoc, 
               xlab="slope of SV", 
               bg=col, pch=21,
               ylab="IRG", cex.lab=2, cex.axis=2)
  plot.nice.lm(greenUpOrder, 
               DFP, ylim=ylimDFP, xlim=xlimOrder,
               legLoc = legLoc, 
               xlab="slope of SV", 
               bg=col, pch=21,
               ylab=ylabDFP, cex.lab=2, cex.axis=2)
  
  plot.nice.lm(greenUpDur, 
               IRG, ylim=ylimIRG, xlim=xlimdur,
               legLoc = legLoc, 
               xlab="Green-up duration", 
               bg=col, pch=21,
               ylab="IRG", cex.lab=2, cex.axis=2)
  plot.nice.lm(greenUpDur, 
               DFP, ylim=ylimDFP, xlim=xlimdur,
               legLoc = legLoc, 
               xlab="Green-up duration", 
               bg=col, pch=21,
               ylab=ylabDFP, cex.lab=2, cex.axis=2)
  
  plot.nice.lm(springScale, 
               IRG, ylim=ylimIRG, xlim=xlimscale,
               legLoc = legLoc, 
               xlab="Spring Scale", 
               bg=col, pch=21,
               ylab="IRG", cex.lab=2, cex.axis=2)
  plot.nice.lm(springScale, 
               DFP, ylim=ylimDFP, xlim=xlimscale,
               legLoc = legLoc, 
               xlab="Spring Scale", 
               bg=col, pch=21,
               ylab=ylabDFP, cex.lab=2, cex.axis=2)
  #par(default.par)
}

getRegressionValMulti<-function(IRG, DFP, greenUpDur, 
                                greenUpOrder, springScale, iNDVI){
  surfing.labs<-c("IRG", "DFP")
  surfing.list<-list(IRG, DFP)
  greenscape.labs<-c("Duration", "Order", "Scale", "iNDVI")
  greenscape.list<-list(greenUpDur, greenUpOrder, springScale, iNDVI)
  regression.list<-list()
  counter<-0
  for(i in 1:2){
    y<-surfing.list[[i]]
    for(j in 1:4){
      counter<-counter+1
      x<-greenscape.list[[j]]
      lm.s<-summary(lm(y~x))
      reg.vals<-c(lm.s$coefficients[1, 1], 
                  lm.s$coefficients[1, 2],
                  lm.s$coefficients[2, 1], 
                  lm.s$coefficients[2, 2])
      names(reg.vals)<-c("Intercept", "SE_Intercept",
                         "Slope", "SE_Slope")
      regression.list[[counter]]<-list(paste(surfing.labs[i], greenscape.labs[j], sep="~"), 
                                       reg.vals)
      
    }
  }
  
  return(regression.list)
}

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
## put histograms on the diagonal
panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}

panel.diagonalLine <- function(x, y, col = par("col"), bg = NA, pch = par("pch"), 
                               cex = 1, col.diagLine = "red", ...) { 
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    abline(a=0, b=1, col=col.diagLine)
}
panel.lmLine <- function(x, y, col = par("col"), bg = NA, pch = par("pch"), 
                         cex = 1, col.diagLine = "red", ...) { 
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    g <- lm(y~x)
  abline(g, col=col.diagLine)
}
