require(geoR)

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



#this function calulates green-wave surfing
getSurfingMetrics<-function(spDF=spDF, #spatial points dataframe of GPS location data 
                            IRGraster=IRGraster,#raster of date of peak IRG
                            ScaleRaster=ScaleRaster,# spring scale raster
                            startJDay=startJDay, #Julian day to start calculation of surfing metrics
                            endJDay=endJDay #Julian day to end calculation of surfing metrics
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

getSvGreenscapeCirle<-function(circles=circles, #circle polygons in the same prjection as the IRG data #m.cirlces.df
                               aggregate.factor = 5,
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
  
  #---------------------#
  # change here -- Luke Wilde, 04.06.2026
  irg.circle <- aggregate(irg.circle, aggregate.factor)
  #---------------------#
  
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
  require(lubridate, quietly=T)
  yday <- lubridate::yday
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
  
  surfing.list<-c(seasonAveIRG, seasonAveDFP, cumIRG, seasonAveIRG/IRGref, missingDays)
  names(surfing.list)<-c("IRG", "DFP", "cumulativeIRG", "relativeIRG", "missingDays")
  return(surfing.list)
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
         cex=1.25, bty="n", inset=c(0, 0)) 
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

# spDF=gps.data.ay;
# irg.vals=irg.vals;
# scale.vals=scale.vals;
# PlotVariogram=T;
# saveJPEG=F;
# dir="C:/Users/lwilde2/Documents/AdaptiveFidelity/Outputs"

  coords.deer<-coordinates(spDF)
  
  #calculate SV and associated metrics
  alldist<- dist(coords.deer, method = "euclidean")
  max.dist.lag<-max(alldist)
  
  rm(alldist)
  
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
