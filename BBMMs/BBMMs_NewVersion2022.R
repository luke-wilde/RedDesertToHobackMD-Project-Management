#Code for conducting BBMMs
#And exporting shapefile of desired UD
#Majority of code written by Jerod Merkle and revised by Anna Ortega
#Wyoming Cooperative Fish and Wildlife Research Unit
#January 14, 2022

#Load required libraries
library(BBMM)
library(rgdal)
library(foreign)
library(raster)
library(lubridate)
library(dplyr)
library(snowfall)
library(stringr)

#Clean working environment
rm(list=ls())

#Import GPS collar data (clean migration data of each individual) ####
#use migration timing excel files for long and medium to delineate timing of migration. 
#short, use the summer range (end of spring to start of fall, or if they died)
#for 
setwd("D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/DissertationChapters/Chapter2_FrostWaveHypothesis/Data/AutumnMigrations")
load("SnowTempNDVIDistance_AutumnMigration_2011_thru_2020_13JAN2022.RData") # all autumn migrations, subset RDH_AllData by AID and SpringMS SpringME

#Rename dataframe
data<-final.gps.data
rm(final.gps.data)

#Reduce database to id_yr,x,y, and date
names(data)
data<-data[c(1:2,8:9)]
head(data)

#Make database spatially aware
proj <- "+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs+towgs84=0,0,0"   #name a new proj
coordinates(data) <- c("x","y")
proj4string(data) <- CRS(proj)

#-------------------------------------------#
# Create Brownian Bridge movement models ####
#-------------------------------------------#

# We need to mark whether there are locations that are greater than 8 or so hours apart (BB analyses choke on this)
data <- data[order(data$id_yr, data$date),]  #order the database by id and date
dif <- c(as.numeric(diff(as.numeric(data$date))),0)/3600  #change in time between points (in hrs)
dif[c(diff(as.numeric(as.factor(paste(data$id_yr)))),0) != 0] <- 0 #replace the difs when it switches migration with a 0
hist(dif)
table(dif > 8)

#add a burst column for when there are breaks larger than 4 hours in the data (so i can miss 1 location with my 2-hr location data)
MaxFixInterval <- 4.2     # identify your max interval as an object because we'll use it later
data$connect <- ifelse(dif > MaxFixInterval, "no", "yes")
table(data$connect)
rm(dif)

#Create raster/grid to calculate BBs over ####
ext <- extent(data)
multiplyers <- c((ext[2]-ext[1])*0.2, (ext[4]-ext[3])*0.2)   # add about 20% around the edges of your extent (you can adjust this if necessary)
ext <- extend(ext, multiplyers)
rm(multiplyers)
land <- raster(ext)
res(land) <- 250     #i'm using a 150m resolution here. Might want to increase this if things are going slowly. Or decrease it if you want more precision
projection(land) <- proj4string(data)

#Plot your grid
plot(extent(land))
plot(data, add=TRUE)

#------------------------#
# Condcut BBMM first ####
#------------------------#

#Create a folder to write your BBs to
fldr <- "D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/DissertationChapters/Chapter2_FrostWaveHypothesis/Data/BBMMs"

if(dir.exists(fldr)==FALSE){   # if it doesn't exist, it'll create it for you
  dir.create(fldr)
}
#Check if there any other files in it
length(dir(fldr))   #this should be 0. Otherwise you may want to remove files from this folder.

u <- unique(data$id_yr) #loop over ids
head(data)

#Run it on multiple processors, as it can take considerable amount of time!
sfInit(parallel = T, cpus = 7)   #must change the cpus
sfExport("data", "land", "u", "MaxFixInterval", "fldr")
sfLibrary(BBMM)
sfLibrary(raster)
regBB <- do.call(rbind, sfClusterApplyLB(1:length(u), function(i){
  temp <- data[data$id_yr==u[i],]
  # this is the function to calculate the regular BB
  bb <- try(brownian.bridge(x=coordinates(temp)[,1],
                            y=coordinates(temp)[,2],
                            time.lag=diff(as.numeric(temp$date)/60),
                            area.grid=coordinates(land),
                            max.lag=MaxFixInterval*60,
                            location.error=20), #this is the location error of your collars
            silent=TRUE)
  #write out results to file too, so if there is an error you don't loose all your work!
  if(class(bb)=="try-error"){
    return(data.frame(id_yr=u[i], MotionVar=NA))   #return an NA so you know there was an error
  }else{
    save(bb, file=paste(fldr, "/regBB_", u[i],".RData", sep=""))   #save the BB you made as a Rdata file so you can reload it later
    return(data.frame(id_yr=u[i], MotionVar=bb[[1]]))    # have it return the motion variance
  }
}))
sfStop()   #this stops the parralization loop

head(regBB)
 
#Were there any BBs that didn't work?
table(is.na(regBB$MotionVar))   #if any of these are TRUE, then YES

#Now take a look at the motion variances for your animals
#Note that motion variances getting into the 8,000 and 10,000 are usually worth throwing out!
#has to do with variability in the model, has bad predictions (check email with Gabe)
hist(regBB$MotionVar)

all.motion.variances<-as.data.frame(regBB)
head(all.motion.variances)
out<-subset(all.motion.variances,all.motion.variances$MotionVar>=8000)
range(out$MotionVar)

#Export id lists with high motion variances - this allows checking progress later
setwd("D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/DissertationChapters/Chapter2_FrostWaveHypothesis/Metadata")
write.csv(all.motion.variances,"MotionVariancesAllIndividuals.csv")
write.csv(out,"IndividualsWithHighMotionVariances.csv")

#Import BBs for creating availability polygons ####
setwd("D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/DissertationChapters/Chapter2_FrostWaveHypothesis/Data")
fls <- dir(paste(getwd(), "/BBMMs",sep=""), full.names = TRUE)  #this is the folder where your BBs are stored

#keep only dynamic Brownian bridge files
fls <- grep("regBB",fls, value=TRUE)
fls

#Get the names out of the file info, grab ID years - update this
library(stringr)
names <- str_split_fixed(fls, "/", 8)[,8]
names <- sub("regBB_","",names)
names <- sub(".RData","",names)
names

#Ensure that all data is present
table(names %in% data$id_yr)   # This should ALL be TRUE!!!
names[names %in% data$id_yr == FALSE] #must be length=0

#Loop over files, and create a list of the polygons created from BB objects
#big checkpoints - L 164-166 should equal 1, prob of full UD
library(snowfall)
sfInit(parallel = T, cpus = 7)   #must change the cpus
sfExport("fls", "names","land")
sfLibrary(move)
HRs <- sfClusterApplyLB(1:length(fls), function(i){
  load(fls[i])   #load up each file
  cutoff <- sort(bb$probability, decreasing=TRUE)
  vlscsum <- cumsum(cutoff)
  cutoff <- cutoff[vlscsum > .99][1] #Subset probabilities greater than 99%
  bb$probability[bb$probability < cutoff] <- 0
  
  #Rescale probabilities so they equal 1
  bb$probability <- bb$probability/sum(bb$probability)
  sum(bb$probability) #Should equal 1
  bb
  
  #Convert bb to raster
  rast <- land
  rast[]<-bb$probability   #this is the probability
  bb<-rast
  sum(values(bb))    #these will sum to 1, because it is a probability
  cutoff <- sort(values(bb), decreasing=TRUE)
  vlscsum <- cumsum(cutoff)
  cutoff <- cutoff[vlscsum > 0.99][1]
  bb <- reclassify(bb, rcl=matrix(c(-1,cutoff,0),2,3, byrow=T))
  bb <- bb/sum(values(bb))   #verify that they add up to 1
  stopovers <- bb
  stopover_percent = 10 #If interested in stopovers, then this should be 10
  #If interested in migration corridor, then this should be 99 - Cite Rodgers et al. 2021
  
  #identify the contour
  library(move)
  popUDVol <- getVolumeUD(as(stopovers, Class = "DBBMM"))
  qtl <- quantile(popUDVol[popUDVol != 1], probs = stopover_percent / 100)
  stopovers <- reclassify(popUDVol, rcl=matrix(c(0, qtl, 1, qtl, Inf, NA),2,3, byrow=T))
  
  #remove patches that are smaller than min_area
  #skip if conducting BBMM for migration corridors (99% UD)
  min_area = 23000 #units are m^2
  clmps <- clump(stopovers)
  clmpvals <- na.omit(values(clmps))
  clmpvals <- data.frame(table(clmpvals))
  clmpvals$Freq <- clmpvals$Freq*res(stopovers)[1]*res(stopovers)[2]   #this turns Freq into actuall m^2
  clmpvals <- as.numeric(as.character(clmpvals$clmpvals)[clmpvals$Freq < min_area])
  clmps[is.na(values(clmps))==TRUE] <- -5
  stopovers[values(clmps) %in% clmpvals == TRUE] <- NA
  
  #convert final raster to polygons
  stopovers <- rasterToPolygons(stopovers, dissolve=TRUE)
  stopovers$id_yr<-names[i]
  outname<- paste("RD2H",names[[i]],sep="_")
  outPath="D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/DissertationChapters/Chapter2_FrostWaveHypothesis/Data/Stopovers/IndividualStopovers"
  writeOGR(stopovers,outPath,outname, driver='ESRI Shapefile',overwrite=TRUE)
}) #inside this shp there are Layer=0 and Layer=1, Layer=0 are small and you can delete these. 




#this code produces stopovers for id-year
#this is the beginning of stop overs analyses
#in arc, multipart to single part - splits up all the individual stopovers - uses model builder to go through and split all the stop overs. This gives you the individual stop overs. Then you merge them all into one entier shp file, then you can remove the layers of 0. Then you could dissolve the overlapping 
#There is still an issue of aggregate, she changed the methods so that "for each polygon you need to calc the nearest neighbor, and then aggregate" she determined stopovers based on if they spent 3 days, 36 points. If it wasn't aggregating correctly 
