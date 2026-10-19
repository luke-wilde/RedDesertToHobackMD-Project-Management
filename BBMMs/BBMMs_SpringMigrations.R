#Code for creating BBMMs (25% UD) of spring migrations and determining stopover use
#Functions written by Jerod Merkle and modified by Anna Ortega, Wyoming Cooperative Fish and Wildlife Research Unit
#May 20, 2021

#Load required libraries
library(rgdal)
library(foreign)
library(raster)
library(lubridate)
library(dplyr)
library(BBMM)
library(snowfall)
library(stringr)

#Import GPS collar data (clean data for spring analyses) ####
setwd("C:/Users/aortega5/Desktop/FinalDataChapter1/SpringMigrations")
load("AllLongDistance_SpringMigrations_2011_thru_2020_CleanForAnalyses.RData")

#Remove additional individual from dataset
final.gps.data<-final.gps.data[final.gps.data$id_yr!="30_2013",] #Only one long-distance migrant in 2013
final.gps.data<-final.gps.data[final.gps.data$id_yr!="127_2015",] #Only one long-distance migrant in 2015

final.gps.data<-final.gps.data[final.gps.data$id_yr!="303_2018",] #Returned to winter range on 3/17/2018
final.gps.data<-final.gps.data[final.gps.data$id_yr!="303_2019",] #Returned to winter range on 2/22/2019

final.gps.data<-final.gps.data[final.gps.data$id_yr!="225_2016",] #First captured on Table Mtn stopover without prior GPS data
final.gps.data<-final.gps.data[final.gps.data$id_yr!="257_2016",] #First captured on Table Mtn stopover without prior GPS data
final.gps.data<-final.gps.data[final.gps.data$id_yr!="258_2016",] #First captured on Table Mtn stopover without prior GPS data
final.gps.data<-final.gps.data[final.gps.data$id_yr!="259_2016",] #First captured on Table Mtn stopover without prior GPS data
final.gps.data<-final.gps.data[final.gps.data$id_yr!="317_2017",] #First captured on Table Mtn stopover without prior GPS data
final.gps.data<-final.gps.data[final.gps.data$id_yr!="319_2017",] #First captured on Table Mtn stopover without prior GPS data
final.gps.data<-final.gps.data[final.gps.data$id_yr!="320_2017",] #First captured on Table Mtn stopover without prior GPS data
final.gps.data<-final.gps.data[final.gps.data$id_yr!="321_2017",] #First captured on Table Mtn stopover without prior GPS data

final.gps.data<-final.gps.data[final.gps.data$id_yr!="332_2018",] #Recaptured during migration
final.gps.data<-final.gps.data[final.gps.data$id_yr!="254_2019",] #Recaptured during migration
final.gps.data<-final.gps.data[final.gps.data$id_yr!="279_2019",] #Recaptured during migration

#Rename dataframe
data<-as.data.frame(final.gps.data)
rm(final.gps.data)

#Remove any id_yr with < 200 GPS relocations
tbl <- table(data$id_yr) #6 individuals with less than 200 locations
sort(tbl)
ids2keep <- names(tbl)[tbl > 200]
data <- data[data$id_yr %in% ids2keep,] #Removed 6 animal-years
rm(tbl, ids2keep)
ids<-unique(data$id_yr) #Should have n = 152 animal-years
names(data)

#Simply dataframe
data<-data[c(1:2,5:7,9)]
data<-data[c("id","id_yr","year","date","x","y")]
colnames(data)<-c("id","id_yr","year","date","x","y")
head(data)

#Make database spatially aware
proj <- "+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs+towgs84=0,0,0"   #name a new proj
coordinates(data) <- c("x","y")
proj4string(data) <- CRS(proj)

#Create Brownian Bridge movement models ####
#Need to mark whether there are locations that are greater than 8 or so hours apart (BB analyses choke on this)
data <- data[order(data$id_yr, data$date),]  #order the database by id and date
data$date<-as.POSIXct(data$date, format="%Y-%m-%d %H:%M:%S")
dif <- c(diff(as.numeric(data$date)/7200),NA) #Change this to the fix rate that's appropiate
dif[c(diff(as.numeric(as.factor(paste(data$id_yr)))),0) != 0] <- 0 #replace the difs when it switches migration with a 0
hist(dif[dif < 25 & dif > -1])
hist(dif[dif < 10 & dif > -1])
table(dif > 8) #35 locations that are more than 8 hours apart

#Add a burst column for when there are breaks larger than 4 hours in the data (so i can miss 1 location with my 2-hr location data)
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
res(land) <- 150     #i'm using a 150m resolution here. Might want to increase this if things are going slowly. Or decrease it if you want more precision
projection(land) <- proj4string(data)

#Plot your grid
plot(extent(land))
plot(data, add=TRUE)

#Create a folder to write your BBs to
fldr <- "C:/Users/aortega5/Desktop/FinalDataChapter1/BBMMs"
if(dir.exists(fldr)==FALSE){   # if it doesn't exist, it'll create it for you
  dir.create(fldr)
}
# check if there any other files in it
length(dir(fldr))   #this should be 0. Otherwise you may want to remove files from this folder.

u <- unique(data$id_yr) #loop over ids

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

# were there any BBs that didn't work?
table(is.na(regBB$MotionVar))   #if any of these are TRUE, then YES

# now take a look at the motion variances for your animals
# Note that motion variances getting into the 8,000 and 10,000 are usually worth throwing out!
hist(regBB$MotionVar)
range(regBB$MotionVar)
outlier<-regBB[order(regBB$MotionVar),]
outlier<-subset(outlier,outlier$MotionVar>=8000)

# some code to look at your regular BBs
whichID <- u[20]    #choose an ID to look at
load(paste("D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/DissertationChapters/Chapter1_GWH_AmongMigratoryStrategies/Data/SpringMigrations/NewBBMMs/", "regBB_",whichID,".RData", sep=""))
rast <- land
rast[] <- bb$probability   #this is the probability
sum(values(rast))    #these will sum to 1, because it is a probability
rast2 <- getVolumeUD(as(rast, Class="DBBMM"))    #turn it into a volume, where it starts with the highest use areas (uses move package)
plot(rast2, xlim=extent(data[data$id_yr == whichID,])[1:2]+c(-1000,1000),
     ylim=extent(data[data$id_yr == whichID,])[3:4]+c(-1000,1000))
plot(data[data$id_yr == whichID,], add=T, pch=".")

#Import BBs for creating availability polygons ####
setwd("D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/DissertationChapters/Chapter1_GWH_AmongMigratoryStrategies/Data/SpringMigrations")
fls <- dir(paste(getwd(), "/NewBBMMs",sep=""), full.names = TRUE)  #this is the folder where your BBs are stored

#keep only dynamic Brownian bridge files
fls <- grep("regBB",fls, value=TRUE)
fls

#Get the names out of the file info
library(stringr)
names <- str_split_fixed(fls, "/", 9)[,9]
names <- sub("regBB_","",names)
names <- sub(".RData","",names)
names

#Ensure that all data is present
table(names %in% data$id_yr)   # This should ALL be TRUE!!!
names[names %in% data$id_yr == FALSE] #must be length=0

#Use 25% UD from regular BBMM
percentile <- 0.25

n<-as.data.frame(names)
head(n)
n$id<-str_split_fixed(n$names, "_", 2)[,1]

l<-unique(n$id)

#Create raster/grid to calculate BBs over ####
ext <- extent(data)
multiplyers <- c((ext[2]-ext[1])*0.2, (ext[4]-ext[3])*0.2)   # add about 20% around the edges of your extent (you can adjust this if necessary)
ext <- extend(ext, multiplyers)
rm(multiplyers)
land <- raster(ext)
res(land) <- 150     #i'm using a 150m resolution here. Might want to increase this if things are going slowly. Or decrease it if you want more precision
projection(land) <- proj4string(data)

#Plot your grid
plot(extent(land))
plot(data, add=TRUE)

#Loop over files, and create a list of the polygons created from BB objects
library(snowfall)
sfInit(parallel = T, cpus = 7)   #must change the cpus
sfExport("fls", "names","percentile","land")
sfLibrary(move)
HRs <- sfClusterApplyLB(1:length(fls), function(i){
  load(fls[i])   #load up each file
  rast <- land
  rast[]<-bb$probability
  rastR <- getVolumeUD(as(rast, Class="DBBMM"))    #turn it into a volume, where it starts with the highest use areas (uses move package)
  polyR <- reclassify(rastR, rcl=matrix(c(0,percentile,1,percentile,1,0),2,3, byrow=T))
  polyR[values(polyR)==0] <- NA
  polyR <- rasterToPolygons(polyR, dissolve=TRUE) #this is the spatialPolygonsDataFrame representing the contour
  return(polyR)
})

sfStop()
names(HRs) <- names
rm(fls, names, percentile)
plot(HRs[[80]])
names(HRs)
head(HRs)

HRRand <- rbind(1:length(HRs))
HRs.ids<-unique(names(HRs))

#Convert list of HRs to spatial dataframe
sp <- bind(HRs)
sp$id_yr<-HRs.ids
sp$year<-str_split_fixed(sp$id_yr, "_", 2)[,2]
head(sp)
tail(sp)

outname<- paste("RD2H_Corridor")

#Export the shapefile 
writeOGR(sp, "D:/WYO_COOP/RedDesertToHobackDataMuleDeerProject/DissertationChapters/Chapter1_GWH_AmongMigratoryStrategies/Data", outname, driver="ESRI Shapefile",overwrite=TRUE)
