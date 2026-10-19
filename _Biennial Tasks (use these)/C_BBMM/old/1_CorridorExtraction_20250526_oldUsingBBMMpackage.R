#Code for conducting BBMMs
#And exporting shapefile of desired UD
#Majority of code written by Jerod Merkle and revised by Anna Ortega
#Wyoming Cooperative Fish and Wildlife Research Unit
#January 14, 2022

#------------------------#
# caution, may need to revise at some point to use move brownian.bridge.dyn function, you can make it more regular (instead of dynamic) by setting a large window (such as the nrow() of the data subset), the margin (set to 1 to reduce behavioral change point analysis), and time.step (set to 1 min. )

#Load required libraries
library(BBMM) #https://cran.r-project.org/src/contrib/Archive/BBMM/
#library(rgdal)
library(foreign)
library(raster)
library(lubridate)
library(dplyr)
library(snowfall)
library(stringr)
library(parallel)
library(sf)
library(tidyverse)
library(future)
library(furrr)


#Clean working environment
rm(list=ls())

filter <- dplyr::filter

#----------------------------------------------#
#Import GPS collar data (clean migration data of each individual) ####
#use migration timing excel files for long and medium to delineate timing of migration. 
#short, use the summer range (end of spring to start of fall, or if they died)
#for 
setwd("G:/Other computers/My Laptop/RDH Database/Processed Data/")
load("RDH_AllMigrations_2011t2024_05262025.RData") # all autumn migrations, subset RDH_AllData by AID and SpringMS SpringME


#view the fix rates:
mig.SpringMigration.sf %>% mutate(Year = as.numeric(Year)) %>% group_by(Year) %>% reframe(upp = quantile(diff, .75, na.rm = T))


minYr = 2011
maxYr = 2024
fixrate = 2.2




complete.Spring <- mig.SpringMigration.sf %>% mutate(Year = as.numeric(Year)) %>% group_by(Year, id_yr) %>% reframe(upp = quantile(diff, .85, na.rm = T)) %>% filter(case_when(Year < 2014 ~ upp <= (3600*3.2), 
                     Year == 2014 ~ upp <= (3600*1.2), 
                     Year > 2015 ~ upp <= (3600*2.2)))
complete.Spring <- bind_rows(complete.Spring, data.frame(id_yr = "255_2016"))


complete.Fall <- mig.FallMigration.sf %>% mutate(Year = as.numeric(Year)) %>% group_by(Year, id_yr) %>% reframe(upp = quantile(diff, .5, na.rm = T)) %>% filter(case_when(Year < 2014 ~ upp <= (3600*3.2), 
              Year == 2014 ~ upp <= (3600*1.2), 
              Year > 2014 ~ upp <= (3600*2.2)))


spring.mig <- mig.SpringMigration.sf %>% filter(id_yr %in% complete.Spring$id_yr) %>% filter(Year >= minYr & Year <= maxYr) #this will allow us to work with only 2023
fall.mig <- mig.FallMigration.sf %>% filter(id_yr %in% complete.Fall$id_yr) %>% filter(Year >= minYr & Year <= maxYr)




spring.mig[c("x","y")] <- st_coordinates(spring.mig)
proj <- st_crs(spring.mig)$input

fall.mig[c("x","y")] <- st_coordinates(fall.mig)




#----------------------------------------#
# Change to sp and pare down data ####

# FIRST SPRING
#Reduce database to id_yr,x,y, and POSIXct
names(spring.mig)
spring.mig.sub<-spring.mig[,c("id_yr","x","y", "POSIXct")]


#check which already exist
completed_BBMMSpring <- "C:/Users/lwilde2/Desktop/Remote Jobs/05.28.2025_BBMM/Out/Spring"

# Get the list of files in the directory
file_list <- list.files(completed_BBMMSpring)

# Extract the desired strings
extracted_strings <- str_extract(file_list, "(?<=regBB_).*(?=\\.RData)")

#Create a folder to write your BBs to
fldr <- "C:/Users/lwilde2/Desktop/Remote Jobs/05.28.2025_BBMM/Out/Spring"

if(dir.exists(fldr)==FALSE){   # if it doesn't exist, it'll create it for you
  dir.create(fldr)
}
#Check if there any other files in it
length(dir(fldr))   #this should be 0. Otherwise you may want to remove files from this folder.

u <- unique(spring.mig.sub$id_yr) #loop over ids
#u <- setdiff(unique(spring.mig.sub$id_yr) , extracted_strings) #if doing it for second time, do this

spring.mig.sub <- spring.mig.sub %>% filter(id_yr %in% u) %>% st_drop_geometry() %>% arrange(id_yr, POSIXct) %>% as.data.frame()

head(spring.mig.sub)

names(spring.mig.sub) <- c("id_yr","x","y","date")

#Make database spatially aware, in sp
coordinates(spring.mig.sub) <- c("x","y")
proj4string(spring.mig.sub) <- CRS(proj)

# spring.mig.sub <- spTransform(spring.mig.sub, CRS("+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs+towgs84=0,0,0" ))

spring.mig.sub$date <- as.POSIXct(spring.mig.sub$date, format = "%Y-%m-%d %H:%M:%S", tz = "MST")

head(spring.mig.sub)
str(spring.mig.sub)

#-------------------------------------------#
# Step 1 Create Brownian Bridge movement models ####
#-------------------------------------------#

#---------------------------------------#
#Step 1s. BBMM of Spring####

# We need to mark whether there are locations that are greater than 8 or so hours apart (BB analyses choke on this)
data <- spring.mig.sub[order(spring.mig.sub$id_yr, spring.mig.sub$date),]  #order the database by id and date
dif <- c(as.numeric(diff(as.numeric(data$date))),0)/3600  #change in time between points (in hrs)
dif[c(diff(as.numeric(as.factor(paste(data$id_yr)))),0) != 0] <- 0 #replace the difs when it switches migration with a 0
hist(dif)
table(dif > (fixrate*2))



#add a burst column for when there are breaks larger than 6 hours in the data (so i can miss 1 location with my 2-hr location data)
MaxFixInterval <- fixrate*2     # identify your max interval as an object because we'll use it later
connect <- ifelse(dif > MaxFixInterval, "no", "yes")
table(connect)





#Create raster/grid to calculate BBs over ####
ext <- extent(data)
multiplyers <- c((ext[2]-ext[1])*0.1, (ext[4]-ext[3])*0.1)   # add about 20% around the edges of your extent (you can adjust this if necessary)
ext <- extend(ext, multiplyers)
rm(multiplyers)
land <- raster(ext)
res(land) <- 100     #i'm using a 100m resolution here. Might want to increase this if things are going slowly. Or decrease it if you want more precision
projection(land) <- proj4string(data)

# #Plot your grid
# plot(extent(land))
# plot(data, add=TRUE)
data <- data[order(data$id_yr, data$date),]
head(data); tail(data)
# 
# n.core <- detectCores()-1
# #Run it on multiple processors, as it can take considerable amount of time!
# sfInit(parallel = T, cpus = n.core)   #must change the cpus
# sfExport("data", "land", "u", "fldr")
# sfLibrary(BBMM)
# sfLibrary(raster)
# regBB <- do.call(rbind, sfClusterApplyLB(1:length(u), function(i){
#   #i = 12
#   
#     temp <- data[data$id_yr==u[i],]
#   # this is the function to calculate the regular BB
#   bb <- try(brownian.bridge(x=coordinates(temp)[,1],
#                             y=coordinates(temp)[,2],
#                             time.lag=diff(as.numeric(temp$date)/60),
#                             area.grid=coordinates(land),
#                             max.lag=temp$MaxFixInterval[1]*60,
#                             location.error=20), #this is the location error of your collars
#             silent=TRUE)
#   #write out results to file too, so if there is an error you don't loose all your work!
#   if(class(bb)=="try-error"){
#     return(data.frame(id_yr=u[i], MotionVar=NA))   #return an NA so you know there was an error
#   }else{
#     save(bb, file=paste(fldr, "/regBB_", u[i],".RData", sep=""))   #save the BB you made as a Rdata file so you can reload it later
#     return(data.frame(id_yr=u[i], MotionVar=bb[[1]]))    # have it return the motion variance
#   }
# }))
# sfStop()   #this stops the parralization loop
# showConnections()


# Set up parallel processing with future
n.core <- detectCores() - 1
plan(multisession, workers = n.core)  # Use multiple workers

# Function to compute Brownian bridge
compute_bb <- function(i) {
  temp <- data[data$id_yr == u[i], ]
  
  bb <- try(
    brownian.bridge(
      x = coordinates(temp)[, 1],
      y = coordinates(temp)[, 2],
      time.lag = diff(as.numeric(temp$date) / 60),
      area.grid = coordinates(land),
      max.lag = MaxFixInterval * 60,
      location.error = 20
    ), 
    silent = TRUE
  )
  
  if (class(bb) == "try-error") {
    return(data.frame(id_yr = u[i], MotionVar = NA))
  } else {
    save(bb, file = paste0(fldr, "/regBB_", u[i], ".RData"))
    return(data.frame(id_yr = u[i], MotionVar = bb[[1]]))
  }
}

# Apply function in parallel
regBB <- future_map_dfr(1:length(u), compute_bb, .progress = TRUE)

# Stop future plan
plan(sequential)  # Reset to sequential mode

head(regBB)

#Were there any BBs that didn't work?
table(is.na(regBB$MotionVar))   #if any of these are TRUE, then YES

#Now take a look at the motion variances for your animals
#Note that motion variances getting into the 8,000 and 10,000 are usually worth throwing out!
#has to do with variability in the model, has bad predictions (check email with Gabe)
hist(regBB$MotionVar)

all.motion.variances<-as.data.frame(regBB)
head(all.motion.variances)
out<-subset(all.motion.variances,all.motion.variances$MotionVar>=10000)
range(out$MotionVar)

#Export id lists with high motion variances - this allows checking progress later
setwd("C:/Users/lwilde2/Desktop/Remote Jobs/05.28.2025_BBMM/Meta/Spring/")
write.csv(all.motion.variances,paste0("AllMotionVariances_20250528_",minYr,"to",maxYr,".csv"))
write.csv(out,paste0("IndividualsWithHighMotionVariances_20250528_",minYr,"to",maxYr,".csv"))

rm(regBB, data)



#-------------------------------------------#
#NOW FALL ####
#Reduce database to id_yr,x,y, and POSIXct
names(fall.mig)
fall.mig.sub<-fall.mig[,c("id_yr","x","y", "POSIXct")]


#check which already exist
completed_BBMMFall <- "C:/Users/lwilde2/Desktop/Remote Jobs/05.28.2025_BBMM/Out/Fall"

# Get the list of files in the directory
file_list <- list.files(completed_BBMMFall)

# Extract the desired strings
extracted_strings <- str_extract(file_list, "(?<=regBB_).*(?=\\.RData)")

#Create a folder to write your BBs to
fldr <- "C:/Users/lwilde2/Desktop/Remote Jobs/05.28.2025_BBMM/Out/Fall"

if(dir.exists(fldr)==FALSE){   # if it doesn't exist, it'll create it for you
  dir.create(fldr)
}
#Check if there any other files in it
length(dir(fldr))   #this should be 0. Otherwise you may want to remove files from this folder.

done <- read.csv("C:/Users/lwilde2/Desktop/Remote Jobs/05.28.2025_BBMM/Meta/Fall/MotionVariancesAllIndividuals_20250528_set2.csv") %>% select(id_yr)

u <- setdiff(unique(fall.mig.sub$id_yr) , done) #setdiff(unique(fall.mig.sub$id_yr) , extracted_strings) #loop over ids

fall.mig.sub <- fall.mig.sub %>% filter(id_yr %in% u) %>% st_drop_geometry() %>% arrange(id_yr, POSIXct) %>% as.data.frame()

head(fall.mig.sub)

names(fall.mig.sub) <- c("id_yr","x","y","date")

#Make database spatially aware, in sp
coordinates(fall.mig.sub) <- c("x","y")
proj4string(fall.mig.sub) <- CRS(proj)

# fall.mig.sub <- spTransform(fall.mig.sub, CRS("+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs+towgs84=0,0,0" ))

fall.mig.sub$date <- as.POSIXct(fall.mig.sub$date, format = "%Y-%m-%d %H:%M:%S", tz = "MST")

head(fall.mig.sub)
str(fall.mig.sub)


# #---------------------------------------#
# #Step 1f. BBMM of Fall ####


# We need to mark whether there are locations that are greater than 8 or so hours apart (BB analyses choke on this)
data <- fall.mig.sub[order(fall.mig.sub$id_yr, fall.mig.sub$date),]  #order the database by id and date
data$dif <- c(as.numeric(diff(as.numeric(data$date))),0)/3600  #change in time between points (in hrs)
data$dif[c(diff(as.numeric(as.factor(paste(data$id_yr)))),0) != 0] <- 0 #replace the difs when it switches migration with a 0
hist(data$dif)
table(data$dif > 4)

Sawyer <- data[which(year(data$date) < 2014),]
Hayes <- data[which(year(data$date) == 2014),]
Current <- data[which(year(data$date) > 2014),]

#Sawyer
#add a burst column for when there are breaks larger than 6 hours in the data (so i can miss 1 location with my 2-hr location data)
Sawyer$MaxFixInterval <- 6.2     # identify your max interval as an object because we'll use it later
Sawyer$connect <- ifelse(Sawyer$dif > Sawyer$MaxFixInterval, "no", "yes")
table(Sawyer$connect)


#Hayes
#add a burst column for when there are breaks larger than 2 hours in the data (so i can miss 1 location with my 2-hr location data)
Hayes$MaxFixInterval <- 2.2     # identify your max interval as an object because we'll use it later
Hayes$connect <- ifelse(Hayes$dif > Hayes$MaxFixInterval, "no", "yes")
table(Hayes$connect)


#Current
#add a burst column for when there are breaks larger than 4 hours in the data (so i can miss 1 location with my 2-hr location data)
Current$MaxFixInterval <- 4.2     # identify your max interval as an object because we'll use it later
Current$connect <- ifelse(Current$dif > Current$MaxFixInterval, "no", "yes")
table(Current$connect)


data <- rbind(Sawyer, Hayes, Current)
data <- data[, !(names(data) %in% "dif")]



#Create raster/grid to calculate BBs over ####
ext <- extent(data)
multiplyers <- c((ext[2]-ext[1])*0.1, (ext[4]-ext[3])*0.1)   # add about 20% around the edges of your extent (you can adjust this if necessary)
ext <- extend(ext, multiplyers)
rm(multiplyers)
land <- raster(ext)
res(land) <- 150     #i'm using a 100m resolution here. Might want to increase this if things are going slowly. Or decrease it if you want more precision
projection(land) <- proj4string(data)

# #Plot your grid
# plot(extent(land))
# plot(data, add=TRUE)
data <- data[order(data$id_yr, data$date),]
head(data); tail(data)

#Plot your grid
#plot(extent(land))
#plot(data, add=TRUE)

# 
# #Run it on multiple processors, as it can take considerable amount of time!
# sfInit(parallel = T, cpus = detectCores()-1)   #must change the cpus
# sfExport("data", "land", "u", "fldr")
# sfLibrary(BBMM)
# sfLibrary(raster)
# regBB <- do.call(rbind, sfClusterApplyLB(1:length(u), function(i){
#   temp <- data[data$id_yr==u[i],]
#   # this is the function to calculate the regular BB
#   bb <- try(brownian.bridge(x=coordinates(temp)[,1],
#                             y=coordinates(temp)[,2],
#                             time.lag=diff(as.numeric(temp$date)/60),
#                             area.grid=coordinates(land),
#                             max.lag=temp$MaxFixInterval[1]*60,
#                             location.error=20), #this is the location error of your collars
#             silent=TRUE)
#   #write out results to file too, so if there is an error you don't loose all your work!
#   if(class(bb)=="try-error"){
#     return(data.frame(id_yr=u[i], MotionVar=NA))   #return an NA so you know there was an error
#   }else{
#     save(bb, file=paste(fldr, "/regBB_", u[i],".RData", sep=""))   #save the BB you made as a Rdata file so you can reload it later
#     return(data.frame(id_yr=u[i], MotionVar=bb[[1]]))    # have it return the motion variance
#   }
# }))
# sfStop()   #this stops the parralization loop


# Set up parallel processing with future
n.core <- detectCores() - 1

# Set up parallel processing with future
plan(multisession, workers = n.core)  # Use multiple workers
options(future.globals.maxSize = 20 * 1024^2)  # Allow up to 20MB instead of default

# Apply function in parallel
regBB <- future_map_dfr(1:length(u), compute_bb, .progress = TRUE)

# Stop future plan
plan(sequential)  # Reset to sequential mode


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
setwd("C:/Users/lwilde2/Desktop/Remote Jobs/05.28.2025_BBMM/Meta/Fall")
write.csv(all.motion.variances,"MotionVariancesAllIndividuals_20250528_set3.csv")
write.csv(out,"IndividualsWithHighMotionVariances_20250528_set3.csv")

# 
# 


# progressed to here 05.28.2025


#--------------------------------------------#
#Step 2. Import BBs for creating availability polygons ####
setwd("C:/Users/lwilde2/Desktop/Remote Jobs/05.28.2025_BBMM/Out/Spring")
fls <- dir(paste(getwd(), "",sep=""), full.names = F)  #this is the folder where your BBs are stored

#keep only dynamic Brownian bridge files
fls <- grep("regBB",fls, value=TRUE)
fls

#Get the names out of the file info, grab ID years - update this
library(stringr)
names <- fls
names <- sub("regBB_","",names)
names <- sub(".RData","",names)
names
# 
# # do not estimate SOs for deer with too high of MotionVariance
# reject <- rbind(read.csv("C:/Users/lwilde2/Desktop/Remote Jobs/05.28.2025_BBMM/Meta/Spring/MotionVariancesAllIndividuals_20250528.csv") %>% filter(MotionVar > 20000) %>% select(id_yr),read.csv("C:/Users/lwilde2/Desktop/Remote Jobs/05.28.2025_BBMM/Meta/Spring/MotionVariancesAllIndividuals_20250528_set2.csv") %>% filter(MotionVar > 20000) %>% select(id_yr))
# 
# names <- setdiff(names, reject$id_yr) #setdiff(unique(fall.mig.sub$id_yr) , extracted_strings) #loop over ids


#Ensure that all data is present
table(names %in% unique(spring.mig.sub$id_yr))   # This should ALL be TRUE!!!
names[names %in% spring.mig.sub$id_yr == FALSE] #must be length=0

#-----------ATTN: use these only if there are id_yrs missing-----------------#
 #fls <- fls[-which(!names %in% data$id_yr)]
 #names <- names[-which(!names %in% data$id_yr)]

# or if some shp didnt work
setwd("C:/Users/lwilde2/Desktop/Remote Jobs/05.28.2025_BBMM/Stopovers/Spring")
fls1 <- dir(paste(getwd(), "",sep=""), full.names = F)  #this is the folder where your BBs are stored

#keep only dynamic Brownian bridge files
fls1 <- grep(".shp",fls1, value=TRUE)
fls1

#Get the names out of the file info, grab ID years - update this
library(stringr)
names1 <- fls1
names1 <- sub("RD2H_","",names1)
names1 <- sub(".shp","",names1)
names1

redo <- setdiff(names, names1)
redofiles <- fls[basename(fls) %in% paste0("regBB_",redo,".RData")]
setwd("C:/Users/lwilde2/Desktop/Remote Jobs/05.28.2025_BBMM/Out/Spring")

#Extract SOs and remove extra small ones ####
#------------------------------------------#

#Loop over files, and create a list of the polygons created from BB objects
#big checkpoints - L 164-166 should equal 1, prob of full UD
library(snowfall)
sfInit(parallel = T, cpus = detectCores()-1)   #must change the cpus
sfExport("fls", "names","land")
sfLibrary(move)
HRs <- sfClusterApplyLB(1:length(fls), function(i){
  
 # i=132
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
  #If interested in migration corridor, then this should be 99 - Cite Rodgers et al. 2021 Ecosphere
  
  #identify the contour
  library(move)
  library(sf)
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
  outPath="C:/Users/lwilde2/Desktop/Remote Jobs/05.28.2025_BBMM/Stopovers/Spring/"
  
  stopovers <- st_as_sf(stopovers)
  
  st_write(stopovers, dsn = paste0(outPath, outname,".shp"), append = T)
  
  #writeOGR(stopovers,outPath,outname, driver='ESRI Shapefile',overwrite=TRUE)
}) #inside this shp there are Layer=0 and Layer=1, Layer=0 are small and you can delete these. 







for(i in 1:length(redo)){
  #i=132
  
  i=2
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
#If interested in migration corridor, then this should be 99 - Cite Rodgers et al. 2021 Ecosphere

#identify the contour
library(move)
library(sf)
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
stopovers$id_yr<-names1[i]
outname<- paste("RD2H",names1[[i]],sep="_")
outPath="C:/Users/lwilde2/Desktop/Remote Jobs/05.28.2025_BBMM/Stopovers/Spring/"

stopovers <- st_as_sf(stopovers)

st_write(stopovers, dsn = paste0(outPath, outname,".shp"), append = T)

print("finished", names1[i])

}



#this code produces stopovers for id-year
#this is the beginning of stop overs analyses
#in arc, multipart to single part - splits up all the individual stopovers - uses model builder to go through and split all the stop overs. This gives you the individual stop overs. Then you merge them all into one entier shp file, then you can remove the layers of 0. Then you could dissolve the overlapping 
#There is still an issue of aggregate, she changed the methods so that "for each polygon you need to calc the nearest neighbor, and then aggregate" she determined stopovers based on if they spent 3 days, 36 points. If it wasn't aggregating correctly 


#----------------------------------------#
# Merge SOs into one file ####


path = "C:/Users/lwilde2/Desktop/RDH Database/Processed Data/BBMMS/Fall/Stopovers/IndividualStopovers/"
fls <- dir(paste(path,sep=""), full.names = T)  #this is the folder where your BBs are stored


#Get the names out of the file info, grab ID years 

file_names <- basename(fls)
shp_files <- file_names[grepl("\\.shp$", file_names)]

shp_files

lst <- list(length(shp_files), NA)

for(i in 1:length(shp_files)){
  tryCatch({
    lst[[i]] <- st_read(file.path(path, shp_files[i]), quiet = TRUE) %>% st_transform(., "EPSG:32612")
  }, error = function(e) {
    message(paste("Error reading", shp_files[i], ":", e$message))
    lst[[i]] <- NULL  # or NA if you prefer
  })
  
}

Corr_all <- do.call(rbind, lst[!sapply(lst, is.null)])

# x <- Corr_all[c(1:7),]
# 
# xx <- st_union(x)
# mapview(x, zcol = "id_yr")
# 
# mapview(xx)



#clean and prep
Corr_all <- Corr_all %>% filter(value == 1)
Corr_all <- Corr_all  %>% separate(id_yr, into = c("AID", "Year"), remove = F)


SO_all <- Corr_all


st_write(SO_all, dsn ="C:/Users/lwilde2/Desktop/RDH Database/Processed Data/BBMMS/Fall/Stopovers/RDH_FallStopovers_2011to2024_06232025.shp")
save(SO_all, file = "C:/Users/lwilde2/Desktop/RDH Database/Processed Data/BBMMS/Fall/Stopovers/RDH_FallStopovers_2011to2024_06232025.RData")


# failed <- c("RD2H_233_2016.shp", "RD2H_244_2015.shp", "RD2H_285_2025.shp", "RD2H_623_2025.shp", "RD2H_713_2025.shp", "RD2H_729_2025.shp" )
# 
# for(i in 1:length(failed)){
# st_read(file.path(path, failed[i]))
#   }

SO_all

#-------- NOT RUN ------------------#
# lst <- list(length(unique(Corr_all$AID)), NA)
# for(i in unique(Corr_all$AID)){
#  #i = 255
# x <- Corr_all %>% filter(AID == i) %>% select(AID)
# x
# xx <- st_as_sf(st_union(x))
# st_geometry(xx) <- "geometry"
# xx$AID <- i
# 
# xx <- st_transform(xx, crs = st_crs(spring.mig)$input)
# 
# lst[[i]] <- xx
# 
# }
# 
# 
# Corr_AID <- do.call(rbind, lst)
# 
# st_write(Corr_all, "C:/Users/lwilde2/Desktop/RDH Database/Processed Data/BBMMS/Spring/Stopovers/RDH_AllStopovers_2014to2023.shp")
# save(Corr_all,Corr_AID, Corr_Year, file = "C:/Users/lwilde2/Documents/AdaptiveFidelity/Data/Raw Data/BBMMS/Stopovers/RDH_AllStopovers_2014to2022.RData")

#---------------------------#
# clip raster and make points ####
# BischCalcs <- "G:/Shared drives/wyo-coop-wilde/NDVI_Calcs/Aikens_greenscape/dlc_params_2014to2022.RData"
# load(BischCalcs)
# 
# allh <- st_as_sf(st_convex_hull(all))
# st_geometry(allh) <- "geometry"
# 
# #transform to raster crs
# allhs <- st_transform(allh, crs = st_crs(xmids_SDAll[[1]])$input)
# 
# clip <- mask(xmids_SDAll[[1]], allhs)
# clippt <- rasterToPoints(clip)
# 
# clippt <- st_as_sf(as.data.frame(clippt), coords = c("x","y"), crs = st_crs(xmids_SDAll[[1]])$input)
# 
# coords <- as.data.frame(st_coordinates(clippt))
# head(coords)
# 
# coords$Y
# 
# index <- clippt[which(coords$Y == min(coords$Y)),]
# 
# index <- st_centroid(st_union(index))
# 
# clippt <- st_transform(clippt, crs = proj)
# index <- st_transform(index, crs = proj)
# 
# clippt$distance <- as.numeric(st_distance(clippt, index, which = "Euclidean"))
# 
# clippt <- st_transform(clippt, crs = st_crs(xmids_SDAll[[1]])$input)
# clippt
# 
# clippt[,c("x", "y")] <- st_coordinates(clippt)
# clippt <- clippt %>% st_drop_geometry() %>% select(-layer.1)
# head(clippt)
# 
# #turn back into raster
# new <- rasterize(x = clippt[,c("x", "y")],
#           y = xmids_SDAll[[1]],
#           field = clippt[,c("distance")],
#           fun = mean)
# 
# new
# plot(new)
# 
# newprj <- projectRaster(new, crs = proj)
# newprj
# plot(newprj)
# 
# RDH_DistanceAlongCorridor <- newprj
# 
# setwd("C:/Users/lwilde2/Documents/PhD_AdaptiveFidelity")
# save(RDH_DistanceAlongCorridor, file = "Data_out/Data/GIS/RDH_DistanceAlongCorridor.RData")
# save(Corr_all, file = "Data_out/Data/GIS/RDH_CorridorStackShapefile.RData")
