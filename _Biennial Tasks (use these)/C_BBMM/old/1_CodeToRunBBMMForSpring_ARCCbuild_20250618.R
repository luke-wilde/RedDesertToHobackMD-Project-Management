
#Clean working environment
rm(list=ls())
gc()


Sys.setenv(TZ = "America/Denver")

#--------------------------------#
# set paths and environment ####

# Retrieve path set in SLURM script
project_path <- "/project/wyldtech1/lwilde2/scripts"


.libPaths(c("/project/wyldtech1/Renv2", .libPaths()))


#---------------------------------# 
# Start code ####
suppressMessages(library(sf))
suppressMessages(library(raster))
#suppressMessages(library(rgeos))
suppressMessages(library(mapview))
suppressMessages(library(tidyverse))
#suppressMessages(library(adehabitatHR))
suppressMessages(library(move))
suppressMessages(library(BBMM)) #must be downloaded from tar.gz file
suppressMessages(library(lubridate))
suppressMessages(library(parallel))

filter <- dplyr::filter 




#-------------------------#
# set up directory paths ####
# Paths derived from SLURM variable
#locsub <- "Y:/medbow/project/wyldtech1/lwilde2"

subpath <- "/project/wyldtech1/lwilde2"
data_path <- paste0(subpath, "/data2/RDH_SpringMigration_2011t2024_05262025.RData")

#create a folder to write your BBs to
outFolder <- "/project/wyldtech1/lwilde2/out2/Spring"
if(dir.exists(outFolder)==FALSE){   # if it doesn't exist, it'll create it for you
  dir.create(outFolder)
}
# check if there any other files in it
length(dir(outFolder))   # Should only be Kernels in here. You may want to remove files from this folder.
dir(outFolder)



#--------------------------#
# load data ####

loaded <- load(data_path)
mig <- get(loaded)


#--------------------------#
# keep only complete ####

#make sure id_yr is right
mig <- mig %>% mutate(id_yr = paste0(AID,"_",year(POSIXct)))


#select the fix rates:
minYr = 2015
maxYr = 2024 
fixrate = 2.2


#keep only complete
complete <- mig %>% mutate(Year = as.numeric(Year)) %>% group_by(Year, id_yr) %>% reframe(upp = quantile(diff, .7, na.rm = T)) %>% filter(case_when(Year < 2014 ~ upp <= (3600*3.2), 
                                                                                                                                                                          Year == 2014 ~ upp <= (3600*1.2), 
                                                                                                                                                                          Year > 2014 ~ upp <= (3600*2.2)))


mig.full <- mig %>% filter(id_yr %in% complete$id_yr) %>% filter(Year >= minYr & Year <= maxYr)


mig.full[c("x","y")] <- st_coordinates(mig.full)



sub <- mig.full %>% dplyr::select("id_yr","x","y", "POSIXct")


u <- unique(sub$id_yr) 
num_IDyrs <- length(u)




#-------------------------------#
# slurm settings  ####

# chatgpt with guga:
args <- commandArgs(trailingOnly = TRUE)

# You can also access specific arguments
taskID <- as.numeric(args[1])
numtasks <- as.numeric(args[2])

cat("Task ID:", taskID, "\n")
cat("Number of tasks:", numtasks, "\n")


# Compute window size based on unique AIDs
windowsize <- 1 #ceiling(num_AIDs / numtasks)

# Define start and end indices for each task
start <- (taskID - 1) * windowsize + 1
end <- min(taskID * windowsize, num_IDyrs)

#--------------------------------------#
# end slurm #



#cancel this job if it exceeds the number of years
if (taskID > num_IDyrs) {
  system(paste("scancel", Sys.getenv("SLURM_JOB_ID")))
}




# select one at a time
u <- u[start:end]  # AIDs assigned to this task

#keep only dynamic Brownian bridge files
fls <- grep("regBB_",dir(outFolder), value=TRUE)
names <- sub("regBB_","",fls)
names <- sub(".tif","",fls)
names
names <- sapply(strsplit(names, "_"), function(x) {
  paste(tail(x, 2), collapse = "_")
})

#skip if already done
if(u %in% names){
  system(paste("scancel", Sys.getenv("SLURM_JOB_ID")))
  stop()
}

#keep only the selected
sub <- sub %>% dplyr::filter(id_yr %in% u) %>% arrange(id_yr, POSIXct) 

names(sub) <- c("id_yr","x","y","date","geometry")

# fall.mig.sub <- spTransform(fall.mig.sub, CRS("+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +units=m +no_defs+towgs84=0,0,0" ))

sub$date <- as.POSIXct(sub$date, format = "%Y-%m-%d %H:%M:%S", tz = "MST")

head(sub)
str(sub)


# #---------------------------------------#
# #Step 1f. BBMM of Fall ####

# First, must mark locations with relatively large gaps bewteen them (e.g., > 8 hrs)
data <- sub %>% 
  arrange(id_yr, date)  #order the database by id and date
dif <- data$date %>%   # calculate change in time between points (in hrs)
  as.numeric() %>% 
  diff() %>% 
  as.numeric() %>% 
  c(0)/3600
#replace with a 0 when it switches individual/season
dif[c(diff(as.numeric(as.factor(paste(data$id_yr)))),0) != 0] <- 0 
hist(dif)
table(dif > (fixrate*2))    #how often are your points skipping?
#add a burst column for when there are breaks larger than 7 hours in the data (so i can miss 1 location with my 3hr location data)
MaxFixInterval <- (fixrate*2)     # identify your max interval as an object because we'll use it later
data$connect <- ifelse(dif > MaxFixInterval, "no", "yes")
table(data$connect)
rm(dif)

# first, create raster/grid to calculate UDs over
ext <- extent(data)
multiplyers <- c((ext[2]-ext[1])*0.3, (ext[4]-ext[3])*0.3)   # add about 30% around the edges of your extent (you can adjust this if necessary)
ext <- extend(ext, multiplyers)
grd <- raster(ext)
res(grd) <- 100     #i'm using a 250m resolution here. Might want to increase this if things are going slowly. Or decrease it if you want more precision
ncell(grd)     # my grid has 5,612 cells at 250m resolution
projection(grd) <- st_crs(data)$proj4string  # tell it the projection of the extent
rm(multiplyers, ext)

#keep id_yr for naming later
taskIDYr <- data$id_yr[1]
#--------------------------#
# ####

# brownian.bridge(x=st_coordinates(data)[,1],
#                 y=st_coordinates(data)[,2],
#                 time.lag=diff(as.numeric(data$date)/60),   # calculate the time differences in the points
#                 area.grid=coordinates(data),   # use your same grid from kernel
#                 max.lag=MaxFixInterval*60,   # specify max fix interval in minutes
#                 location.error=20)

compute_bb <- function(temp = temp){

# this is the function to calculate the regular BB
bb <- try(brownian.bridge(x=st_coordinates(temp)[,1],
                          y=st_coordinates(temp)[,2],
                          time.lag=diff(as.numeric(temp$date)/60),   # calculate the time differences in the points
                          area.grid=coordinates(grd),   # use your same grid from kernel
                          max.lag=MaxFixInterval*60,   # specify max fix interval in minutes
                          location.error=20), #this is the location error of your collars
          silent=TRUE)    

#write out results to file as you go, so if there is an error you don't loose all your work!
if(class(bb)=="try-error"){
  return(data.frame(id_yr=taskIDYr, MotionVar=NA))   #return an NA so you know there was an error
}else{   # if the BB did NOT fail
  rast <- grd
  rast[] <- bb$probability   #this is the probability from bb, put into values of grd
  writeRaster(rast, paste0(outFolder,"/regBB_",taskID,"_",taskIDYr,".tif"), 
              format="GTiff", overwrite=TRUE)
  return(data.frame(id_yr=taskIDYr, MotionVar=bb[[1]]))    # have it return the motion variance
}

}#end function

compute_bb(temp = data)

cat("Finished: ", taskIDYr, "\n")


#--------------------------#
# extract stopovers ####
percentile <- 0.75   #what contour percentile are you interested in? following Sawyer and Kauffman 2011

rast <- raster(paste0(outFolder,"/regBB_",taskID,"_",taskIDYr,".tif"))
sum(values(rast))    #these will sum to 1, because it is a probability
rastR <- getVolumeUD(as(rast, Class="DBBMM"))    #turn it into a volume, where it starts with the highest use areas (uses move package)
polyR <- reclassify(rastR, rcl=matrix(c(0,percentile,1,percentile,1,0),2,3, byrow=T))
polyR[values(polyR)==0] <- NA
polyR <- rasterToPolygons(polyR, dissolve=TRUE) #this is the spatialPolygonsDataFrame representing the contour
polyR <- st_as_sf(polyR)
names(polyR)[1] <- "value"
polyR$id_yr <- taskIDYr
polyR <- polyR %>% dplyr::filter(value > .99)


#remove excessively small SOs
min_area <- 23000 #in sq-m

polyR.split <- st_cast(polyR, "POLYGON")
polyR.split <- polyR.split %>% filter(as.numeric(st_area(.)) > min_area)


st_write(polyR.split, paste0(outFolder,"/RD2H_",taskID,"_",taskIDYr,".shp"), append = F)


