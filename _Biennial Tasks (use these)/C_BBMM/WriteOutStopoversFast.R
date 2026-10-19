# Retrieve path set in SLURM script
project_path <- "/project/wyldtech1/lwilde2/scripts"


.libPaths(c("/project/wyldtech1/Renv2", .libPaths()))

# Load libraries quietly
suppressMessages({
  library(terra)
  library(sf)
  library(raster)
  library(mapview)
  library(tidyverse)
  library(move)
  library(BBMM)
  library(lubridate)
  library(parallel)
  library(igraph)
  library(foreach)
  library(doParallel)
  library(foreach)
  library(doSNOW)
})

# Set directories
storeFolder <- "/project/wyldtech1/lwilde2/data2/regBB/Fall/"
outFolder   <- "/project/wyldtech1/lwilde2/data3/Fall/"


#keep only dynamic Brownian bridge files
fls <- grep("regBB_.*\\.tif$",dir(storeFolder), value=TRUE)
names <- sub("regBB_","",fls)
names <- sub(".tif","",fls)

names <- sapply(strsplit(names, "_"), function(x) {
  paste(tail(x, 2), collapse = "_")
})
#names

fls1 <- grep("RD2H_.*\\.shp$", dir(outFolder), value = TRUE)
names1 <- sub("RD2H_","",fls1)
names1 <- sub(".shp","",fls1)

names1 <- sapply(strsplit(names1, "_"), function(x) {
  paste(tail(x, 2), collapse = "_")
})

num <- length(names)

# chatgpt with guga:
args <- commandArgs(trailingOnly = TRUE)

# You can also access specific arguments
taskID <- as.numeric(args[1])
numtasks <- as.numeric(args[2])

cat("Task ID:", taskID, "\n")
cat("Number of tasks:", numtasks, "\n")


# Compute window size based on unique AIDs
windowsize <- ceiling(num / numtasks)

# Define start and end indices for each task
start <- (taskID - 1) * windowsize + 1
end <- min(taskID * windowsize, num)

newfls <- fls[start:end]
newnames <- names[start:end]


# Define percent threshold and patch cutoff
stopover_percent <- 10
min_area <- 23000  # m^2

for(i in 1:length(newnames)){
  rast <- raster(paste0(storeFolder, newfls[i]))
  # Threshold raster at 99% cumulative probability
  cutoff_vals <- sort(values(rast), decreasing = TRUE)
  cutoff <- cutoff_vals[cumsum(cutoff_vals) > 0.99][1]
  values(rast)[values(rast) < cutoff] <- 0
  values(rast) <- values(rast) / sum(values(rast), na.rm = TRUE)
  # Compute volume UD
  popUDVol <- getVolumeUD(as(rast, "DBBMM"))
  qtl <- quantile(popUDVol[popUDVol != 1], probs = stopover_percent / 100)
  stopovers <- reclassify(popUDVol, rcl = matrix(c(0, qtl, 1, qtl, Inf, NA), ncol = 3, byrow = TRUE))
  # Remove small patches
  clmps <- clump(stopovers)
  clmpvals <- na.omit(values(clmps))
  clmp_table <- data.frame(clmpvals = clmpvals) %>%
    count(clmpvals, name = "Freq") %>%
    mutate(Freq = Freq * prod(res(stopovers)))
  small_ids <- as.numeric(clmp_table$clmpvals[clmp_table$Freq < min_area])
  clmps[is.na(values(clmps))] <- -5
  stopovers[values(clmps) %in% small_ids] <- NA
  # Convert to polygons
  stopovers_poly <- rasterToPolygons(stopovers, dissolve = TRUE)
  stopovers_poly$id_yr <- newnames[i]
  names(stopovers_poly)[1] <- "value"
  stopovers_sf <- st_as_sf(stopovers_poly)
  st_write(stopovers_sf, paste0(outFolder, "RD2H_", newnames[i], ".shp"), append = FALSE, quiet = TRUE)
}













# 
# 
# .libPaths(c("/project/wyldtech1/Renv2", .libPaths()))
# 
# 
# #---------------------------------# 
# # Start code ####
# 
# suppressMessages(library(terra))
# suppressMessages(library(sf))
# suppressMessages(library(raster))
# #suppressMessages(library(rgeos))
# suppressMessages(library(mapview))
# suppressMessages(library(tidyverse))
# #suppressMessages(library(adehabitatHR))
# suppressMessages(library(move))
# suppressMessages(library(BBMM)) #must be downloaded from tar.gz file
# suppressMessages(library(lubridate))
# suppressMessages(library(parallel))
# suppressMessages(library(igraph))
# 
# 
# storeFolder <- "/project/wyldtech1/lwilde2/data2/regBB/Fall/"
# outFolder <- "/project/wyldtech1/lwilde2/data3/Fall"
#   
# #keep only dynamic Brownian bridge files
# fls <- grep("regBB_",dir(storeFolder), value=TRUE)
# names <- sub("regBB_","",fls)
# names <- sub(".tif","",fls)
# 
# names <- sapply(strsplit(names, "_"), function(x) {
#   paste(tail(x, 2), collapse = "_")
# })
# names
# 
# cores <- 4
# cl <- makeCluster(cores, type = "SOCK")
# registerDoParallel(cl)
# 
# for(i in 1:length(names)){
#   
#   stopover_percent <- 10
# min_area <- 23000  # m^2
# 
# rast <- raster(paste0(storeFolder,fls[i]))
# cutoff <- sort(values(rast), decreasing = TRUE)
# vlscsum <- cumsum(cutoff)
# cutoff <- cutoff[vlscsum > 0.99][1]
# values(rast)[values(rast) < cutoff] <- 0
# values(rast) <- values(rast) / sum(values(rast))  # normalize
# 
# # Volume UD
# popUDVol <- getVolumeUD(as(rast, Class = "DBBMM"))
# qtl <- quantile(popUDVol[popUDVol != 1], probs = stopover_percent / 100)
# stopovers <- reclassify(popUDVol, rcl = matrix(c(0, qtl, 1, qtl, Inf, NA), 2, 3, byrow = TRUE))
# 
# # Filter out small patches
# clmps <- clump(stopovers)
# clmpvals <- na.omit(values(clmps))
# clmpvals <- data.frame(clmpvals = clmpvals) %>%
#   count(clmpvals, name = "Freq") %>%
#   mutate(Freq = Freq * prod(res(stopovers)))
# small_ids <- as.numeric(clmpvals$clmpvals[clmpvals$Freq < min_area])
# clmps[is.na(values(clmps))] <- -5
# stopovers[values(clmps) %in% small_ids] <- NA
# 
# # Convert to polygons
# stopovers <- rasterToPolygons(stopovers, dissolve = TRUE)
# stopovers$id_yr <- names[i]
# names(stopovers)[1] <- "value"
# 
# stopovers <- st_as_sf(stopovers)
# 
# 
# st_write(stopovers, paste0(outFolder,"/RD2H_",names[i],".shp"), append = F)
# }