
rm(list = ls())
library(tidyverse)
library(sf)
library(adehabitatHR)
library(MoveTools)
library(raster)
library(terra)
library(move)
library(parallel)
library(mapview)

#load data
load("G:/Other computers/My Laptop/RDH Database/Processed Data/RDH_AllMigrations_2011t2025_05142026.RData")

load("G:/Other computers/My Laptop/RDH Database/Processed Data/RDH_AllSeasonalRanges_2011t2025_05142026.RData")

#--------------------------#
# do the migration first ####
lines <- Points2Lines(mig.SpringMigration.sf %>% filter(Mgtry == "Long") %>% as(., "Spatial") %>% st_as_sf(), date_name = "POSIXct", id_name = "id_yr", byid = T, no_cores = detectCores()-1)

#create buffers
buffs <- st_buffer(lines, dist = 400)

# Create an empty raster with the desired resolution
r <- raster(extent(buffs), resolution = 100)  # Adjust resolution as needed

# Rasterize the polygons
r <- rasterize(st_as_sf(st_geometry(buffs)), r, fun = 'count', background = 0)

# Get the count of overlapping polygons at each cell
overlap_counts <- values(r)

# Calculate the threshold for 95% overlap
threshold <- quantile(overlap_counts, probs = 0.95, na.rm = TRUE)

# Create a binary raster indicating cells with at least 95% overlap
overlap_95 <- r >= threshold

# Convert the binary raster to polygons
overlap_95_polygons <- rasterToPolygons(overlap_95, dissolve = TRUE)

# Convert to sf object
overlap_95_sf <- st_as_sf(overlap_95_polygons)

RDH_MigCorr_14t25 <- overlap_95_sf %>% filter(layer == 1)
plot(RDH_MigCorr_14t25)

mapview(RDH_MigCorr_14t25)

save(RDH_MigCorr_14t25, file = "G:/Other computers/My Laptop/RDH Database/Processed Data/RDH_95CorrByLineBuff_14t25.RData")
# st_write(RDH_MigCorr_14t25, "G:/Other computers/My Laptop/RDH Database/RDH_95CorrByLineBuff_14t25.shp")

# # NOT RUN
#
# #--------------------------#
# # Now both summer and winter ####
# # Assuming mig.SpringMigration.sf is already an sf object
# 
# mcp <- WinterRanges.sf %>%
#   ungroup() 
# mcp <- as(mcp, "Spatial")
# 
# winterMCP_100 <- mcp(mcp, percent = 100)
# winterMCP_95 <- mcp(mcp, percent = 95)
# 
# mapview(winterMCP_95)
# 
# st_write(st_as_sf(winterMCP_95), "C:/Users/lwilde2/Downloads/RDH_WinterRange_95MCP_14t23.shp")
# st_write(st_as_sf(winterMCP_100), "C:/Users/lwilde2/Downloads/RDH_WinterRange_100MCP_14t23.shp")
# 
# mcp <- SummerRange.sf %>%
#   ungroup() 
# mcp <- as(mcp, "Spatial")
# 
# summerMCP_100 <- mcp(mcp, percent = 100)
# summerMCP_95 <- mcp(mcp, percent = 95)
# 
# mapview(summerMCP_95)
# 
# 
# 
# 
# 
# # Calculate the centroid of the convex hull
# centroid <- st_centroid(mcp)
# 
# # Calculate distances from the centroid to all points
# distances <- st_distance(WinterRange.sf, centroid)
# 
# # Combine distances with the original data
# WinterRange.sf <- WinterRange.sf %>%
#   mutate(distance = as.numeric(distances))
# 
# # Sort by distance and select the top 95% closest points
# WinterRange_95 <- WinterRange.sf %>%
#   arrange(distance) %>%
#   slice(1:ceiling(n() * 0.95))
# 
# # Calculate the convex hull of the 95% points
# mcp_95 <- WinterRange_95 %>%
#   st_union() %>%
#   st_convex_hull()