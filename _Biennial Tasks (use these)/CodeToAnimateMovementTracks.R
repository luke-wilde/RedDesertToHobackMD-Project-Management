# Code to create movevis maps

#-----------------#
# Load libs ####


rm(list = ls())

#https://cran.r-project.org/web/packages/brms/vignettes/brms_multivariate.html
#https://cran.r-project.org/web/packages/brms/vignettes/brms_missings.html#combining-measurement-error-and-missing-values

#devtools::install_github("jmerkle1/MerkleLabGIS")
#install.packages("C:/Users/lwilde2/Downloads/MoveTools_0.0.0.9001.tar.gz", repos = NULL, type = "source")

#load("C:/Users/lwilde2/Desktop/Remote Jobs/03.07.2025_MultivarBRMS/Renv_03.09.2025.RData")

# List of packages
packages <- c(
  "tidyverse", "sf", "lubridate", "mapview", "moveVis", "move"
)


# Install missing packages
installed_packages <- installed.packages()
for (pkg in packages) {
  if (!(pkg %in% installed_packages[, "Package"])) {
    install.packages(pkg)
  }
}

# Load all packages
lapply(packages, library, character.only = T)

library(devtools)
#install_github("ropensci/prism")

#correct select so that dplyr is preferred
select <- dplyr::select
summarise <- dplyr::summarise
rename <- dplyr::rename
filter <- dplyr::filter


setwd("C:/Users/lwilde2/Desktop/RDH Database/Processed Data")
#migrations and summary tables
#load("C:/Users/lwilde2/Desktop/RDH Database/Processed Data/RDH_AllMigrations_2014t2023_20241126.RData")

load("RDH_SpringStopovers_14t23_MergedAt5k_20240826.RData")
load("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_CleanData.RData")
load("RDH_SpringStopoverBundle_14to23_09032024.RData")

#--------------------#
# ####




sub22 <- rdh_data.sf %>% filter(AID %in% c(603,255) & Year %in% c(2022)) %>% st_transform(., "+proj=longlat +datum=WGS84") %>% filter(Month %in% c(3,4,5,6,7, 8)) %>% mutate(Date = ymd(Date), hour = hour(POSIXct)) %>% group_by(AID, Date) %>% filter(hour < 15 & hour > 11) %>% sample_n(1)
sub23 <- rdh_data.sf %>% filter(AID %in% c(603,255) & Year %in% c(2023)) %>% st_transform(., "+proj=longlat +datum=WGS84") %>% filter(Month %in% c(3,4,5,6,7, 8)) %>% mutate(Date = ymd(Date), hour = hour(POSIXct)) %>% group_by(AID, Date) %>% filter(hour == 14 | hour == 13) 

stops <- onstop_yr_S_fin %>% filter(AID %in% c(603,255))

stops <- stops %>% st_transform(.,"+proj=longlat +datum=WGS84")

stops22 <- stops %>% filter(Year == "2022")
stops23 <- stops %>% filter(Year == "2023")



# # Enable multi-core usage
# use_multicore()
# 
# # Allow disk use with a maximum of 50 frames in memory
# use_disk(frames_to_disk = T, dir_frames = paste0(tempdir(), "/moveVis"), n_memory_frames = 100)

# Create move object
move22 <- move(x = sub22$Long, y = sub22$Lat, time = sub22$POSIXct, animal = sub22$AID, proj = CRS("+proj=longlat +datum=WGS84"))

# Align move data to a uniform time scale
move22_aligned <- align_move(move22, res = "minimum", unit = "days")

get_maptypes()

# # Explode the polygons into individual points
# stops22_points <- stops22 %>%
#   st_cast("POINT")
# 
# # Extract coordinates
# coordinates <- st_coordinates(stops22_points) %>%
#   as.data.frame() %>%
#   rename(Longitude = X, Latitude = Y)
# 
# # Repeat the attributes for each point
# attributes <- stops22_points %>%
#   st_drop_geometry() %>%
#   slice(rep(1:n(), each = lengths(st_geometry(stops22))))
# 
# # Combine the coordinates with the repeated attributes
# stops22df <- bind_cols(coordinates, attributes)
# stops22df$group_combined <- with(stops22df, interaction(AID, grp1.c, drop = TRUE))
# stops22df <- stops22df %>% 
#   arrange(AID, grp1.c, Longitude, Latitude)
# 

# Create frames for the animation
frames <- frames_spatial(move22_aligned, path_size = 5, path_colours = c("#377EB8","#984EA3"), tail_colour = c("white"), trace_show = F, equidistant = FALSE, path_fade = T, 
                         map_service = "osm_stamen", map_type = "terrain", maxpixels = 5.5e+05, map_token = "acb8ca1f-1602-4ad1-93b4-bf43f8a2cbe9") %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% 
  add_timestamps(type = "label", size = 2) %>% 
   add_scalebar(height = 0.015) %>% 
  add_progress()  %>%
   add_gg(gg = expr(theme(legend.position = "none",axis.title.x = element_blank(), axis.title.y = element_blank(),text = element_text(size = 6))))

#frames1 = add_gg(frames, g = expr(geom_point(aes(x = Longitude, y = Latitude, color = factor(AID), group = group_combined), shape = 21, size = 1, alpha = .1, data = stops22df)), data = stops22df )

ggplot(stops22) + geom_sf(aes(color = AID), shape = 16, size = 4, alpha = .05) + theme_void() + scale_color_manual(values = c("#377EB8","#984EA3")) + theme(legend.position = "none") 

ggsave("C:/Users/lwilde2/Downloads/RDH_stopovers.png", width = 2400,
        height = 3200, units = "px",
        dpi = 600)

render_frame(frames, i = 86) #+ geom_sf(aes(color = factor(AID), fill = factor(AID), group = factor(AID)), data = stops22, alpha = .15)

animate_frames(frames,  fps = 8, #12
               width = 2400,
               height = 3200,
               res = 600,
                out_file = "C:/Users/lwilde2/Downloads/RDH_Profiles_moveVis_vid3_v1.mov", overwrite = T)


# Disable multi-core usage
use_multicore(FALSE)

# Disable disk usage
use_disk(FALSE)




# Create move object
move23 <- move(x = sub23$Long, y = sub23$Lat, time = sub23$POSIXct, animal = sub23$AID, proj = CRS("+proj=longlat +datum=WGS84"))

# Align move data to a uniform time scale
move23_aligned <- align_move(move23, res = "minimum", unit = "days")

get_maptypes()

# # Explode the polygons into individual points
# stops22_points <- stops22 %>%
#   st_cast("POINT")
# 
# # Extract coordinates
# coordinates <- st_coordinates(stops22_points) %>%
#   as.data.frame() %>%
#   rename(Longitude = X, Latitude = Y)
# 
# # Repeat the attributes for each point
# attributes <- stops22_points %>%
#   st_drop_geometry() %>%
#   slice(rep(1:n(), each = lengths(st_geometry(stops22))))
# 
# # Combine the coordinates with the repeated attributes
# stops22df <- bind_cols(coordinates, attributes)
# stops22df$group_combined <- with(stops22df, interaction(AID, grp1.c, drop = TRUE))
# stops22df <- stops22df %>% 
#   arrange(AID, grp1.c, Longitude, Latitude)
# 

# Create frames for the animation
frames <- frames_spatial(move23_aligned, path_size = 5, path_colours = c("#377EB8","#984EA3"), tail_colour = c("white"), trace_show = F, equidistant = FALSE, path_fade = T, 
                         map_service = "osm_stamen", map_type = "terrain", maxpixels = 5.5e+05, map_token = "acb8ca1f-1602-4ad1-93b4-bf43f8a2cbe9") %>% 
  add_labels(x = "Longitude", y = "Latitude") %>% 
  add_timestamps(type = "label", size = 2) %>% 
  add_scalebar(height = 0.015) %>% 
  add_progress()  %>%
  add_gg(gg = expr(theme(legend.position = "none",axis.title.x = element_blank(), axis.title.y = element_blank(),text = element_text(size = 6))))

#frames1 = add_gg(frames, g = expr(geom_point(aes(x = Longitude, y = Latitude, color = factor(AID), group = group_combined), shape = 21, size = 1, alpha = .1, data = stops22df)), data = stops22df )

#ggplot(stops23) + geom_sf(aes(color = AID), shape = 16, size = 4, alpha = .05) + theme_void() + scale_color_manual(values = c("#377EB8","#984EA3")) + theme(legend.position = "none") 

#ggsave("C:/Users/lwilde2/Downloads/RDH_stopovers23.png", width = 2400,
       #height = 3200, units = "px",
       #dpi = 600)

render_frame(frames, i = 86) #+ geom_sf(aes(color = factor(AID), fill = factor(AID), group = factor(AID)), data = stops22, alpha = .15)

animate_frames(frames,  fps = 8,
               width = 2400,
               height = 3200,
               res = 600,
               out_file = "C:/Users/lwilde2/Downloads/RDH_Profiles_moveVis_vid2023_v1.mov", overwrite = T)


