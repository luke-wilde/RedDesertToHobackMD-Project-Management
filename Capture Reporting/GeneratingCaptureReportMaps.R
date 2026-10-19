# Code to prepare capture report maps
# Author Luke Wilde

# Date created: 05032022

# Last update: NULL

#------------------------------------------#
# Load Libraries ####
#------------------------------------------#
library(ggmap)
#install.packages("rstudioapi")
library(rstudioapi)
#install_github('Chrisjb/basemapR')
library(basemapR)
library(sf) 
library(rgdal) 
library(devtools)
#devtools::install_github('oswaldosantos/ggsn')
library(ggsn)
library(ggthemes)
library(cowplot)
library(tidyverse)

ggmap::register_google(key = "AIzaSyCT0Bg9WLKFO1cgPN6DhKGgD0mVWrZXBVQ", write = TRUE)

#------------------------------------------#
# Load Data ####
#------------------------------------------#

#boulder
Boulder <- st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureReports/March2023/Mesa_March2023_AllCaps.kml")
Boulder <-st_zm(Boulder, drop=T)

Boulder <- Boulder %>% mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])


# red desert
RedDesert <- st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureReports/March2023/RDH_March2023_AllCaps.kml")
RedDesert <-st_zm(RedDesert, drop=T)

RedDes_DF <- st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureReports/March2023/RDH_March2023_AllCaps_DoeFawn.kml")
RedDes_DF <-st_zm(RedDes_DF, drop=T)

RedDesert <- st_join(RedDesert, RedDes_DF)

RedDesert <- RedDesert %>% dplyr::mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])

#bring in staging areas
RDStaging <- st_read("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureReports/March2023/RDH_March2023_StagingAreas.kml")
RDStaging <-st_zm(RDStaging, drop=T)

RDStaging <- RDStaging %>% dplyr::mutate(lon = st_coordinates(.)[,1], lat = st_coordinates(.)[,2])

#state shape
library(spData)
data("us_states", package = "spData")

Wystate <- us_states %>% dplyr::filter(NAME %in% c("Wyoming"))

Wypoint <- st_point_on_surface(Wystate)

Wycoord <- as.data.frame(st_coordinates(Wypoint))
Wycoord$NAME <- Wystate$NAME

#------------------------------------------#
# Build map: Boulder Deer ####
#------------------------------------------#
bbox <- c(left = st_bbox(Boulder)[1]-.15, bottom = st_bbox(Boulder)[2]-.1, right = st_bbox(Boulder)[3]+.1, top = st_bbox(Boulder)[4]+.15)

WYmap <- get_stamenmap(bbox = bbox, zoom = 8, maptype = "terrain-background")

center <- c(lon=((bbox[1]+bbox[3])/2), lat=((bbox[2]+bbox[4])/2))
WYggmap <- get_googlemap(center=c(lon = -109.7011, lat = 42.65059), zoom=10, scale=4, type='roadmap')

Boulder$Cap <- "March 2022 Capture Locations"

pp <- ggmap(WYggmap) + geom_sf(data = Boulder, fill="blue", size=3.5 , shape=21, aes(color=Cap)) + coord_sf(expand = FALSE) + theme_bw() + theme(legend.position = c(0.2, 0.08), legend.background = element_rect(fill = alpha("white", 0.5)), legend.key=element_rect(fill = alpha("white", .0))) + theme(axis.text.y = element_text(angle = 360))   + scalebar(x.min = (bbox[3] - 0.08), x.max = (bbox[1] + 0.32), y.min = (bbox[2] + 0.05), y.max = (bbox[4] - 0.3), dist = 15, dist_unit = "km",transform = TRUE, model = "WGS84", st.size=3) + labs(x="", y="", color="") + scale_color_manual(values="black") #+ north(Boulder, location = "topleft", scale=0.15, symbol=12)

pp_ins <- ggplot() + geom_sf(data = Wystate, fill = "white") + geom_sf(data = st_as_sfc(st_bbox(Boulder)), fill=NA, color = "red", size=1.2) + geom_text(data = Wycoord, aes(X, Y*1.02, label = NAME), size = 8, colour = "black") + theme_void(); pp_ins

ggdraw() + draw_plot(pp) + draw_plot(pp_ins, x=0.36, y = 0.68, height = 0.15)


ggsave("C:/Users/lwilde2/Desktop/WGFD Sublette MD Project/Capture Report/2022/CapLoc_map_Bldr.png", dpi=600, height=10, width = 8)

#------------------------------------------#
# Build map: Red Desert Deer ####
#------------------------------------------#
RDbbox <- c(left = extent(RedDesert)[1]-.15, bottom = extent(RedDesert)[3]-.1, right = extent(RedDesert)[2]+.1, top = extent(RedDesert)[4]+.15)

WYmap <- get_stamenmap(bbox = RDbbox, zoom = 8, maptype = "terrain-background")

center <- c(lon=((RDbbox[1]+RDbbox[3])/2), lat=((RDbbox[2]+RDbbox[4])/2))
RDWYggmap <- get_googlemap(center=c(lon = -109.16194, lat = 42.19385), zoom=9, scale=4, type='hybrid')

RedDesert$Cap <- "March 2022 Capture Locations"
RDStaging$Cap <- "March 2022 Staging Area Locations"

p <- ggmap(RDWYggmap, show.legend=T) + geom_sf(data = RedDesert, size=3.5 ,  aes(shape=Cap, fill=Cap), show.legend = T) + coord_sf(expand = FALSE) + geom_sf(data=RDStaging,  size=3.5 ,  aes(shape=Cap, fill=Cap), show.legend = T) + theme_bw() + theme(legend.position = c(0.2, 0.08), legend.background = element_rect(fill = alpha("white", 0.5)), legend.key=element_rect(fill = alpha("white", .0))) + theme(axis.text.y = element_text(angle = 360)) + labs(x="", y="", shape="", fill="") + scale_fill_manual(values=c("blue","red")) + scale_shape_manual(values=c(21,24))
p

pp <- p + scalebar(x.min = (RDbbox[3] + 0.25), x.max = (RDbbox[1] + 0.32), y.min = (RDbbox[4] - 0.1), y.max = (RDbbox[4] + 1), dist = 30, dist_unit = "km",transform = TRUE, model = "WGS84", st.size=4)  #+ north(Boulder, location = "topleft", scale=0.15, symbol=12)

pp_ins <- ggplot() + geom_sf(data = Wystate, fill = "white") + geom_sf(data = st_as_sfc(st_bbox(RedDesert)), fill=NA, color = "red", size=1.2) + geom_text(data = Wycoord, aes(X, Y*1.02, label = NAME), size = 8, colour = "black") + theme_void(); pp_ins

ggdraw() + draw_plot(pp) + draw_plot(pp_ins, x=0.36, y = 0.68, height = 0.15)


ggsave("C:/Users/lwilde2/Desktop/WGFD Sublette MD Project/Capture Report/2022/CapLoc_map_RDH.png", dpi=600, height=10, width = 8)


#ggsave("C:/Users/lwilde2/Desktop/WGFD Sublette MD Project/Capture Report/2022/CapLoc_map_2.png", dpi=600, height=10, width = 8)

#-------------------------------------------#
# Build map: Red Desert and Boulder Deer ####
#-------------------------------------------#
ALLbbox <- c(left = extent(RedDesert)[1]-.95, bottom = extent(RedDesert)[3]+.9, right = extent(RedDesert)[2]+.1, top = extent(RedDesert)[4]+.55)

WYmap <- get_stamenmap(bbox = RDbbox, zoom = 8, maptype = "terrain-background")

center <- c(lon=((ALLbbox[1]+ALLbbox[3])/2), lat=((ALLbbox[2]+ALLbbox[4])/2))
ALLWYggmap <- get_googlemap(center=c(lon = -109.437223, lat = 42.235185), zoom=9, scale=4, type='hybrid')

RedDesert$Cap <- "March 8-10 2022 Capture Locations"
RDStaging$Cap <- "March 8-10 2022 Staging Area Locations"
Boulder$Cap <- "March 15 2022 Capture Locations"

p <- ggmap(ALLWYggmap, show.legend=T) + geom_sf(data = RedDesert, size=3.5 ,  aes(shape=Cap, fill=Cap), show.legend = T) + coord_sf(expand = FALSE) + geom_sf(data=RDStaging,  size=3.5 ,  aes(shape=Cap, fill=Cap), show.legend = T) + geom_sf(data = Boulder,  size=3.5, aes(shape = Cap, fill=Cap), show.legend = T) + theme_bw() + theme(legend.position = c(0.2, 0.08), legend.background = element_rect(fill = alpha("white", 0.5)), legend.key=element_rect(fill = alpha("white", .0))) + theme(axis.text.y = element_text(angle = 360)) + labs(x="", y="", shape="", fill="")  + scale_fill_manual(values=c("purple","blue","red")) + scale_shape_manual(values=c(21,21,24))
p

pp <- p + scalebar(x.min = (RDbbox[3] + 0.05), x.max = (RDbbox[1] + 0.22), y.min = (RDbbox[4] - 0.05), y.max = (RDbbox[4] + 1.2), dist = 30, dist_unit = "km",transform = TRUE, model = "WGS84", st.size=4); pp #+ scalebar(x.min = (-108.79 + 0.35), x.max = (-110.03 - 0.32), y.min = (41.60 - 0.1), y.max = (42.84 + 1), dist = 30, dist_unit = "km",transform = TRUE, model = "WGS84", st.size=4); pp  #+ north(Boulder, location = "topleft", scale=0.15, symbol=12)


tmp <- data.frame("xmin" = -110.03494, "ymin" = 41.60320, "xmax" = -108.79185, "ymax" = 42.84387)

tmp2 <- st_union(RedDesert, Boulder)
st_bbox(tmp2)

pp_ins <- ggplot() + geom_sf(data = Wystate, fill = "white") + geom_sf(data = st_as_sfc(st_bbox(tmp2)), fill=NA, color = "red", size=1.2) + geom_text(data = Wycoord, aes(X, Y*1.02, label = NAME), size = 8, colour = "black") + theme_void(); pp_ins

ggdraw() + draw_plot(pp) + draw_plot(pp_ins, x=0.36, y = 0.68, height = 0.12)


ggsave("C:/Users/lwilde2/Desktop/WGFD Sublette MD Project/Capture Report/2022/JointCapMap_RDH_BLDR.png", dpi=600, height=12, width = 9)



#------------------------------------------#
# Build map: Deer Current ####
#------------------------------------------#
#uses code shared with Anne
RDpoint <- final.point
RDlines <- final.lines

BLpoint <- final.point
BLlines <- final.lines



mapview(BLpoint) + mapview(BLlines)

mapview(RDpoint) + mapview(RDlines)


ALLbbox <- c(left = extent(RedDesert)[1]-.75, bottom = extent(RedDesert)[3]-.5, right = extent(RedDesert)[2]+.5, top = extent(RedDesert)[4]+.75)

#WYmap <- get_stamenmap(bbox = RDbbox, zoom = 8, maptype = "terrain-background")

center <- c(lon=((ALLbbox[1]+ALLbbox[3])/2), lat=((ALLbbox[2]+ALLbbox[4])/2))
ALLWYggmap <- get_googlemap(center=c(lon = -109.26194, lat = 42.29385), zoom=9, scale=4, type='hybrid')

ggmap(RDWYggmap, show.legend=T) + geom_sf(BLpoint) + geom_sf(RDpoint) 

RedDesert$Cap <- "March 2022 Capture Locations"
RDStaging$Cap <- "March 2022 Staging Area Locations"

p <- ggmap(RDWYggmap, show.legend=T) + geom_sf(data = RedDesert, size=3.5 ,  aes(shape=Cap, fill=Cap), show.legend = T) + coord_sf(expand = FALSE) + geom_sf(data=RDStaging,  size=3.5 ,  aes(shape=Cap, fill=Cap), show.legend = T) + theme_bw() + theme(legend.position = c(0.2, 0.08), legend.background = element_rect(fill = alpha("white", 0.5)), legend.key=element_rect(fill = alpha("white", .0))) + theme(axis.text.y = element_text(angle = 360)) + labs(x="", y="", shape="", fill="") + scale_fill_manual(values=c("blue","red")) + scale_shape_manual(values=c(21,24))
p

pp <- p + scalebar(x.min = (RDbbox[3] + 0.25), x.max = (RDbbox[1] + 0.32), y.min = (RDbbox[4] - 0.1), y.max = (RDbbox[4] + 1), dist = 30, dist_unit = "km",transform = TRUE, model = "WGS84", st.size=4)  #+ north(Boulder, location = "topleft", scale=0.15, symbol=12)

pp_ins <- ggplot() + geom_sf(data = Wystate, fill = "white") + geom_sf(data = st_as_sfc(st_bbox(RedDesert)), fill=NA, color = "red", size=1.2) + geom_text(data = Wycoord, aes(X, Y*1.02, label = NAME), size = 8, colour = "black") + theme_void(); pp_ins

ggdraw() + draw_plot(pp) + draw_plot(pp_ins, x=0.36, y = 0.68, height = 0.15)


#ggsave("C:/Users/lwilde2/Desktop/WGFD Sublette MD Project/Capture Report/2022/CapLoc_map_RDH.png", dpi=600, height=10, width = 8)

