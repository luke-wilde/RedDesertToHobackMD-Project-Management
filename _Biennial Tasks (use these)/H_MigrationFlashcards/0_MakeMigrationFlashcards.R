

#------------------------------------------#
# Load Libraries ####
#------------------------------------------#
# library(devtools)
# remotes::install_github("huh/collar", build_vignettes = TRUE)
library(collar)
library(sf) 
library(adehabitatLT)
library(adehabitatHR)
library(tidyverse)
library(plotly)
library(leaflet)
library(sp)
library(doremi)
library(htmlwidgets)
library(htmltools)
library(MerkleLabGIS)
library(terra)
library(patchwork)
library(ggspatial)
library(gridExtra)
library(webshot)

#lyr <- bucket()

#ggmap::register_google(key = "AIzaSyCT0Bg9WLKFO1cgPN6DhKGgD0mVWrZXBVQ", write = TRUE)


#------------------------------------------#
# Load dataset ####
#------------------------------------------#

#load full dataset
load("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_CleanData.RData")

#load elevation
elev <- rast("/vsicurl/https://pathfinder.arcc.uwyo.edu/devise/MerkleLabGIS/DEM/etopocompressed.tif") #

# rdh_data.sf <- rdh_data.sf %>% group_by(id_yr) %>% mutate(month = month(POSIXct)) %>% filter(month )

rdh_data.sf <- rdh_data.sf %>% st_transform(., crs = "+proj=longlat +datum=WGS84") %>% mutate(bioyr = paste0(AID,"_",ifelse(JDate < 182, Year - 1, Year)))

# Prepare data for ltraj
RDH_m_sp <- as(rdh_data.sf, "Spatial")

xy <- coordinates(RDH_m_sp)
date <- as.POSIXct(RDH_m_sp$POSIXct)
id <- as.factor(RDH_m_sp$AID)
bioyr <- as.factor(RDH_m_sp$bioyr)

# Make a trajectory stack
RDH_lt <- as.ltraj(xy = xy, date = date, id = id, burst = bioyr, typeII = TRUE, 
                   proj4string = CRS(st_crs(rdh_data.sf)$proj4string))

# Convert to dataframe
RDH_nsd <- ld(RDH_lt)

RDH_nsd <- RDH_nsd %>% 
  st_as_sf(coords = c("x","y"), crs = st_crs(st_crs(rdh_data.sf)$proj4string)) %>% 
  mutate(month = month(date))

RDH_nsd$elev <- extract(elev, st_transform(RDH_nsd, crs = st_crs(elev)))[,2]

# Arrange
RDH_nsd <- RDH_nsd %>% arrange(id, burst, date)

# Define the PDF output file 
# 
# pdf("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/Migration Flashcards/AllMigrations_2014t2023.pdf", width = 8, height =11)
# 
# deer <- unique(RDH_nsd$id_yr)
# for(i in deer[1:3]){
#   #i = "108_2019"
#   ex <- RDH_nsd %>% filter(burst == i)
#   
#   #NSD <- ggplotly(ggplot(ex) + geom_point(aes(x = date, y = R2n, size = dist/dt), alpha = .1, color = "darkorange") + geom_line(aes(x = date, y = R2n))+labs(y= "NSD") + theme_classic() + theme(legend.position = "none"))
#   
#   NSD <- ggplot(ex) + geom_line(aes(x = date, y = R2n), linewidth = .9)+labs(y= "Net-square Displacement") + theme_classic() + theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
#   
#   elev <- ggplot(ex) + geom_line(aes(x = date, y = elev) )+labs(y= "Elevation (m)") + theme_classic() + theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
#   
#   speed <- ggplot(ex) + geom_line(aes(x = date, y = dist/dt) )+labs(y= "Speed (m/s)") + theme_classic() + theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
#   
#   lat <- ggplot(ex) + geom_line(aes(x = date, y = st_coordinates(ex)[,2]), linewidth = .9)+labs(y= "Latitude (UTM_N)") + theme_classic() + theme(legend.position = "none") + theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
#   
#   long <- ggplot(ex) + geom_line(aes(x = date, y = st_coordinates(ex)[,1]), linewidth = .9)+labs(y= "Longitude (UTM_E)") + theme_classic() + theme(legend.position = "none") + theme(legend.position = "none", axis.text.y = element_blank())
#   
#   # Create interactive leaflet map
#   #map <- ggplot(ex) + annotation_map_tile(type = "thunderforestlandscape") + geom_sf(aes(color = factor(month(date))))
#   #map
#   
#   #p <- NSD + ggtitle(i) + elev + speed + lat + long + plot_layout(nrow = 5, ncol = 1)
#    #Combine all widgets using htmltools 
#   # comb <- subplot(NSD, elev, nrows = 2, shareX = T)
#    # Save the combined widget 
#   # saveWidget(comb, file = paste0("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/HTMLs/", i, ".html"))
#   
#   # Arrange and print all plots on one page 
#  grid.arrange(NSD + ggtitle(i), elev, speed, lat, long, nrow = 5)
#   
# }
# dev.off()


# Define the PDF output file
pdf("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/Migration Flashcards/AllMigrations_2014t2023_2.pdf", width = 8, height = 11)

deer <- unique(RDH_nsd$burst)
for (i in deer) {
 # i = "112_2019"
  # Filter data for the current deer
  ex <- RDH_nsd %>% filter(burst == i)
  
  start_date <- as.Date(paste(year(ex$date[1]),"-01-01",sep=""))
  end_date <- as.Date(paste(year(ex$date[1]),"-12-31",sep=""))
  
  # Create plots
  NSD <- ggplot(ex) + 
    geom_line(aes(x = as.Date(date), y = R2n), linewidth = 0.9) + 
    labs(y = "Net-square Displacement") + 
    scale_x_date(limits = c(start_date, end_date), date_breaks = "months") + 
    theme_classic() + 
    theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
  
  elev <- ggplot(ex) + 
    geom_line(aes(x = as.Date(date), y = elev)) + 
    scale_x_date(limits = c(start_date, end_date), date_breaks = "months") + 
    labs(y = "Elevation (m)") + 
    theme_classic() + 
    theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
  
  speed <- ggplot(ex) + 
    geom_line(aes(x = as.Date(date), y = dist/dt)) + 
    scale_x_date(limits = c(start_date, end_date), date_breaks = "months") + 
    labs(y = "Speed (m/s)") + 
    theme_classic() + 
    theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
  
  lat <- ggplot(ex) + 
    geom_line(aes(x = as.Date(date), y = st_coordinates(ex)[,2]), linewidth = 0.9) + 
    scale_x_date(limits = c(start_date, end_date), date_breaks = "months") + 
    labs(y = "Latitude (UTM_N)") + 
    theme_classic() + 
    theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
  
  long <- ggplot(ex) + 
    geom_line(aes(x = as.Date(date), y = st_coordinates(ex)[,1]), linewidth = 0.9) + 
    scale_x_date(limits = c(start_date, end_date), date_breaks = "months", date_labels = "%m") +  
    labs(y = "Longitude (UTM_E)", x = "Month") + 
    theme_classic() + 
    theme(legend.position = "none", axis.text.y = element_blank())
  
  # Arrange and print all plots on one page
  grid.arrange(NSD + ggtitle(i), elev, speed, lat, long, nrow = 5)
}

# Close the PDF device
dev.off()

write.csv(deer, "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/Migration Flashcards/AllMigrations_2014t2023_digitizednotes.csv", row.names = F)

#----------------------------#
# Fall ####



# der_est <- calculate.glla(time = as.vector(as.POSIXct(ex$date)),
#                                  signal = ex$R2n,
#                                  embedding = 288,
#                                  n = 1)
# 
# der_est <- data.frame(time = der_est$dtime, dxdy = der_est$dsignal[,2], id = "255_2022")
# 
# ggplot(der_est)

# d <- ggplot(data=der_est) + labs(x = "Time", title = "dx/dy calculated over 2 days") + geom_point(aes(time, dxdy, color = id)) + scale_color_manual(values = c("blue","orange")) + geom_hline(yintercept = 0, linetype = "dashed") + theme_classic()
# d

# y <- ggplotly(d)
# 
# 
# p <- ggplot(data = ATS_nsd) + geom_line(aes(x=date,y = R2n, color = id), size = 1.5) + scale_color_manual(values = c("blue","orange")) + theme_classic()
# p
# 
# x <- ggplotly(p)
# 
# x
# y
# 
# 
# saveWidget(widget = x, file = "C:/Users/lwilde2/Downloads/RDH_2022_ATSHighResCollars_NSD.html")
# saveWidget(widget = y, file = "C:/Users/lwilde2/Downloads/RDH_2022_ATSHighResCollars_dxdy_NSD.html")

