#install.packages(c("ggspatial", "rosm", "prettymapr"))
#install.packages(c( "maptiles", "tidyterra"))

library(tidyverse)
library(sf)
library(MoveTools)
library(ggspatial)
library(rosm)
library(prettymapr)
library(maptiles)
library(tidyterra)


fire <- st_read("C:/Users/lwilde2/Downloads/Re__Mule_Deer_locations_-_Lander_area/Weiser.shp")
load("~/RDH Database/RD2H_GPSCollarData/RD2H_CleanData.RData")
deer <- rdh_data.sf

inside <- st_filter( deer, fire |> st_transform(crs = st_crs(deer)) |> st_buffer(10000), join = st_within)
id <- inside |> distinct(id_yr)
id

m <- deer |> filter(id_yr %in% id$id_yr)

mL <- m |> as( "Spatial") |> st_as_sf() |> Points2Lines(date_name = "POSIXct", id_name = "id_yr")

mL_in <- st_intersection(mL, st_transform(fire, crs = st_crs(mL)))

#mapping ---------------


# 1. Load your polygon & points (or create a sample polygon in Wyoming to test)
# Replace this block with: study_poly <- st_read("your_polygon.shp")
study_poly <- fire |> st_transform(crs = 4326)

sample_pts <- mL_in |>  st_transform(crs = 4326)
sample_pts1 <- mL |>  st_transform(crs = 4326)

# 2. Reproject to Web Mercator (EPSG:3857) for crisp, unwarped map tiles
poly_3857 <- st_transform(study_poly, 3857)
pts_3857  <- st_transform(sample_pts, 3857)
pts1_3857 <- st_transform(sample_pts1, 3857)

# Create a padded view extent around your polygon
map_bbox <- st_bbox(poly_3857) + c(-18000, -18000, 18000, 18000)

# 3. Fetch the exact Esri World Topographic basemap
topo_tiles <- get_tiles(map_bbox, provider = "Esri.WorldTopoMap", zoom = 11, crop = TRUE)

# 4. Plot with the exact teal-green polygon & plum-magenta point aesthetics
ggplot() +
  # Warm topographic hillshade basemap
  geom_spatraster_rgb(data = topo_tiles) +
  
  # Soft seafoam-teal translucent fill + wider soft inner glow
  geom_sf(
    data = poly_3857,
    fill = "red3",
    alpha = 0.18,
    color = "red3",
    linewidth = 2.4
  ) +
  
  # Crisp, deep emerald-teal outer boundary line
  geom_sf(
    data = poly_3857,
    fill = NA,
    color = "red4",
    linewidth = 1.0
  ) +
  
 
  geom_sf(
    data = pts1_3857,
    shape = 21,
    color = "grey56",
    stroke = 0.4,
    size = 0.6
  ) +
  
  geom_sf(
    data = pts_3857,
    shape = 21,
    color = "purple3",
    stroke = 0.4,
    size = 2.6
  ) +
  

  
  # Clean tick-style scale bar in bottom-right
  annotation_scale(
    location = "br",
    width_hint = 0.25,
    style = "ticks",
    line_col = "grey30",
    text_col = "grey20"
  ) +
  
  # Lock extent and apply thin gray frame
  coord_sf(
    xlim = c(map_bbox["xmin"], map_bbox["xmax"]),
    ylim = c(map_bbox["ymin"], map_bbox["ymax"]),
    expand = FALSE
  ) +
  theme_void() +
  theme(
    panel.border = element_rect(color = "grey45", fill = NA, linewidth = 0.8)
  )

st_write(mL, dsn = "C:/Users/lwilde2/Downloads/RedDesertDeer_Weiser.shp")
