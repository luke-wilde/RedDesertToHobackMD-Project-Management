final.sf <- st_as_sf(final.gps, 
                     +                      coords = c("Long", "Lat"), # note x goes first
                     +                      crs = "+proj=utm +zone=12 +ellps=WGS84 +datum=WGS84 +no_defs", # projection, this is NAD83
                     +                      remove = FALSE) # don't remove lat/lon cols from dataframe

processed_sf <- final.sf |> filter(AID %in% c(807,1068,743)) |> arrange(AID, POSIXct) |> group_by(AID) |> slice_tail(n = 5) |> ungroup() |> mutate( Date = as.character(Date-1), AID = as.character(AID)) 

# 2. Split the sf object into a list of layers by Date
list_by_date <- split(processed_sf, processed_sf$Date)

mapviewOptions(basemaps = c("Esri.WorldShadedRelief", "Esri.WorldImagery", "OpenTopoMap"))


# 3. Mapview the list with zcol = "AID"
mapview(
  list_by_date, 
  zcol = "AID", 
  label = rep("AID", length(list_by_date)), 
  hide = c(T,T,T,T,F)
)


processed_sf2 <- final.sf |> filter(Date > date("2026-10-20")) |> arrange(AID, POSIXct) |> group_by(AID) |> slice_tail(n = 5) |> ungroup() |> mutate( Date = as.character(Date-1), AID = as.character(AID)) 

# 2. Split the sf object into a list of layers by Date
list_by_date2 <- split(processed_sf2, processed_sf$Date)

mapviewOptions(basemaps = c("Esri.WorldShadedRelief", "Esri.WorldImagery", "OpenTopoMap"))


# 3. Mapview the list with zcol = "AID"
mapview(
  list_by_date2, 
  zcol = "AID", 
  label = rep("AID", length(list_by_date2)), 
  hide = c(T,T,T,T,F)
)
