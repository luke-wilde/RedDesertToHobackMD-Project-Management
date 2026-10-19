#prepare env

library(XML)
library(sf)
library(tidyverse)

load("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_CleanData.RData")

#choose deer
deer <- rdh_data.sf %>% filter(AID == 242)


for(j in unique(deer$Year)){
# j = 2019
  #prepare
  df <- deer %>% mutate(Date = date(POSIXct), Time = parse_time(str_sub(POSIXct, 12,18))) %>% select(AID, id_yr, Year, Month, Day, Date, Time, POSIXct) %>% st_transform(., "EPSG:4326") %>% mutate(across(c(Date, Time, POSIXct), as.character))
  
  df
  
  
  df <- df %>%
    mutate(Timestamp_ISO8601 = strftime(POSIXct, format = "%Y-%m-%dT%H:%M:%SZ", tz = "MST"))
  
  df <- df %>% filter(Year == j)
  
  # Initialize KML structure
  doc <- newXMLDoc()
  kml <- newXMLNode("kml", doc = doc)
  document <- newXMLNode("Document", parent = kml)
  
# Iterate through features
for (i in 1:nrow(df)) {
  placemark <- newXMLNode("Placemark", parent = document)
  newXMLNode("name", df$Date[i], parent = placemark)
  
  # Correctly format coordinates
  coords <- paste(st_coordinates(df$geometry[i, ])[1], st_coordinates(df$geometry[i, ])[2], sep = ",")
  point <- newXMLNode("Point", parent = placemark)
  newXMLNode("coordinates", coords, parent = point)
  
  # Add TimeStamp
  newXMLNode("TimeStamp", newXMLNode("when", df$Timestamp_ISO8601[i]), parent = placemark)
}#i



# Save the KML file -- BE SURE TO CHANGE NAME AND ERASE EXISTING FILES, WILL NOT OVERWRITE CORRECTLY
saveXML(doc, file = paste0("C:/Users/lwilde2/Downloads/Deer242_",j,".kml"))

} #j
