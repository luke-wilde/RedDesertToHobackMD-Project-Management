#---------------------#
# Prepare environment ####

#---------------------#
## Load libs ####
library(tidyverse)
library(readxl)

#---------------------#
## Load datasets and capture locations ####

setwd("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/")

# 1. Create a function to process each Excel file
process_capture_data <- function(file_path) {
  #file_path <- "Annual Capture Data/RD2H_MD_March2024_CaptureData.xlsx"
  # Read the Excel file
  data <- read_excel(file_path)
  
  # Define required columns
  required_columns <- c(
    "DeerID","LabAccess","Sex", "CapDate", "ChaseTime", "ProcessStartTime", "ReleaseTime", 
    "CapNum", "NewCap", "CapUTM_E", "CapUTM_N", "StagingArea", "StageUTM_E", 
    "StageUTM_N", "BodyLength_cm", "BodyGirth_cm", "MetatarsusLength_cm", 
    "BodyCond", "Lactation", "NetWeight_kg", "ScaledMaxFat", "newLIVINDEX", 
    "scaledIFBFat", "IFBodyMass", "IFFFLeanMass", "FetusNum", "EyeDiam1_mm", 
    "EyeDiam2_mm", "Vocal", "Activity", "Respiration_bpm"  )
  
  # Find missing columns
  missing_columns <- setdiff(required_columns, colnames(data))
  
  # Log missing columns without stopping
  if (length(missing_columns) > 0) {
    message("File: ", file_path, " is missing columns: ", paste(missing_columns, collapse = ", "))
  }
  
  # Extract only the required columns
  # Adjust column names if they're different in your files
  data_subset <- data %>% 
    dplyr::select(
      contains("DeerID"), 
      contains("LabAccess"),
      contains("Sex"),
      contains("CapDate"),
      contains("ChaseTime"),
      contains("ProcessStartTime"), 
      contains("ReleaseTime"), 
      contains("CapNum"),
      contains("NewCap"),
      contains("CapUTM_E"), 
      contains("CapUTM_N"), 
      contains("StagingArea"),
      contains("StageUTM_E"), 
      contains("StageUTM_N"),
      contains("BodyLength_cm"),
      contains("BodyGirth_cm"),
      contains("MetatarsusLength_cm"),
      contains("BodyCond"),
      contains("Lactation"),
      contains("NetWeight_kg"),
      contains("ScaledMaxFat"),
      contains("newLIVINDEX"),
      contains("scaledIFBFat"),
      contains("IFBodyMass"),
      contains("IFFFLeanMass"),
      contains("FetusNum"),
      contains("EyeDiam1_mm"),
      contains("EyeDiam2_mm"),
      contains("Vocal"),
      contains("Activity"),
      contains("Respiration_bpm"),
      contains("Injuries"),
      contains("Other Release Notes"),
      contains("Notes"))
  
  # Convert all columns to character type
  data_subset <- data_subset %>%
    mutate(across(everything(), as.character))
  
  # Extract year and month from filename for tracking purposes
  file_name <- basename(file_path)
  if (grepl("Dec", file_name)) {
    month <- "Dec"
  } else {
    month <- "Mar" 
  }
  year <- str_extract(file_name, "\\d{4}")
  
  # Add capture period identifier
  data_subset$capture_period <- paste0(month, year)
  
  return(data_subset)
}

# 2. Get all file paths
file_paths <- list.files(
  path = "Annual Capture Data", 
  pattern = "RD2H_MD_.+_CaptureData\\.xlsx$",
  full.names = TRUE
)



# 3. Process all files and combine into a single dataset
all_capture_data <- map_dfr(file_paths, process_capture_data) %>% rename(AID = DeerID) %>% mutate(ProcessStartTime = hm(paste(hour(ProcessStartTime), minute(ProcessStartTime), sep=":")), ReleaseTime = hm(paste(hour(ReleaseTime ), minute(ReleaseTime), sep=":"))); all_capture_data %>% head()

all_capture_data$ProcessStartTime <- format(parse_date_time(all_capture_data$ProcessStartTime, 'HMS', tz = "US/Mountain"), '%I:%M:%S %p')
all_capture_data$ReleaseTime <- format(parse_date_time(all_capture_data$ReleaseTime, 'HMS', tz = "US/Mountain"), '%I:%M:%S %p')

write.csv(all_capture_data, "G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/0_MainDataSets/RDH_AllCaptureData_Rolling.csv")

#---------------------#
# for miranda ####
library(sf)

#load in gpx files
mar23_1 <- st_read("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/March_2023/WY Superior 1132023.GPX")
mar23_2 <- st_read("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/March_2023/WY Superior MD 1032023.GPX")
mar23_3 <- st_read("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/March_2023/WY Superior doefawn 632023.GPX")
mar23_4 <- st_read("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/March_2023/WY Superior doefawns 532023.GPX")

mar23 <- bind_rows(mar23_1, mar23_2, mar23_3, mar23_4) %>% st_transform(., crs="EPSG:32612")
rm(mar23_1, mar23_2, mar23_3, mar23_4)

dec23_1 <- st_read("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/December_2023/WY Superior 7122023.GPX")
dec23_2<- st_read("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/December_2023/WY Superior 8122023.GPX")
dec23_3<- st_read("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/December_2023/WY Superior 9122023.GPX")

dec23 <- bind_rows(dec23_1, dec23_2, dec23_3) %>% st_transform(., crs="EPSG:32612")
rm(dec23_1, dec23_2, dec23_3)

mar24_1 <- st_read("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/March_2024/WY Red Desert 1332024.GPX")
mar24_2 <- st_read("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/March_2024/WY Red Desert MD 1232024.GPX")

mar24 <- bind_rows(mar24_1, mar24_2) %>% st_transform(., crs="EPSG:32612") %>% mutate(name = str_sub(name, 2,4))
rm(mar24_1, mar24_2)

dec24_1 <- st_read("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/December_2024/WY Red Desert 9122024.GPX")
dec24_2 <- st_read("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/December_2024/WY Red desert MD 7122024.GPX")
dec24_3 <- st_read("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/December_2024/WY Red Desert MD 8122024.GPX")

dec24 <- bind_rows(dec24_1,dec24_2,dec24_3) %>% st_transform(., crs="EPSG:32612")
rm(dec24_1,dec24_2,dec24_3)

mar25_1 <- st_read("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/March_2025/WY Red Desert MD 332025.GPX")
mar25_2 <- st_read("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/March_2025/WY Red Desert MD 432025.GPX") 

mar25 <- bind_rows(mar25_1, mar25_2) %>% st_transform(., crs="EPSG:32612") %>% mutate(name = str_sub(name, 2,4))
rm(mar25_1, mar25_2)

select <- dplyr::select

RDH_MSshare <- all_capture_data %>% select(AID, LabAccess, Sex, CapDate, ProcessStartTime, ReleaseTime, CapNum, CapUTM_E, CapUTM_N, StagingArea, StageUTM_E, StageUTM_N, OtherReleaseNotes, `Other Release Notes`, `Observations/OtherNotes`, CollarMalfunctionNotes) %>% mutate(CapEvent = paste0(month(CapDate), "_", year(CapDate)))

RDH_MSshare_year <- RDH_MSshare %>% filter(CapEvent == "3_2023")  %>% left_join(., mar23 %>% mutate(CapUTM_E = st_coordinates(.)[,1], CapUTM_N = st_coordinates(.)[,2]) %>% st_drop_geometry() %>% select(name, CapUTM_E, CapUTM_N) %>% mutate(name = as.character(name)), by = c("CapUTM_E" = "name")) %>% select(!c(CapUTM_N.x,CapUTM_E)) %>% rename(CapUTM_E = CapUTM_E.y, CapUTM_N = CapUTM_N.y) %>%
  mutate(CapUTM_E = as.character(CapUTM_E), CapUTM_N = as.character(CapUTM_N))


RDH_MSshare_filtered <- RDH_MSshare %>% filter(CapEvent != "3_2023")

RDH_MSshare <- bind_rows(RDH_MSshare_filtered, RDH_MSshare_year)

#
RDH_MSshare_year <- RDH_MSshare %>% filter(CapEvent == "12_2023")  %>% left_join(., dec23 %>% mutate(CapUTM_E = st_coordinates(.)[,1], CapUTM_N = st_coordinates(.)[,2]) %>% st_drop_geometry() %>% select(name, CapUTM_E, CapUTM_N) %>% mutate(name = as.character(name)), by = c("CapUTM_E" = "name")) %>% select(!c(CapUTM_N.x,CapUTM_E)) %>% rename(CapUTM_E = CapUTM_E.y, CapUTM_N = CapUTM_N.y) %>%
  mutate(CapUTM_E = as.character(CapUTM_E), CapUTM_N = as.character(CapUTM_N))


RDH_MSshare_filtered <- RDH_MSshare %>% filter(CapEvent != "12_2023")

RDH_MSshare <- bind_rows(RDH_MSshare_filtered, RDH_MSshare_year)

#
RDH_MSshare_year <- RDH_MSshare %>% filter(CapEvent == "12_2024")  %>% left_join(., dec24 %>% mutate(CapUTM_E = st_coordinates(.)[,1], CapUTM_N = st_coordinates(.)[,2]) %>% st_drop_geometry() %>% select(name, CapUTM_E, CapUTM_N) %>% mutate(name = as.character(name)), by = c("StageUTM_E" = "name")) %>% select(!c(CapUTM_N.x,CapUTM_E.x)) %>% rename(CapUTM_E = CapUTM_E.y, CapUTM_N = CapUTM_N.y) %>%
  mutate(CapUTM_E = as.character(CapUTM_E), CapUTM_N = as.character(CapUTM_N))


RDH_MSshare_filtered <- RDH_MSshare %>% filter(CapEvent != "12_2024")

RDH_MSshare <- bind_rows(RDH_MSshare_filtered, RDH_MSshare_year)

#
RDH_MSshare_year <- RDH_MSshare %>% filter(CapEvent == "3_2025")  %>% left_join(., mar25 %>% mutate(CapUTM_E = st_coordinates(.)[,1], CapUTM_N = st_coordinates(.)[,2]) %>% st_drop_geometry() %>% select(name, CapUTM_E, CapUTM_N) %>% mutate(name = as.character(name)), by = c("CapUTM_E" = "name")) %>% select(!c(CapUTM_N.x,CapUTM_E)) %>% rename(CapUTM_E = CapUTM_E.y, CapUTM_N = CapUTM_N.y) %>%
  mutate(CapUTM_E = as.character(CapUTM_E), CapUTM_N = as.character(CapUTM_N))


RDH_MSshare_filtered <- RDH_MSshare %>% filter(CapEvent != "3_2025")

RDH_MSshare <- bind_rows(RDH_MSshare_filtered, RDH_MSshare_year)

# need to fix
RDH_MSshare_year <- RDH_MSshare %>% filter(CapEvent == "3_2024")  %>% left_join(., mar24 %>% mutate(CapUTM_E = st_coordinates(.)[,1], CapUTM_N = st_coordinates(.)[,2]) %>% st_drop_geometry() %>% select(name, CapUTM_E, CapUTM_N) %>% mutate(name = as.character(name)), by = c("CapUTM_E" = "name")) %>% select(!c(CapUTM_N.x,CapUTM_E)) %>% rename(CapUTM_E = CapUTM_E.y, CapUTM_N = CapUTM_N.y) %>%
mutate(CapUTM_E = as.character(CapUTM_E), CapUTM_N = as.character(CapUTM_N))


RDH_MSshare_filtered <- RDH_MSshare %>% filter(CapEvent != "3_2024")

RDH_MSshare <- bind_rows(RDH_MSshare_filtered, RDH_MSshare_year)


#add in A Townsend backfill ####

D22.D23.bkfl <- read_xlsx("G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Captures/CaptureLocs/December_2023/Dec2022-Dec2023CapStageLocs_backfill.xlsx")

D22.D23.bkfl <- D22.D23.bkfl %>%
  filter(StageArea != "Field")

cap.d22d23 <- st_as_sf(D22.D23.bkfl, coords = c("CapUTM_E","CapUTM_N"), crs = "epsg:4326") %>% st_transform(., crs = "epsg:32612") %>% mutate(CapUTM_E = st_coordinates(.)[,2], CapUTM_N = st_coordinates(.)[,2]) %>% st_drop_geometry() %>% select(DeerID,Date, CapUTM_E, CapUTM_N)
stage.d22d23 <- st_as_sf(D22.D23.bkfl, coords = c("StageUTM_E","StageUTM_N"), crs = "epsg:4326") %>% st_transform(., crs = "epsg:32612") %>% mutate(StageUTM_E = st_coordinates(.)[,2], StageUTM_N = st_coordinates(.)[,2])  %>% st_drop_geometry() %>% select(DeerID,Date, StageUTM_E, StageUTM_N)

d22d23 <- cap.d22d23 %>% left_join(.,stage.d22d23) %>% mutate(CapDate = as.character(date(Date)), AID = DeerID) %>% select(-Date, -DeerID)

RDH_MSshare_fixed <- RDH_MSshare %>%
  left_join(
    d22d23 %>% select(AID, CapDate, CapUTM_E, CapUTM_N, StageUTM_E, StageUTM_N),
    by = c("AID", "CapDate"),
    suffix = c("", "_new")
  ) %>%
  mutate(
    CapUTM_E  = coalesce(as.numeric(CapUTM_E),  CapUTM_E_new),
    CapUTM_N  = coalesce(as.numeric(CapUTM_N),  CapUTM_N_new),
    StageUTM_E = coalesce(as.numeric(StageUTM_E), StageUTM_E_new),
    StageUTM_N = coalesce(as.numeric(StageUTM_N), StageUTM_N_new)
  ) %>%
  select(-ends_with("_new"))

#add in manual backfill ####
RDH_MSshare_fixed %>% group_by(CapEvent) %>% count(cap = is.na(CapUTM_E), stag = is.na(StageUTM_E)) %>% filter(cap == F & stag == F) %>%  View()




all_capturewithlocs_data <- RDH_MSshare_fixed

# write.csv(RDH_MSshare, "C:/Users/lwilde2/Datashare_MSottile_05072025.csv")
write.csv(all_capturewithlocs_data, "G:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/0_MainDataSets/RDH_AllCaptureDataWithLocs_Rolling.csv")
#---------------------#
## ####


#---------------------#
# ####

#---------------------#
## ####