#---------------------#
# Prepare environment ####

#---------------------#
## Load libs ####
library(tidyverse)
library(readxl)

#---------------------#
## Load datasets and capture locations ####

setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/Fawn Recruitment/")

# 1. Create a function to process each Excel file
process_FFR_data <- function(file_path) {
  #file_path <- "Annual Capture Data/RD2H_MD_March2024_CaptureData.xlsx"
  # Read the Excel file
  data <- read_excel(file_path)
  
  # Define required columns
  required_columns <- c(
    "Date", "MigrationStrategy", "DeerID",  "YoungAtHeel?","YoungAtHeel", "NumYoung", "NumDoes", "NumFawns", "RecruitmentConfidence","Lat_DD","Long_DD","GroupSize","NumFawns","NumBucks","NumDoes","NumUNK","Notes",'YoungAtHeel (1=Yes, 0=No)','NumOfOffspring (with Collar)')
  
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
      contains("Date"),
      contains("MigrationStrategy"),
      contains("DeerID"),
      contains("YoungAtHeel?"),
      contains("YoungAtHeel"),
      contains("NumYoung"),
      contains("NumDoes"),
      contains("NumFawns"),
      contains("RecruitmentConfidence"),
      contains("Lat_DD"),
      contains("Long_DD"),
      contains("GroupSize"),
      contains("NumFawns"),
      contains("NumBucks"),
      contains("NumDoes"),
      contains("NumUNK"),
      contains("Notes"),
      contains('YoungAtHeel (1=Yes, 0=No)'),
      contains('NumOfOffspring (with Collar)')
    )
  
  # Convert all columns to character type
  data_subset <- data_subset %>%
    mutate(across(everything(), as.character))
  return(data_subset)
}

# 2. Get all file paths
file_paths <- list.files(
  path = getwd(), 
  pattern = "RD2H_MD_.+_FawnRecruitment\\.xlsx$",
  full.names = TRUE
)



# 3. Process all files and combine into a single dataset
all_FFR_data <- map_dfr(file_paths, process_FFR_data) %>% rename(AID = DeerID) %>% mutate(Date = date(Date), across(c("Lat_DD", "Long_DD"),as.numeric)) ; all_FFR_data %>% head()

all_FFR_data <- all_FFR_data %>% mutate(YoungAH = ifelse(!is.na(`YoungAtHeel?`),ifelse(`YoungAtHeel?` == "N",0,1), ifelse(!is.na(`YoungAtHeel`),ifelse(`YoungAtHeel` == "N",0,1), `YoungAtHeel (1=Yes, 0=No)`)), NumAH = ifelse(is.na(NumYoung),`NumOfOffspring (with Collar)`,NumYoung))


write.csv(all_FFR_data, "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/0_MainDataSets/FawnRecruitment_Rolling.csv")
