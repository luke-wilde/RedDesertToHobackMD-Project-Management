# Code to sort GPS data by the doe fawn pairs we have
# Luke Wilde
# 6 JAN 2024


#--------------------------#
# set up env and load data ####


rm(list= ls())

# List of packages
packages <- c(
  "tidyverse", "sf", "mapview", "lubridate","terra","devtools","MoveTools", "readxl"
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

#correct select so that dplyr is preferred
select <- dplyr::select
summarise <- dplyr::summarise
rename <- dplyr::rename

#load gps data
load("C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_CleanData.RData")

#load in doe-fawn information
doefawn <- read_xlsx(path = "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/0_MainDataSets/RDH_DoeFawnPairs_Rolling.xlsx")

doefawn

#--------------------------#
# Seperate them into those captured as fawns or yearlings ####

early <- doefawn %>% filter(`Age of First Migration Together` %in% c("Yearling", "Fawn")) %>% select(!c(`Father AID`, `Age of First Migration Together`))

early_long <- early %>% gather(key = "Role", value = "AID", `Mother AID`, `Fawn AID`) %>% mutate(Role = ifelse(Role == "Mother AID", "Mother", "Fawn"))


gps <- rdh_data.sf %>% select(Mgtry, AID, POSIXct, Date, Time, UTM_N, UTM_E, id_yr, AvgFixRate) %>% right_join(., early_long %>% select(Role, Family, AID, `Fawn Sex`, `Fawn Birth Year`,  `First spring migration together`) %>% mutate(AID = as.integer(AID)), by = "AID", relationship = "many-to-many")

gps %>% as.data.frame() %>% head()
names(gps)



#--------------------------#
# Save to file ####

st_write(gps, "C:/Users/lwilde2/Downloads/RDH_DoeFawnPairs_FawnOrYearlingCapOnly_6JANUARY2025.shp", append = F)


#--------------------------#
# ####
#--------------------------#
# ####