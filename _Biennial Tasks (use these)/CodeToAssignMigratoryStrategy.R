#Code to determine the Migratory Strategy of TBD animals

setwd("C:/Users/lwilde2/Documents/RDH Database/RD2H_GPSCollarData/")

#=====================================================#


rm(list=ls())

#load libraries

library(devtools)
#devtools::install_github("jmerkle1/MerkleLab-Code-Repository", subdir="MoveTools")
library(MoveTools)
library(sf)
library(tidyverse)
library(readxl)
library(mapview)
#library(maptools)
library(parallel)

nc <- detectCores()-1



#----------------------------------------------------------------#
#### Settings for fetching locations on Telonics Collars ONLY ####
#----------------------------------------------------------------#

fldr_out = "C:\\Users\\lwilde2\\Desktop\\TelonicsTemp"## Where you want the temp files stored, this will be emptied after the data are brought into the R environment
days = days #the number of days worth of daily locations you want (will only affec thow long the "tail" following points is; think tadpoles)
username = "AnnaOrtega" #log in info for RDH
password = "n6rW2QSw" #log in info for RDH
TDC_path= "C:\\Program Files (x86)\\Telonics\\Data Converter\\TDC.exe" #the default area where apps are saved, go to https://www.telonics.com/software/tdc.php to download the TDC and work from there!

start = NULL#as.POSIXct(Sys.time() - (3 * 24 * 3600))

#now load the gps files from Telonics

if(file.exists(fldr_out)){print(length(list.files(fldr_out))==0)} else{dir.create(file.path(fldr_out))} # should be [1] TRUE or no output

fixes <- MoveTools::fetch_telonics_gstar_positions( username = username,password = password,
                                                    fldr_out = fldr_out,  
                                                    TDC_path = TDC_path,
                                                    keep.reports = FALSE,   # if FALSE, will remove all temp files at the end
                                                    start_date = start)  # if you put NULL here, you'll get ALL data, if you change the first number to 1, you get only todays data

#fixes will save to a directory, and you can store that location below as the dir.tdc object. Or you can integrate in the case that Jerod's code loads directly into the R environment

check <- c("754415A",
"754391A",
"752064A",
"752060A",
"745335A",
"736684A",
"754408A",
"728181A",
"728183A",
"728186A",
"752069A",
"752074A",
"752071A",
"745360A",
"752057A",
"752077A",
"752080A",
"745355A",
"754392A",
"754393A",
"751841A",
"752073A",
"745333A")


capd <- date("2025-12-7")
         
gps <- fixes |> filter(CollarSerialNumber %in% check) |> mutate(dt_ = ymd_hms(GPS.Fix.Time)) |> filter(dt_ > capd) |> select(CollarSerialNumber, dt_)

lin <- as(gps, "Spatial") |> st_as_sf(.) |> Points2Lines( date_name = "dt_", id_name = "CollarSerialNumber", byid = T, no_cores = nc  )

gps |> mapview(zcol = "CollarSerialNumber", burst = TRUE)


