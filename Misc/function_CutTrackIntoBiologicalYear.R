#Author: Luke Wilde
#Date: 10/14/2021
#Purpose: function to cut based on biological year (CAUTION: only good for 1 cut point such as July 1)

cut_bio_yr <- function(track = data, day = "POSIXct", yr = "Year", cut_day="07-01"){
  
  library(dplyr)
  
  y_var <- track %>% select(paste(yr)); names(y_var) <- "y"; y_var <- as.numeric(y_var$y)
  d_var <- track %>% select(paste(day)); names(d_var) <- "d"; d_var <- as.POSIXct(d_var$d, "%Y-%m-%d %H:%M:%S")
  
  #check input data class
  if(!(inherits(d_var, "POSIXct"))) stop("day needs to be in POSIXct, and column name in quotations. For help, look at as.POSIXct")
  if(!(inherits(cut_day, "character"))) stop("cut day must be specified in MM-DD format, with quotations")
  
  #transform obs date to Julian
  j <- as.numeric(format(as.Date(d_var, "%Y-%m-%d"),"%j")) 

  #transform cut day to Julian
  lst <- list(NA, nrow(track))
  for(i in 1:nrow(track)){
  lst[i] <- paste(y_var[i],"-", cut_day)}
  cut_day_yr <- as.vector(do.call(rbind, lst))
  cut_day_yr<-gsub(" - ","-",cut_day_yr)
  cut_day_fmt <- as.numeric(format(as.Date(cut_day_yr, "%Y-%m-%d"),"%j"))
  
  # create indicator variable
  bio_yr_indic <- list(NA, nrow(track))
  for(i in 1:nrow(track)){
  bio_yr_indic[i] <- if(j[i] >= cut_day_fmt[i]) {1} else {0}}
  track$bio_yr_indic <- as.vector(do.call(rbind, bio_yr_indic))
  
  
  #creat rule to tell R to count the 0 of this year and the 1 of last year as the same value
  bio_yr <- list(NA, nrow(track))
  for(i in 1:nrow(track)){
  bio_yr[i] <- if(track$bio_yr_indic[i] == 0) {y_var[i] - 1} else {y_var[i]}}
  track$bio_yr <- as.vector(do.call(rbind, bio_yr))
  
  data_with_bio_yr <<- track
  
  return(data_with_bio_yr)
  
}

