# code to estimate days remaining on a collar
# written by Luke Wilde
# date = 02/03/2023




library(gdata)
library(tidyverse)
#call the function'

source("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/RCode/other RCode/CollarCode/CalculateLifeEstimatesOnly_function.R")

#csv.name = the name of the file you are calling
#wd = is the working directory from which you are pulling csv.name
#ATTN: Default is Telonics settings and battery estimate calculations




check = c("706088A", "718860A", "718876A", "723311A", "723308A", "723296A", "723306A", "723313A", "715851A", "723297A", "723312A", "727875A", "728184A", "728200A", "728198A", "728187A", "728216A", "728201A", "728185A", "727555A", "728215A", "727571A", "728199A", "728220A", "728202A", "727575A", "728204A", "728218A", "727593A", "727570A", "728213A", "728227A", "728203A", "727820A", "727592A", "727822A", "728221A", "728197A", "727825A", "728222A", "727821A", "727550A", "727899A", "728228A", "727915A", "728214A", "728225A", "736687A", "736688A", "736690A", "736691A", "736692A", "736696A", "736676A", "736695A", "736678A", "736672A", "736673A", "736675A", "736669A", "736679A", "736689A", "736668A", "736664A", "736685A", "736680A", "736663A", "752086A", "745354A", "745352A", "745351A", "744583A", "745353A", "745339A", "745337A", "745340A", "745336A", "745334A", "745361A", "745359A", "745358A", "745357A", "744566A", "745338A", "745348A", "745345A", "745356A", "745350A", "745349A", "745342A", "744567A", "745363A", "752078A", "752072A", "752061A", "752062A", "719038A", "751842A", "736682A", "728219A", "752075A", "752079A", "752063A", "736683A", "752076A", "736686A", "727565A", "736681A", "736662A", "723299A", "727824A", "736674A", "727554A", "736666A", "752081A", "752084A","719040A")


out <- data.frame(collar = NA, n = NA, n.1 = NA)[0,]
wd = "C:/Users/lwilde2/Desktop/RDH Database/RD2H_GPSCollarData/RD2H_Telonics_Data/IndividualRawGlobalstarDataFromTDC/"

for(i in 1:length(check)){
  #i=10
  tryCatch({
  tab <- read.csv(paste(wd, check[i], " Complete",".csv",sep=""), skip = 22)
  
  max = 1440
  conservative = (365*3+90)
  
  require(lubridate)
  require(tidyverse)
  require(stringr)
  num_Morts <- tab %>% filter(Mortality %in% c("Yes", "Yes*")) %>% mutate(GPS.Fix.Time = as.POSIXct(GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S"), day = date(GPS.Fix.Time)) %>% summarise(n = n_distinct(day)) * 1.25
  num_Active <- tab %>% filter(!Mortality %in% c("Yes", "Yes*")) %>% mutate(GPS.Fix.Time = as.POSIXct(GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S"), day = date(GPS.Fix.Time)) %>% summarise(n = n_distinct(day))
  num_Active_tries <- tab %>% filter(GPS.Fix.Attempt %in% c("Failed", "Unresolved QFP")) %>% mutate(GPS.Fix.Time = as.POSIXct(GPS.Fix.Time, format = "%Y.%m.%d %H:%M:%S"), day = date(GPS.Fix.Time)) %>% summarise(n = n_distinct(day))
  
  # df$collar[i] <- str_sub(csv.name,1,7)
  # df$conservative[i] <- (conservative - (num_Morts+num_Active_tries+num_Active))$n
  # df$max[i] <- (max - (num_Morts+num_Active_tries+num_Active))$n
  

  
  collar <- check[i]
  cons <- (conservative - (num_Morts+num_Active_tries+num_Active))
  max <- (max - (num_Morts+num_Active_tries+num_Active))
  out <- rbind(out, data.frame(collar, cons, max))
  rm(collar, cons, max)
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

out$consdat <- out$n + Sys.Date()
out$avgdat <- out$n.1 + Sys.Date()

out

t <- as.vector(paste(month(as.Date(out$consdat)),day(as.Date(out$consdat)), year(as.Date(out$consdat)),sep="/"))
tt <- as.vector(paste(month(as.Date(out$avgdat)),day(as.Date(out$avgdat)), year(as.Date(out$avgdat)),sep="/"))

View(data.frame(t, tt))
