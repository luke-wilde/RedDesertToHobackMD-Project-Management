library(tidyverse)

poss <- seq(160, 162.0, by = 0.01)

taken <- read.csv("H:/Other computers/My Laptop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/0_MainDataSets/CheckForAvailCollFreq_Rolling.csv")


avail <- setdiff(poss, taken$Freq)

choice <-  avail %>% as.tibble() %>% sample_n(12)

choice %>% as.data.frame()
