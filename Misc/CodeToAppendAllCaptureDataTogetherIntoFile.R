#code to append all capture data into one large database

library(readxl)

setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/Annual Capture Data/")

file_list <- list.files()
rm(dataset)
#read_excel(file_list[3])$StageUTM_E

for (file in file_list){
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read_excel(file)
     }
  
  
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <-read_excel(file)
    
    #this converts all columns to character to allow binding
    dataset<-dplyr::bind_rows(dataset%>%mutate_all(as.character), temp_dataset%>%mutate_all(as.character))
    
    rm(temp_dataset)
  }
}

#write to database
setwd("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Data/Archived_RD2HData/AllCaptures/")
write.csv(dataset, "AllCaptureData_Append.csv")
