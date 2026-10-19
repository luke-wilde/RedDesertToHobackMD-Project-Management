#script to process migration table for input to BBMM

library(dplyr)

timing21 <- read.csv("G:/My Drive/RDH Data Sharing/Migrations/migtime_2021.csv")

report <- read.csv("C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Paperwork/Chapter33Permit/2021/AllFrequencies_2021_2.csv")

head(timing21); head(report)

names(report)[4] <- "AID"

mig21 <- timing21 %>% mutate("Year" = nsdYear, "AID" = newUid,		"SpringMS" = as.POSIXct(startSpring, format = "%m/%d/%Y"),	"SpringMSDate" = as.Date(startSpring, format = "%m/%d/%Y"),	"SpringMSTime" = as.character("00:00:00"),	"SpringME" = as.POSIXct(endSpring, format = "%m/%d/%Y"),	"SpringMEDate" = as.Date(endSpring, format = "%m/%d/%Y"),	"SpringMETime"= as.character("00:00:00"),"FallMS" = as.POSIXct(startFall, format = "%m/%d/%Y"),	"FallMSDate" = as.Date(startFall, format = "%m/%d/%Y"),	"FallMSTime" = as.character("00:00:00"),	"FallME" = as.POSIXct(endFall, format = "%m/%d/%Y"),	"FallMEDate" = as.Date(endFall, format = "%m/%d/%Y"),	"FallMETime"= as.character("00:00:00")
)

head(mig21)

mig_21_info <- mig21 %>%  left_join(report[,c("AID","Comments", "Potential.Range.of.Movement..Miles.")],by = "AID")


str(mig_21_info)
unique(mig_21_info$MigratoryStrategy)


names(mig_21_info)[c(29,30)] <- c("Notes","MigratoryStrategy")

mig_21_info$MigratoryStrategy <- recode_factor(as.vector(mig_21_info$MigratoryStrategy), "90-150" = "Long", "30-90" = "Medium", "<30" = "Short", .missing = "Resident")


write.csv(mig_21_info, "C:/Users/lwilde2/Desktop/RDH Database/RedDesertToHobackMuleDeerResearchProject/Migrations/AllSpringFallMigrationTiming_2021.csv")
