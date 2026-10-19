#Author: Luke Wilde
#Date: 10/13/2021
#code to automatically download Lotek data 

library(devtools)
library(collar)
#devtools::install_github("huh/collar")

user = "aortega5"
pass = "WyomingElk24!"

#lotek_refresh_token(force_refresh = FALSE) #Attempts to acquire a new access token when current one expires.
lotek_login(usr=user, pwd=pass)

devices <- fetch_lotek_devices()
alerts <- fetch_lotek_alerts()
positions <- fetch_lotek_positions()

str(positions)

#reformat
positions <- data.frame("Device ID" = positions$DeviceID, "Date & Time [GMT]" = positions$UploadTimeStamp, "Date & Time [Local]" = positions$UploadTimeStamp, positions$Latitude, positions$Longitude, "Altitude" = positions$Altitude, "Fix Status" = positions$RxStatus, "DOP" = positions$PDOP, "Temp [C]" = positions$Temperature, "Main [V]" = positions$MainV, "Back [V]" = positions$BkUpV)

lotek_logout()


#write to file
write.csv(positions, "D:/RD2H_GPSCollarData/RD2H_LOTEK_Data/AllLOTEKFromWebService.CSV")

write.csv(alerts, "D:/RD2H_GPSCollarData/Lotek Alerts/AllLOTEKFromWebService_Alerts.CSV")
