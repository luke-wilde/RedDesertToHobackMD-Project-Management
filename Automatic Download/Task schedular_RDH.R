# This code is from the 'taskscheduleR' package and utilizes the task schedular built into windows systems. 
# It is intended to run an R script automatically according to the set parameters. The starttime and startdate 
# should be set to when teh code will first be exucuted. If the schedule is set to Daily then is will run at the
# given start time on a daily bases.

library(taskscheduleR)

# File path to the R script to run
rscript = "D:/RedDesertToHobackMuleDeerResearchProject/RCode/Automatic Download/AutoDownloadingRD2HLastLocations.R"

# Task schedular
taskscheduler_create(
  taskname = basename(rscript),
  rscript,
  schedule = "DAILY",
    #c("ONCE", "MONTHLY", "WEEKLY", "DAILY", "HOURLY", "MINUTE", "ONLOGON",
     #          "ONIDLE"),
  #starttime = format(Sys.time() + 62 , "%H:%M"),
  starttime = "00:15",
  #startdate = format(Sys.Date()+1 , "%m/%d/%Y"),
  startdate = "10/10/2021",
  days = "*",
    #c("*", "MON", "TUE", "WED", "THU", "FRI", "SAT", "SUN", 1:31),
  months = "*",
    #c("*", "JAN", "FEB", "MAR", "APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT",
     #        "NOV", "DEC"),
  idletime = 60L,
  Rexe = file.path(Sys.getenv("R_HOME"), "bin", "Rscript.exe"),
  rscript_args = "",
  schtasks_extra = "",
  debug = FALSE
)


# Delete the task if no longer wanted
taskscheduler_stop(taskname = basename(rscript))
taskscheduler_delete(taskname = basename(rscript))
