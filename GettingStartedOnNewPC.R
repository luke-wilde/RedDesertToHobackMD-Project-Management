startup <- function(packages = c("sf","tidyverse","lubridate","purrr","readxl"), reset.env = F){
  
  options(scipen = 999)
  # 
  # #devtools::install_github("jmerkle1/MerkleLabGIS")
  # #install.packages("C:/Users/lwilde2/Downloads/MoveTools_0.0.0.9001.tar.gz", repos = NULL, type = "source")
  
  #devtools::install_github("stan-dev/rstanarm",ref = "feature/survival", build_vignettes = FALSE)
  
  # List of packages
  pac <- c(packages,"devtools","parallel")
  
  
  # Install missing packages
  installed_packages <- installed.packages()
  for (pkg in pac) {
    if (!(pkg %in% installed_packages[, "Package"])) {
      install.packages(pkg)
    }
  }
  
  # Load all packages
  lapply(pac, library, character.only = T)
  
  
  #install_github("ropensci/prism")
  
  if(reset.env == T){rm(list = ls()); print("Welcome. Ready to code?! ;)")} else { print("Welcome. Ready to code?! ;)")}
  
  #correct select so that dplyr is preferred
  
  assign("select",    dplyr::select,    envir = .GlobalEnv)
  assign("summarise", dplyr::summarise, envir = .GlobalEnv)
  assign("rename",    dplyr::rename,    envir = .GlobalEnv)
  assign("filter",    dplyr::filter,    envir = .GlobalEnv)
  
  nc <- parallel::detectCores() - 1
  assign("nc", nc, envir = .GlobalEnv)
  
  print(paste("Cores on machine:", nc))
}

startup(reset.env = F, packages = c("sf","tidyverse","mapview","lwgeom","igraph","stringr","readxl","purrr","MerkleLabGIS","MoveTools","sfnetworks","tidygraph","ggthemes","showtext", "broom", "boot","ggh4x","units",
                                    #optional
                                    "leastcostpath","terra","amt","future","furrr","glmmTMB","MuMIn","performance","car", "DHARMa","ggeffects","mgcv","splines","ggsignif","ggpubr","lme4", "emmeans","ggridges","BaBA","moveHMM","momentuHMM","ggtext","showtext","ggExtra","ggside","concaveman","rstatix"))


devtools::install_github("jmerkle1/MerkleLab-Code-Repository", subdir = "MoveTools")
devtools::install_github("jmerkle1/MerkleLabGIS")
