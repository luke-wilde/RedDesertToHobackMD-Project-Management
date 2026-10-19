ggpreview <- function(..., device = "png", cairo = FALSE) {
  fname <- tempfile(fileext = paste0(".", device))
  if (cairo & device == "pdf") {
    ggplot2::ggsave(filename = fname, device = cairo_pdf, ...)
  } else if (cairo & device == "png") {
    ggplot2::ggsave(filename = fname, device = device, type = "cairo", ...)
  } else {
    ggplot2::ggsave(filename = fname, device = device, ...)
  }
  system2("open", fname)
  invisible(NULL)
}

prepare_environment <- function(packages) {
  # Install missing packages
  installed_packages <- installed.packages()
  for (pkg in packages) {
    if (!(pkg %in% installed_packages[, "Package"])) {
      install.packages(pkg, dependencies = T)
    }
  }
  
  # Load all packages
  print("Loaded these packages and dependencies:")
  print(lapply(packages, library, character.only = TRUE)[[1]])

  library(conflicted)
  library(dplyr)
  library(MASS)  # Example package that masks `select()`
  
  # Prefer dplyr functions over conflicting ones
  conflict_prefer("select", "dplyr")
  conflict_prefer("filter", "dplyr")
  conflict_prefer("summarise", "dplyr")
  conflict_prefer("rename", "dplyr")
  
  detach("package:conflicted", unload = TRUE)
  
  # Set up parallel processing
  library(parallel)
  nc <<- detectCores() - 1
  
  # Set options
  options(scipen = 999)
  
  
  
}
