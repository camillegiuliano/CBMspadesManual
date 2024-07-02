## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE,
  eval = FALSE
)

## ----install-spatial----------------------------------------------------------
#  ## (re)install spatial packages from source
#  install.packages("sf", type = "source",
#                   configure.args = "--with-proj-lib=$(brew --prefix)/lib/")
#  
#  install.packages("terra", type = "source")
#  
#  ## legacy spatial packages may still be required until fully retired Oct 2023
#  install.packages("rgdal", type = "source",
#                   configure.args = c("--with-proj-lib=$(brew --prefix)/lib/",
#                                      "--with-proj-include=$(brew --prefix)/include/"))
#  
#  install.packages("rgeos", type = "source")
#  
#  ## confirm the GDAL, GEOS, PROJ versions being used
#  library(sf)
#  library(rgdal)

## ----install-igraph-----------------------------------------------------------
#  install.packages("igraph", type = "source")

