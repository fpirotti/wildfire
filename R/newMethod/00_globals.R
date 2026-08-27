if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(
  pbmcapply, this.path ,
  terra, hdar,arrow,
  sf, data.table, xgboost,
  parallel, openxlsx,
  reticulate,
  dplyr
)

year <- 2023
bbox <- c(10.4, 45.6, 17.7, 51.1)

outdir <- "/archivio/shared/geodati/raster/wildfire/CEfuelMap"

lat <- seq(bbox[[2]], bbox[[4]]-0.5, by = 0.5)# north bounds  51.1
lon <- seq(bbox[[1]], bbox[[3]]-0.1, by = 0.1) # east bounds 17.7

fmt_lat <- function(x){
  if(x >= 0)
    sprintf("N%02.2f", x)
  else
    sprintf("S%02.2f", abs(x))
}

fmt_lon <- function(x){
  if(x >= 0)
    sprintf("E%03.2f", x)
  else
    sprintf("W%03.2f", abs(x))
}

coords <- matrix(c(
  bbox[[1]], bbox[[2]],
  bbox[[3]], bbox[[2]],
  bbox[[3]], bbox[[4]],
  bbox[[1]], bbox[[4]],
  bbox[[1]], bbox[[2]]
), ncol = 2, byrow = TRUE)

geometry = st_sfc(st_polygon(list(coords)), crs = 4326)
# plot(geometry)

# Change to whatever root path and to where the CLCplus data tiles are and respective confidence tiles
root <- "/archivio/shared/geodati/raster"
rootPathCLC <- file.path(root,"CLMS_CLCplus_RASTER_2023/TIFFs")
rootPathCLCconf <- file.path(root,"CLMS_CLCplus_RASTER_2023confidence/TIFFs")
## DOWNLOADS NECESSARY INPUT ACCORDING TO BOUNDS ----
skip <- TRUE  ## this is used in 01_step.R to skip all checks that files exist
## if we are sure that we have all necessary files it saves time
memlogOn <- TRUE  ##  MEMLOG function reports the memory usage and does garbage
##  cleaning which takes time and might not be necessary.
