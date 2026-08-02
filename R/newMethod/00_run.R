if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(
  pbmcapply, this.path ,
  terra, hdar,
  sf, data.table,
  parallel,
  reticulate,
  dplyr
)

year <- 2023
bbox <- c(10.4, 45.6, 17.7, 51.1)
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
plot(geometry)
# Change to whatever root path and to where the CLCplus data tiles are and respective confidence tiles
root <- "/archivio/shared/geodati/raster"
rootPathCLC <- file.path(root,"CLMS_CLCplus_RASTER_2023/TIFFs")
rootPathCLCconf <- file.path(root,"CLMS_CLCplus_RASTER_2023confidence/TIFFs")

## data ----
clc_to_SB <- data.frame(
  clc_code = c(
    1,2,3,4,5,6,7,8,9,10,11
  ),

  clc_class = c(
    "Sealed",
    "Woody needle leaved trees",
    "Woody broadleaved deciduous trees",
    "Woody broadleaved evergreen trees",
    "Low-growing woody plants",
    "Permanent herbaceous",
    "Periodically herbaceous",
    "Lichens and mosses",
    "Non- and sparsely vegetated",
    "Water",
    "Snow and ice"
  ),

  SB_model = c(
    "NB1",
    "TL3",
    "TL5",
    "TL6",
    "SH5",
    "GR4",
    "GR3",
    "NB9",
    "NB9",
    "NB8",
    "NB2"
  ),

  SB_number = c(
    91,
    183,
    185,
    186,
    145,
    104,
    103,
    99,
    99,
    98,
    92
  ),

  fuel_type = c(
    "Non-burnable",
    "Conifer litter",
    "Broadleaf litter",
    "Evergreen broadleaf litter",
    "Shrub",
    "Grass",
    "Grass",
    "Non-burnable",
    "Non-burnable",
    "Non-burnable",
    "Non-burnable"
  )
)

fuel_models <- data.frame(
  number = c(
    101:109,
    121:124,
    141:149,
    161:165,
    181:189,
    201:204,
    91,92,93,98,99
  ),

  code = c(
    paste0("GR",1:9),
    paste0("GS",1:4),
    paste0("SH",1:9),
    paste0("TU",1:5),
    paste0("TL",1:9),
    paste0("SB",1:4),
    "NB1","NB2","NB3","NB8","NB9"
  ),

  group = c(
    rep("Grass",9),
    rep("Grass-Shrub",4),
    rep("Shrub",9),
    rep("Timber-Understory",5),
    rep("Timber-Litter",9),
    rep("Slash-Blowdown",4),
    rep("Non-burnable",5)
  ),

  vegetation = c(
    rep("Grassland",9),
    rep("Grass/Shrub",4),
    rep("Shrubland",9),
    rep("Forest",5),
    rep("Forest",9),
    rep("Forest",4),
    "Urban","Snow/Ice","Agriculture","Water","Bare Ground"
  ),

  forest_type = c(
    rep(NA,9),
    rep(NA,4),
    rep(NA,9),
    c("Conifer","Mixed","Mixed","Conifer","Mixed"),
    c("Conifer","Conifer","Mixed","Broadleaf","Broadleaf",
      "Conifer","Mixed","Broadleaf","Mixed"),
    c("Conifer","Conifer","Mixed","Mixed"),
    rep(NA,5)
  ),

  R = c(
    # GR
    189,120, 80, 40, 20,110,170,220,255,
    # GS
    140,120,100, 80,
    # SH
    180,160,140,120,100, 90, 80, 70, 60,
    # TU
    30, 40, 50, 60, 70,
    # TL
    20, 30, 40, 70,100, 20, 60,110, 80,
    # SB
    150,130,110, 90,
    # NB
    180,240,240, 60,180
  ),

  G = c(
    # GR
    255,230,210,180,150,170,180,190,220,
    # GS
    180,170,160,150,
    # SH
    150,135,120,110,100, 90, 80, 70, 60,
    # TU
    100,110,120,130,140,
    # TL
    70, 80, 90,120,150,100,130,170,140,
    # SB
    100, 80, 70, 60,
    # NB
    180,240,220,140,180
  ),

  B = c(
    # GR
    70, 50, 40, 30, 20, 40, 60, 80,120,
    # GS
    60, 50, 40, 30,
    # SH
    40, 35, 30, 25, 20, 15, 10, 10, 10,
    # TU
    30, 35, 40, 45, 50,
    # TL
    20, 20, 25, 35, 45, 30, 40, 55, 45,
    # SB
    50, 40, 30, 20,
    # NB
    180,255,120,255,180
  ),

  stringsAsFactors = FALSE
)

fuel_models$hex <- rgb(
  fuel_models$R,
  fuel_models$G,
  fuel_models$B,
  maxColorValue = 255
)

tmpWd <- getwd()
setwd(this.path::this.dir())
## DOWNLOADS NECESSARY INPUT ACCORDING TO BOUNDS ----
skip <- TRUE  ## this is used in 01_step.R to skip all checks that files exist
              ## if we are sure that we have all necessary files it saves time
source("01_step.R")
## ORGANIZES TRAINING AND CREATES MODEL ----
source("02_step.R")
## CREATES MODEL  ----
source("R/processing_20_Ronly_FuelModels_step2.R")
