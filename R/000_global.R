library(rgee)
library(rgeeExtra)
library(stars)
library(googledrive)
library(this.path)

### setting version ----
versionFuelModel  = 3

########### THIS REQUIRES FIRST THAT THE processing_01_GEE_tileMeta.R!
# 1. Authenticate ----
drive_auth(email = "cirgeo@unipd.it")
# ee_Initialize(user = 'cirgeo'  )
scott_burgan_models <- c(
  # Grass Models (GR1 - GR9)
  101, 102, 103, 104, 105, 106, 107, 108, 109,
  # Grass-Shrub Models (GS1 - GS4)
  121, 122, 123, 124,
  # Shrub Models (SH1 - SH9)
  141, 142, 143, 144, 145, 146, 147, 148, 149,
  # Timber-Understory Models (TU1 - TU5)
  161, 162, 163, 164, 165,
  # Timber Litter Models (TL1 - TL9)
  181, 182, 183, 184, 185, 186, 187, 188, 189,
  # Slash-Blowdown Models (SB1 - SB4)
  201, 202, 203, 204,
  # Non-Burnable Models (Urban, Ag, Water, Rock)
  91, 92, 93, 98, 99
)
### setting scale ----
# proj3035_30m = ee$Projection('EPSG:3035')$atScale(scale);
proj_3035_30m <- list(
  crs = "EPSG:3035",
  crsTransform = c(30, 0, 4321000, 0, -30, 3210000)
)
proj_3035_10m <- list(
  crs = "EPSG:3035",
  crsTransform = c(10, 0, 4321000, 0, -10, 3210000)
)
