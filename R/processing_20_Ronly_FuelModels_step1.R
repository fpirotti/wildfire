library(this.path)
source(file.path(this.path::this.dir(), "000_global.R"))
library(terra)

CLC <- list.files("/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023/TIFFs", full.names = T)
CLCconf <- list.files("/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023confidence/TIFFs", full.names = T)

if(length(CLC)!=length(CLCconf)){
  sprintf("%s%s",
          substr(basename(CLCconf),0,30)[[1]],
          setdiff(substr(basename(CLC), 28,405),substr(basename(CLCconf),30,407))
  )
}

clcTilePath<-CLC[[1]]
clcConfTilePath<-CLCconf[[1]]


processTile<-function(clcTilePath, clcConfTilePath){


  clcTile <- rast(clcTilePath)
  clcTileConf <- rast(clcConfTilePath)
  fuelModelTile <- rast(clcTile)
  fuelModelConfTile <- rast(clcTileConf)
  # fuelModelTile <- setValues(fuelModelTile, 98L)

  clc_v <- terra::values(clcTile, mat=FALSE)
  clc_v_conf <- terra::values(clcTileConf, mat=FALSE)

  fuel <- integer(length(clc_v))
  fuel[] <- 98L

  fuel[clc_v == 1]  <- 91     # Urban
  fuel[clc_v == 10] <- 98     # Water
  fuel[clc_v == 11] <- 92     # Snow
  fuel[clc_v %in% c(8,9)] <- 99   # Bare

  mask_grass_or_grassshrub <- clc_v %in% c(6,7)
  mask_shrub <- clc_v == 5
  mask_forest <- clc_v %in% c(2,3,4)

  fuelModelTile[] <- fuel

}

# initialize output

