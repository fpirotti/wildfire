library(this.path)
source(file.path(this.path::this.dir(), "000_global.R"))
library(terra)
library(reticulate)
library(rvest)
library(xml2)
library(httr2)

downloadETHtreeHeight <- function(){
  base <- "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files="
  outdir <- "/archivio/shared/geodati/raster/TreeHeights_10m_2020_ETH"
  lat <- seq(45, 52, by = 3)
  lon <- seq(9, 18, by = 3)

  fmt_lat <- function(x){
    if(x >= 0)
      sprintf("N%02d", x)
    else
      sprintf("S%02d", abs(x))
  }

  fmt_lon <- function(x){
    if(x >= 0)
      sprintf("E%03d", x)
    else
      sprintf("W%03d", abs(x))
  }

  tiles <- expand.grid(
    lat = lat,
    lon = lon,
    KEEP.OUT.ATTRS = FALSE
  )

  tiles$file <- sprintf(
    "ETH_GlobalCanopyHeight_10m_2020_%s%s_Map.tif",
    vapply(tiles$lat, fmt_lat, ""),
    vapply(tiles$lon, fmt_lon, "")
  )

  tiles$fileSD <- sprintf(
    "ETH_GlobalCanopyHeight_10m_2020_%s%s_Map_SD.tif",
    vapply(tiles$lat, fmt_lat, ""),
    vapply(tiles$lon, fmt_lon, "")
  )
  tiles$url <- paste0(base, URLencode(tiles$file))
  tiles$urlSD <- paste0(base, URLencode(tiles$fileSD))

  dir.create(outdir, showWarnings = FALSE)

  for(i in seq_len(nrow(tiles))){

    dest <- file.path(outdir, tiles$file[i])
    message(tiles$file[i])
    if(!file.exists(dest)){
      download.file(
        tiles$url[i],
        destfile = dest,
        mode = "wb",
        quiet = FALSE
      )
    }
    dest <- file.path(outdir, tiles$fileSD[i])
    message(tiles$fileSD[i])

    if(!file.exists(dest)){
      download.file(
        tiles$urlSD[i],
        destfile = dest,
        mode = "wb",
        quiet = FALSE
      )
    }
  }
}

# downloadETHtreeHeight()
##################
# Create a virtual environment (only once)
if (!virtualenv_exists("geotessera-env")) {
  virtualenv_create("geotessera-env")
}

# Use the environment
use_virtualenv("geotessera-env", required = TRUE)

# Install geotessera
py_install(
  packages = "geotessera",
  envname = "geotessera-env",
  method = "virtualenv",
  pip = TRUE
)

# Test
gt <- import("geotessera")
print(gt)

##################
download.file("https://maps.effis.emergency.copernicus.eu/effis?service=WFS&request=getfeature&typename=ms:modis.ba.poly&version=1.1.0&outputformat=SPATIALITEZIP", "EFFIS_fireAreas")
cmd <- "gdalwarp"


system2(cmd, args = args, stdout = TRUE, stderr = TRUE)

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

  ## grab CH
  e <- terra::ext(clcTile)
  args <- c(
    "-multi",
    "-wo", "NUM_THREADS=ALL_CPUS",
    "-t_srs", "EPSG:3035",
    "-tr", "10", "10",
    "-tap",
    "-te",
    e$xmin, e$ymin, e$xmax, e$ymax,
    "source.tif",
    "out.tif"
  )


  fuelModelTile[] <- fuel

}

# initialize output

