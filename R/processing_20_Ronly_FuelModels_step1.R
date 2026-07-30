library(this.path)

# library(terra)
library(reticulate)
# library(rvest)
# library(xml2)
library(httr2)
library(parallel)
library(sf)

year <- 2023
lat <- seq(45.6, 49.6, by = 0.5)# north bounds  51.1
lon <- seq(10.4, 17.6, by = 0.1) # east bounds 17.7

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
  lon[[1]], lat[[1]],
  lon[[length(lon)]], lat[[1]],
  lon[[length(lon)]], lat[[length(lat)]],
  lon[[1]], lat[[length(lat)]],
  lon[[1]], lat[[1]]
), ncol = 2, byrow = TRUE)

geometry = st_sfc(st_polygon(list(coords)), crs = 4326)
plot(geometry)

####################################
# TREE HEIGHT FEATURES -----
##################
downloadETHtreeHeight <- function(){
  r<-range(lat)
  lat <- seq(floor(r[[1]]), ceiling(r[[2]]), by = 3)
  r<-range(lon)
  lon <- seq(floor(r[[1]]), ceiling(r[[2]]), by = 3)
  tiles <- expand.grid(
    lat = lat,
    lon = lon,
    KEEP.OUT.ATTRS = FALSE
  )
  options(timeout = 3600)
  base <- "https://libdrive.ethz.ch/index.php/s/cO8or7iOe5dT2Rt/download?path=%2F3deg_cogs&files="
  outdir <- "/archivio/shared/geodati/raster/TreeHeights_10m_2020_ETH"



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
  msg<-list()
  for(i in seq_len(nrow(tiles))){

    dest <- file.path(outdir, tiles$file[i])
    message(tiles$file[i])
    if(!file.exists(dest)){
      download.file(
        tiles$url[i],
        destfile = dest,
        mode = "wb",
        quiet = TRUE
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
  outdir
}
path.CHM <- downloadETHtreeHeight()


####################################
# TESSERA FEATURES -----
##################
# needs python 3.12 ! Create a virtual environment (only once)
downloadTESSERA <- function(){

  # diff(range(lat))*diff(range(lon))*100*300/1000
  tiles <- expand.grid(
    lat = lat,
    lon = lon,
    KEEP.OUT.ATTRS = FALSE
  )

    if (!virtualenv_exists("geotessera-env")) {
    virtualenv_create("geotessera-env")
    # Use the environment
    use_virtualenv("geotessera-env", required = TRUE)

    # Install geotessera
    py_install(
      packages = "geotessera",
      envname = "geotessera-env",
      method = "virtualenv",
      pip = TRUE
    )
  }


  outdir <- "/archivio02/shared/geodati/raster/GeoTESSERA"
  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

  download_one<-  function(i) {

    lo <- tiles$lon[i]
    la <- tiles$lat[i]

    # bbox format: xmin,ymin,xmax,ymax
    bbox <- sprintf(
      "%s,%s,%s,%s",
      lo,
      la,
      lo + 0.1,
      la + 0.5
    )


    # tile_out <- file.path(
    #   outdir,
    #   sprintf("TESSERA_10m_%d_%s_%s", year, fmt_lon(lo), fmt_lat(la))
    # )

    stdout_log <- file.path(outdir, sprintf("stdoutTESSERA_10m_%d_%s_%s", year, fmt_lon(lo), fmt_lat(la)))
    stderr_log <- file.path(outdir, sprintf("stderrTESSERA_10m_%d_%s_%s", year, fmt_lon(lo), fmt_lat(la)))
    system2(
      "/home/pirotti/.virtualenvs/geotessera-env/bin/geotessera",
      c(
        "download",
        "--bbox", bbox,
        "--year", year,
        "--output", outdir
      ),
      stdout = stdout_log,
      stderr = stderr_log
    )


  }

  results <- mclapply(
    seq_len(nrow(tiles)),
    download_one,
    mc.cores = 50
  )

  tifs <- list.files(file.path(outdir, "global_0.1_degree_representation"), pattern="\\.tiff$", recursive = T, full.names = T)
  cat(tifs, file="my_list.txt")
  system("gdaltindex tile_index.gti.gpkg -t_srs EPSG:3035 --optfile my_list.txt")
}

# downloadTESSERA()

#######################
# DOWNLOAD FIRE LOSS  ----------
#######################
firelossFile <-"/archivio/shared/geodati/raster/EUR_fire_forest_loss_2001-25_annual.tif"
if(!file.exists(firelossFile)){
  message("downloading", firelossFile)

  options(timeout = 3600)
  download.file("https://glad.umd.edu/users/Alexandra/Fire_GFL_data/2001-25/EUR_fire_forest_loss_2001-25_annual.tif",
                destfile = firelossFile)
}


# ####################################

# CLC <- list.files("/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023/TIFFs", full.names = T)
# CLCconf <- list.files("/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023confidence/TIFFs", full.names = T)
#
# if(length(CLC)!=length(CLCconf)){
#   sprintf("%s%s",
#           substr(basename(CLCconf),0,30)[[1]],
#           setdiff(substr(basename(CLC), 28,405),substr(basename(CLCconf),30,407))
#   )
# }
#
# clcTilePath<-CLC[[1]]
# clcConfTilePath<-CLCconf[[1]]
#
#
# processTile<-function(clcTilePath, clcConfTilePath){
#
#
#   clcTile <- rast(clcTilePath)
#   clcTileConf <- rast(clcConfTilePath)
#   fuelModelTile <- rast(clcTile)
#   fuelModelConfTile <- rast(clcTileConf)
#   # fuelModelTile <- setValues(fuelModelTile, 98L)
#
#   clc_v <- terra::values(clcTile, mat=FALSE)
#   clc_v_conf <- terra::values(clcTileConf, mat=FALSE)
#
#   fuel <- integer(length(clc_v))
#   fuel[] <- 98L
#
#   fuel[clc_v == 1]  <- 91     # Urban
#   fuel[clc_v == 10] <- 98     # Water
#   fuel[clc_v == 11] <- 92     # Snow
#   fuel[clc_v %in% c(8,9)] <- 99   # Bare
#
#   mask_grass_or_grassshrub <- clc_v %in% c(6,7)
#   mask_shrub <- clc_v == 5
#   mask_forest <- clc_v %in% c(2,3,4)
#
#   ## grab CH
#   e <- terra::ext(clcTile)
#   args <- c(
#     "-multi",
#     "-wo", "NUM_THREADS=ALL_CPUS",
#     "-t_srs", "EPSG:3035",
#     "-tr", "10", "10",
#     "-tap",
#     "-te",
#     e$xmin, e$ymin, e$xmax, e$ymax,
#     "source.tif",
#     "out.tif"
#   )
#
#
#   fuelModelTile[] <- fuel
#
# }
#
# # initialize output
#
