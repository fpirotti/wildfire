# =================================== #
# DOWNLOAD INPUT DATA -----
#################################
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(
  jsonlite, hdar
)

# =================================== #
# COPERNICUS CLC BACKBONE  -----
# =================================== #
downloadCLCPlus <- function(){
  # 1. Initialize the client and authenticate
  # (Replace with your actual WEkEO username and password)
  user <- "fpirotti"
  password <- "*******************************"

  if( !(exists("skip") && skip) ){
    client <- Client$new(user = user, password = password, save_credentials = TRUE)
  } else {
    message("Skipping check in CLC dataset")
  }
  # 2. Check your connection status
  print(client$token()) # If successful, this will output your active session token

  # 3. Accept Copernicus Terms and Conditions (Required for downloads)
  # This command automatically accepts all relevant T&Cs for the datasets
  # client$terms_and_conditions(accept_all = TRUE)

  outList <- list()
  for(rootPath in c(rootPathCLC, rootPathCLCconf)){

    if( !(exists("skip") && skip) ){
      # 4. Paste your WEkEO JSON query structure here
      json_query_string <- sprintf('{
    "dataset_id": "EO:EEA:DAT:CLC-PLUS",
    "productType": "%s",
    "resolution": "10m",
      "bbox": [%.3f,%.3f,%.3f,%.3f],
    "year": "%s",
    "itemsPerPage": 200,
    "startIndex": 0
  }', ifelse(rootPath==rootPathCLC, "Raster Layer","Confidence Layer"),
                                   bbox[[1]],
                                   bbox[[2]],
                                   bbox[[3]],
                                   bbox[[4]],
                                   year)

      # Convert the raw JSON text string into a list structure that R understands
      query_list <- fromJSON(json_query_string, simplifyVector = FALSE)
      print("Searching WEkEO catalog...")
      matches <- client$search(json_query_string)
      match <- matches
      matches <- list(TCD=match)
      # Print out data metrics found
      cat("Total files found: ", matches[[1]]$total_count, "\n")
      cat("Total download size: ", matches[[1]]$total_size/1000000, " Mbytes\n")

      # 6. Execute the download
      # Define your local output directory
      output_dir <- rootPath

      print("Starting download...")
      existing_files <- tools::file_path_sans_ext(list.files(output_dir))
      df <- sapply(matches[[1]]$results, function(x){
        # print(x$id)
        nchar(x[["id"]])<48||x$id%in%existing_files
      })

      matches[[1]]$results <- matches[[1]]$results[!df]
      if(length(matches[[1]]$results)>0){
        message("Downloading " , length(matches[[1]]$results), " files.")

        download_dir <- dirname(rootPathCLC)
        matches[[1]]$download(output_dir = download_dir, force = FALSE)

        # 1. Find all zip files in your directory
        zip_files <- list.files(download_dir, pattern = "\\.zip$", full.names = TRUE)
        # 2. Loop through and unzip them
        for (zip_file in zip_files) {
          # Create a target folder name based on the zip file name (minus the .zip extension)

          fn <- tools::file_path_sans_ext(zip_file)

          if (!file.exists(fn)) {
            message(paste("📦 Unzipping:", basename(zip_file)))
            unzip(zip_file, exdir = rootPathCLC)
          } else {
            message(paste("⏩ Already unzipped:", basename(zip_file)))
          }
        }
      } else {
        message("All files downloaded already ")
      }
    }

    ## grab the tiles as VRT
    message("Creating VRT")
    file.remove(sprintf("%s/000_mosaic.vrt", rootPath))
    system(sprintf("gdalbuildvrt  %s/000_mosaic.vrt %s/*.tif",
                   rootPath, rootPath) )

    outList[[ifelse(rootPath==rootPathCLC, "Raster Layer","Confidence Layer")]] <-
      terra::rast(sprintf("%s/000_mosaic.vrt", rootPath))

  }
  outList

}
path.CLCplus <- downloadCLCPlus()
# =================================== #
# TREE HEIGHTs -----
# =================================== #
downloadETHtreeHeight <- function(){

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


  r<-range(lat)
  while(floor(r[[1]])%%3!=0){
    r[[1]] <- r[[1]] - 1
  }
  lat <- seq(floor(r[[1]]), ceiling(r[[2]]), by = 3)
  r<-range(lon)
  while(floor(r[[1]])%%3!=0){
    r[[1]] <- r[[1]] - 1
  }
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
  message("Creating VRT ")



  system(sprintf("gdalbuildvrt  %s/000_mosaic.vrt %s/*Map.tif",
                 outdir, outdir),ignore.stderr =T)
  system(sprintf("gdalbuildvrt  %s/000_mosaicSD.vrt %s/*Map_SD.tif",
                 outdir, outdir),ignore.stderr =T)

  message("ETH Tree height tiles mosaic returned as list with values and SD rasters")

  list(rast.values=terra::rast(sprintf("%s/000_mosaic.vrt",
                      outdir)),
       rast.SD=terra::rast(sprintf("%s/000_mosaicSD.vrt",
                  outdir) ) )
}
path.CHM <- downloadETHtreeHeight()

# =================================== #
# TESSERA FEATURES -----
# =================================== #
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
  tifs <- list.files(file.path(outdir, "global_0.1_degree_representation"), pattern="\\.tiff$", recursive = T, full.names = T)
  old.nTiffs <- length(tifs)
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
    # bbox <- sprintf(
    #   "%s,%s,%s,%s",
    #   10.4, 45.6, 17.7, 51.1)

    # stdout_log <- file.path(outdir, sprintf("stdoutTESSERA_10m_%d", year))
    # stderr_log <- file.path(outdir, sprintf("stderrTESSERA_10m_%d", year))

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

  if( !(exists("skip") && skip) ){
    message("We start downloading tiles, ", length(tifs), " already present, are you sure you need to re-run (existing tiffs will be skipped)?")
    ans <- readline("Continue? (y/n): ")
    if (tolower(ans) == "y") {
      results <- pbmclapply(
        seq_len(nrow(tiles)),
        download_one,
        mc.cores = 50
      )
    }
  } else {
    message("Skipping check in GeoTESSERA dataset")
  }




  tifs <- list.files(file.path(outdir, "global_0.1_degree_representation"), pattern="\\.tiff$", recursive = T, full.names = T)

  if(old.nTiffs == length(tifs)){
    message("No new tiles downloaded")
  } else{
    message(abs(old.nTiffs - length(tifs)), " new files downloaded, recreating Tileindex" )
    message("Creating tile index for ", length(tifs), " files")
    cat(tifs, file="000_list.txt")
    file.remove(sprintf("%s/000_tileindex.gpkg",outdir))
    system( sprintf("gdaltindex %s/000_tileindex.gpkg  -t_srs EPSG:3035 --optfile 000_list.txt",
                    outdir) )
    file.remove("000_list.txt")
  }

  message("GeoTESSERA Map downloaded and tile index provided (tiles are in different CRS)")
  sf::read_sf(sprintf("%s/000_tileindex.gpkg",
                      outdir))


}
path.TesseraTiles <- downloadTESSERA()

# =================================== #
#  FIRE LOSS  ----------
# =================================== #
downloadFireLoss <- function(){
  firelossFile <-"/archivio/shared/geodati/raster/EUR_fire_forest_loss_2001-25_annual.tif"
  if(!file.exists(firelossFile)){
    message("downloading", firelossFile)
    options(timeout = 3600)
    if(Sys.which("aria2c") == ""){
      system(sprintf("wget -q --show-progress -c -N -P %s https://glad.umd.edu/users/Alexandra/Fire_GFL_data/2001-25/EUR_fire_forest_loss_2001-25_annual.tif",
              firelossFile))
    } else {
      system(sprintf("aria2c -q --summary-interval=1 -s 16 -x 16 -d %s https://glad.umd.edu/users/Alexandra/Fire_GFL_data/2001-25/EUR_fire_forest_loss_2001-25_annual.tif",
                     dirname(firelossFile) ))
    }

  }
  message("Fire Loss Map downloaded")
  terra::rast(firelossFile)
}
path.FireLoss <-   downloadFireLoss()


# =================================== #


