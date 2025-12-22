library(terra)
library(parallel)

source("00_globals.R")

if(file.exists("matches.rda")) load("matches.rda") else stop("please first run ", " 01_COPERNICUS_dataDownload.R script!")

checkTifsMissing <- function(q){
  output_directory <- sprintf("%s/%s", output_base_dir, q)
  output_directory_tif <- file.path(output_directory, "TIFFs")
  ids <- sapply(matches[[q]]$results, FUN = function(x) {  x$id })
  tifExistInFolder <- sapply(ids, FUN = function(x) {
    file.exists(paste0(file.path(output_directory_tif,x), ".tif"))
  })
  ids[!tifExistInFolder]
}
# checkTifsMissing(q)
## step 3 UNZIP  ----
message_log("===================== STEP 3 UNZIP ==============")

for(q in names(matches)){
  output_directory <- sprintf("%s/%s", output_base_dir, q)
  output_directory_tif <- file.path(output_directory, "TIFFs")
  message_log(q, "...unzipping all in ", output_directory_tif, " and keeping only tif files")
  if(!file.exists(output_directory)){
    warning_log("  directory ", output_directory, " does not exist! Going to next one.")
    next
  }

  if(!file.exists(output_directory_tif)){
    warning_log("  TIF directory ", output_directory_tif, " does not exist! Going to next one.")
    next
  }
  zips <- list.files(output_directory, full.names = T, pattern = "\\.zip$")
  tifs<-tools::file_path_sans_ext(list.files(output_directory_tif, pattern = "\\.tif$"))

  if(length(zips) < 1){
    message_log(q, ": UNZIP - NO ZIPS TO UNZIP")
  } else {
    message_log(q, ": UNZIPPING ",length(zips)," ZIPs")
    noret <- mclapply(zips, mc.cores = 30,
                    FUN = function(x) {
                      f<-tools::file_path_sans_ext(filename(x))
                      if( is.element( f, tifs ) ){
                        return(T)
                      }
                      unzip_success <- tryCatch( {
                        unzip(x,exdir = output_directory_tif)
                        TRUE
                      },
                      error = function(e) {
                        FALSE
                      })
                      if (unzip_success) {
                        file.remove(x)
                        return(T)
                      } else {
                        warning_log(q, ": ❌ Zip file NOT removed due to unzip failure.")
                      }
                      return(F)
                    })

  }

  missing <- checkTifsMissing(q)
  if(length(missing)!=0){
    warning_log(q, ": UNZIP -  ", length(missing)," missing files to download!")
    next
  }

  message_log(q, ": Removing XMLs")
  sapply(list.files(output_directory_tif, full.names = T, pattern = "\\.xml$"),
         FUN = function(x) {
           file.remove(x)
  })

  message_log(q, ": Creating VRT!")
  vrtPath <- sprintf("%s/%s.vrt",output_directory, q)
  isNC <- grepl("SurfaceSoil", q)
  files <- list.files(ifelse( isNC, output_directory, output_directory_tif),
             full.names = T, pattern = ifelse( isNC, "\\.nc$", "\\.tif$")
  )

  if( isNC){
    message_log(q, ": Creating VRT for soil moisture!")
    mosaic <- sf::gdal_utils("buildvrt",
                             files,
                             destination = vrtPath,
                             options =  c("-sd", "1", "-separate" )
    )
    vrtPath <- sprintf("%s/%s_Conf.vrt",output_directory, q)
    mosaic <- sf::gdal_utils("buildvrt",
                             files,
                             destination = vrtPath,
                             options =  c("-sd", "2", "-separate" )
    )
    vrtPath <- sprintf("%s/%s.vrt",output_directory, q)
  } else {
    mosaic <- sf::gdal_utils("buildvrt",
                             files,
                             destination = vrtPath
    )
  }
  if(!mosaic){
    warning_log(q, ": VRT not successful while creating ", vrtPath)
  } else {
    if(file.exists("output/"%+%filename(vrtPath) ) ) file.remove( "output/"%+%filename(vrtPath))
    file.symlink(vrtPath, "output/"%+%filename(vrtPath))
  }

}
message_log("==============finish  STEP 3 UNZIP ==============")

## step 4  create TIF and overviews  ----
message_log("============== STEP 4 tif and overviews ==============")
cores = min(names(matches), 10)

for(q in names(matches)){

forceTifCreation <- FALSE
  # outmc <- mclapply(names(matches),
  #        mc.cores = cores,
  #         FUN=function(q){


      output_directory <- sprintf("%s/%s", output_base_dir, q)
      output_directory_tif <- file.path(output_directory, "TIFFs")
      vrtPath <- sprintf("%s/%s.vrt",output_directory, q)

      message_log(q, " STEP 4 tif and overviews ==============")

      if(forceTifCreation || !file.exists(sprintf("%s/%s.tif",output_directory, q))){

       opts <- c("-ot", "Byte",
                 "-co", "TILED=YES",
                 "-co", "BIGTIFF=YES",
                 "-co", "COMPRESS=DEFLATE")

       # if(forceTifCreation) opts <- c(opts, "-overwrite")

       if(grepl("CropTypes", q) || grepl("SurfaceSoil", q) ){
         opts <- c(opts, "-scale", "1000" ,"3300")
         next
       }


      # if(isNC){
      #
      #   rr <- terra::rast(vrtPath)
      #
      #   rr <- stars::read_stars(vrtPath)
      #   # new_crs = st_crs('EPSG:3035')
      #   # nrr <- stars::st_warp(rr[,,,1], crs = new_crs  )
      #   # plot(nrr)
      #   names(rr)<- as.Date(substr( basename(files), 14, 21), "%Y%m%d" )
      #   terra::time(rr) <- as.POSIXct(as.Date(substr( basename(files), 14, 21), "%Y%m%d" ) )
      #   rr.mean <- terra::app(rr, fun=function(x) mean(x, na.rm = TRUE),
      #                         filename= sprintf("%s/%s_Mean2024.tif",output_directory, q),
      #                         cores=50)
      #   message_log(q, ": START writing final file TIF - compressed  deflate and tiled=yes for ", q)
      #   # writeRaster(rr / 10 -100, sprintf("%s/%s.tif",output_directory, q), datatype="INT1U")
      #   writeRaster(rr , sprintf("%s/%s.tif",output_directory, q), datatype="INT1U")
      #   message_log(q, ": STOP writing final file TIF - compressed  deflate and tiled=yes for ", q)
      #
      #
      #   vrtPath <- sprintf("%s/%s_Conf.vrt",output_directory, q)
      #   rr <- terra::rast(vrtPath)
      #   names(rr)<- as.Date(substr( basename(files), 14, 21), "%Y%m%d" )
      #   terra::time(rr) <- as.POSIXct(as.Date(substr( basename(files), 14, 21), "%Y%m%d" ) )
      #   message_log(q, ": START writing final file TIF - compressed  deflate and tiled=yes for ", q)
      #   # writeRaster(rr / 10 -100, sprintf("%s/%s.tif",output_directory, q), datatype="INT1U")
      #   writeRaster(rr , sprintf("%s/%s_Conf.tif",output_directory, q), datatype="INT1U")
      #   message_log(q, ": STOP writing final file TIF - compressed  deflate and tiled=yes for ", q)
      #
      # }


       ret <- tryCatch( {
         rr <- sf:: gdal_utils(util = "translate",
                               vrtPath,
                               destination = sprintf("%s/%s.tif",output_directory, q),
                       options = opts  )
         TRUE
       },
       error = function(e) {
         e$message
       })

       if(!isTRUE(ret) ){
         warning_log(q, ": PROBLEM WRITING TIF  ",sprintf("%s/%s.tif",output_directory,  q) ,"! Check problem message = '", ret,"'")
       } else {
         message_log(q, ": FINISHED writing final file TIF"  )
       }
     } else {

       message_log(q, ": Final file TIF EXISTS")
     }

  if(!file.exists(sprintf("%s/%s.tif.ovr",output_directory, q))){
    sf::gdal_addo(sprintf("%s/%s.tif",output_directory,  q),
                  read_only = T,
                  overviews = c(2, 4, 8, 16, 32, 64, 128, 256, 512,1024),
                  config_options= c("GDAL_NUM_THREADS"=sprintf("%d", 40))
    )
  }

 }
#)
# }



## step 5  tiles   ----
#
#   for(q in names(matches)){
#     output_directory <- sprintf("%s/%s", output_base_dir, q)
#     output_directory_tif <- file.path(output_directory, "TIFFs")
#     system("gdaltindex " %+% output_directory %+% "/" %+% q %+% ".shp " %+%
#              output_directory_tif %+% "/*.tif")
#   }

# library(ssh)
#
# # Create SSH connection (will prompt for password unless SSH key is used)
#
# # session <- ssh_connect("pirotti@www.cirgeo.unipd.it:5055")
# session <- ssh_connect(
#   host = "pirotti@www.cirgeo.unipd.it:5055",
#   key = "~/.ssh/id_ed25519"  # or your actual key file path
# )

# scp_upload(
#   session,
#   files = "corine2023plus10m.tif",  # Local file path
#   to = "./uploads/"                       # Remote destination folder (on the server)
# )
