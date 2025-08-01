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
    warning_log(q, ": UNZIP -  ",," missing files to download!")
    next
  }

  message_log(q, ": Removing XMLs")
  sapply(list.files(output_directory_tif, full.names = T, pattern = "\\.xml$"),
         FUN = function(x) {
           file.remove(x)
  })

  message_log(q, ": Creating VRT!")
  vrtPath <- sprintf("%s/%s.vrt",output_directory, q)
  mosaic <- sf::gdal_utils("buildvrt",
                           list.files(output_directory_tif, full.names = T, pattern = "\\.tif$"),
                           destination = vrtPath,
                           options = c(
                             "-overwrite"
                           ))
  if(!mosaic){
    warning_log(q, ": VRT not successful while creating ", vrtPath)
  } else {
    if(file.exists("output/"%+%filename(vrtPath) ) ) file.remove( "output/"%+%filename(vrtPath))
    file.symlink(vrtPath, "output/"%+%filename(vrtPath))
  }

}


## step 4  create TIF and overviews  ----
cores = min(names(matches), 10)
for(q in names(matches)){

 lapply(names(matches),
          # mc.cores = cores,
          FUN=function(q){

    if(grepl("CropTypes", q)){
      browser()
    }


     if(forceTifCreation || !file.exists(sprintf("%s/%s.tif",output_directory, q))){

       opts <- c("-ot", "Byte",
                 "-co", "TILED=YES",
                 "-co", "BIGTIFF=YES",
                 "-co", "COMPRESS=DEFLATE")

     message_log(q, ": START writing final file TIF - compressed  deflate and tiled=yes for ", q)
       ret <- tryCatch( {
         rr <- sf:: gdal_utils(util = "translate", vrtPath,
                       destination = sprintf("%s/%s.tif",output_directory, q),
                       options = opts  )
         TRUE
       },
       error = function(e) {
         FALSE
       })

       if(!ret){
         warning_log("PROBLEM WRITING TIF  ",sprintf("%s/%s.tif",output_directory,  q) ,"! Check problem!!")
       } else {
         message_log("FINISHED writing final file TIF for ", q)
       }
     } else {

       message_log("Final file TIF exists for ", q)
     }

    sf::gdal_addo(sprintf("%s/%s.tif",output_directory,  q),
                           read_only = T,
                           overviews = c(2, 4, 8, 16, 32, 64, 128, 256, 512,1024),
                           config_options= c("GDAL_NUM_THREADS"=sprintf("%d", 10))
     )
  })
}

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
