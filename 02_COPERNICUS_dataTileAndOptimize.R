library(terra)
library(parallel)

source("00_globals.R")

if(file.exists("matches.rda")) load("matches.rda") else stop("please first run ", " 01_COPERNICUS_dataDownload.R script!")

## step 3 create TIF and overviews  ----
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

  noret <- mclapply(zips, mc.cores = 10,
                    FUN = function(x) {
                      f<-tools::file_path_sans_ext(filename(x))
                      if( is.element( f, tifs ) ){
                        return(1)
                      }
                      out <- tryCatch( {
                        unzip(x,exdir = output_directory_tif)
                        TRUE
                      },
                      error = function(e) {
                        warning_log(e$message)
                        FALSE
                      })
                      return(out)
                    })


  tifs<-tools::file_path_sans_ext(list.files(output_directory_tif, pattern = "\\.tif$"))
  if(length(matches[[q]]$results) == length(tifs) ){
    message_log("All Done for ",q,"!=======")
  } else {
    warning_log(q, ": WARNING! ",  length(tifs), " TIF files but expecting ",
                 length(matches[[q]]$results),
                 " - will not create raster for ",q,"!=======")
    next
  }

  tifs<-tools::file_path_sans_ext(list.files(output_directory_tif, pattern = "\\.tif$"))
  zips<-tools::file_path_sans_ext(list.files(output_directory_tif, pattern = "\\.zip$"))

  hasTif <- is.element( zips, tifs)
  if(any(!hasTif)){
    warning_log(q, ": Not all zip files are converted to TIFs!",
                sum(any(!hasTif)) ," not downloaded. Check problem!!")
    next
  }

  message_log(q, ": Unzipped ", sum(unlist(noret)), " zip files" )

  message_log(q, ": Removing XMLs")
  sapply(list.files(output_directory_tif, full.names = T, pattern = "\\.xml$"),
         FUN = function(x) {
           file.remove(x)
  })

  message_log(q, ": Creating VRT!")
  vrtPath <- sprintf("%s/%s.vrt",output_directory, q)
  mosaic <- sf::gdal_utils("buildvrt",
                           list.files(output_directory_tif, full.names = T, pattern = "\\.tif$"),
                           destination = vrtPath )


  if(!mosaic){
    warning_log(q, ": VRT not successful while creating ", vrtPath)
  } else {
    file.symlink(vrtPath, "output/"%+%filename(vrtPath))
  }

}


mclapply(names(matches)), function=FUN(q){

    if(grepl("CropTypes", q)){
      browser()
    }

    message_log("Creating VRT")
    vrtPath <- sprintf("%s/%s.vrt",output_directory, q)
    mosaic <- sf::gdal_utils("buildvrt",
                             list.files(output_directory_tif, full.names = T, pattern = "\\.tif$"),
                             destination = vrtPath )


    if(!mosaic){
      warning_log("VRT not successful while creating ", q)
    }

    message_log("Removing ZIPs")
    noret <- sapply(list.files(output_directory, full.names = T, pattern = "\\.zip$")[hasTif],
           FUN = function(x) {
             file.remove(x)
           })


     if(forceTifCreation || !file.exists(sprintf("%s/%s.tif",output_directory, q))){

     message_log("START writing final file TIF - compressed with predictor=2
  deflate and tiled=yes for ", q)
       ret <- tryCatch( {
         rr <- sf:: gdal_utils(util = "translate", vrtPath,
                       destination = sprintf("%s/%s.tif",output_directory, q),
                       options = c("-ot", "Byte",
                                   "-co", "TILED=YES",
                                   "-co", "BIGTIFF=YES",
                                   "-co", "COMPRESS=DEFLATE")  )
         TRUE
       },
       error = function(e) {
         warning_log(e$message)
         FALSE
       })

       if(!ret){
         warning_log("PROBLEM WRITING TIF  ",sprintf("%s/%s.tif",output_directory,  q) ,"! Check problem!!")
       } else {
         message_log("FINISHED writing final file TIF for ", q)
       }
       # sf::gdal_utils("info", sprintf("%s/%s.tif",output_directory,  q) )
       sf::gdal_addo(sprintf("%s/%s.tif",output_directory,  q),
                     read_only = T,
                     overviews = c(2, 4, 8, 16, 32, 64, 128, 256, 512),
                     config_options= c("GDAL_NUM_THREADS"=sprintf("%d", ncores))
       )
     } else {

       message_log("Final file TIF exists for ", q)
     }
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
