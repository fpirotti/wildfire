if(!require("hdar")){install.packages("hdar")}
library(hdar)
library(jsonlite)
library(terra)
library(parallel)
library(httr2)
source("00_globals.R")
output_base_dir <- "/archivio/shared/geodati/raster"

ncores <- min(40, max(1,abs(parallel::detectCores()-2) ) )
## bounding box with
## West-most (lower) X, South-most Y,
## East-most (higher) X, North-most Y
bbox=c(-10,20,50,83)
startIndex=0
itemsPerPage=100
forceQuery <- FALSE
forceTifCreation <- FALSE
username.hdar <- "fpirotti"
password.hdar <-"XfLUrVLtfuSD94M!!!"
client <- Client$new(username.hdar, password.hdar, save_credentials = TRUE)

client$get_token()


### COPERNICUS DATA -----
query <- list(

"CLMS_CLCplus_RASTER_2023"=list(
    "dataset_id"=  "EO:EEA:DAT:CLC-PLUS",
    "product_type"=  "Raster Layer",
    "resolution"=  "10m",
    "year"=  "2023",
    "type"="Byte"
  ),
"CLMS_CLCplus_RASTER_2023confidence"= list(
    "dataset_id"=  "EO:EEA:DAT:CLC-PLUS",
    "product_type"=  "Confidence Layer",
    "resolution"=  "10m",
    "year"=  "2023",
    "type"="Byte"
  ),
"CLMS_TCF_TreeDensity_RASTER_2021" = list(
  "dataset_id"= "EO:EEA:DAT:HRL:TCF",
  "product_type"="Tree Cover Density",
  "resolution"= "10m",
  "year"= "2021",
  "type"="Byte"
),
"CLMS_TCF_TreeDensity_RASTER_2021_Conf" = list(
  "dataset_id"=  "EO:EEA:DAT:HRL:TCF",
  "product_type"=  "Tree Cover Density Confidence Layer",
  "resolution"=  "10m",
  "year"=  "2021",
  "type"="Byte"
),

"CLMS_TCF_DominantLeafType_RASTER_2021" = list(
  "dataset_id"=  "EO:EEA:DAT:HRL:TCF",
  "product_type"=  "Dominant Leaf Type",
  "resolution"=  "10m",
  "year"=  "2021",
  "type"="Byte"
),

"CLMS_TCF_DominantLeafType_RASTER_2021_Conf" = list(
  "dataset_id"=  "EO:EEA:DAT:HRL:TCF",
  "product_type"=  "Dominant Leaf Type Confidence Layer",
  "resolution"=  "10m",
  "year"=  "2021",
  "type"="Byte"
),

"CLMS_CropTypes_RASTER_2021" = list(
  "dataset_id"=  "EO:EEA:DAT:HRL:CRL",
  "product_type"=  "Crop Types",
  "resolution"=  "10m",
  "year"=  "2021",
  "type"="ConvToByte"
),
"CLMS_CropTypes_RASTER_2021_Conf" = list(
  "dataset_id"= "EO:EEA:DAT:HRL:CRL",
  "product_type"= "Crop Types Confidence Layer",
  "resolution"=  "10m",
  "type"="ConvToByte"
),
"CLMS_SurfaceSoilMoisture_2024"=list(
  "dataset_id"= "EO:CLMS:DAT:CLMS_GLOBAL_SSM_1KM_V1_DAILY_NETCDF",
  "productType"= "SSM1km",
  "resolution"= "1000",
  "startdate"= "2023-01-01T00:00:00.000Z",
  "enddate"= "2025-01-01T23:59:59.999Z"
 )
)
## step 1 query tiles ----
matches <- list()
if(file.exists("matches.rda")) load("matches.rda")
for(q in names(query)){
  if(!forceQuery  && !is.null(matches[[q]])){
    message_log("Exists, skipping ", q)
    next
  }
  qcont <- query[[q]]
  type <- query[[q]]$type
  query[[q]]$type <- NULL
  message_log("Querying ", q)
  qcont2 <-  qcont
  # message(qcont2$dataset_id , " - ", qcont2$product_type)

  if(is.null(query[[q]]$bbox)) query[[q]]$bbox <- bbox
  if(is.null(query[[q]]$startIndex)) query[[q]]$startIndex <- startIndex
  if(is.null(query[[q]]$itemsPerPage)) query[[q]]$itemsPerPage <- itemsPerPage

  qcont <- jsonlite::toJSON(query[[q]],auto_unbox = T)

  message_log("Querying starting ", q)
  matches[[q]] <- client$search(qcont)
  if(length(matches[[q]]$results) < 10) {
    browser()
  }
  save(matches,file="matches.rda")
  message_log("Querying  ",q," finished with  ", length(matches[[q]]$results), " results;" )
}

## step 2 download files ----
for(q in names(matches)){

  output_directory <- sprintf("%s/%s", output_base_dir, q)
  if(!file.exists(output_directory)){
    message_log("Creating directory ", output_directory)
    dir.create(output_directory)
  }

  output_directory_tif <- file.path(output_directory, "TIFFs")
  message_log("Output TIF dir   ", output_directory_tif)
  if(!file.exists(output_directory_tif)){
    message_log("Creating TIF directory ", output_directory_tif)
    dir.create(output_directory_tif)
  }

  existInFolder <- sapply(matches[[q]]$results, FUN = function(x) {
    file.exists(paste0(file.path(output_directory,x$id), ".zip"))||
      file.exists(paste0(file.path(output_directory_tif,x$id), ".tif"))
  })
  existInFolder <- unlist(existInFolder)
  message_log(q, ": ", sum(existInFolder),
          " already done out of ",
          length(matches[[q]]$results))

  if(length(matches[[q]]$results) == sum(existInFolder) ){
    message_log("All Done for ",q,"!=======")
    next
  }

  matches2 <- matches[[q]]$clone(deep = T)
  matches2$results <- matches2$results[!existInFolder]

  message(length(matches2$results), " to do be done out of ", length(matches[[q]]$results))
  if(leftToDownload == length(matches2$results) ){
    message_log("still same number of files left to download (",leftToDownload,"), strange... will break")
    break
  }



  i=0
  leftToDownload=9999
  while(i==0){

    existInFolder <- sapply(matches[[q]]$results, FUN = function(x) {
      file.exists(paste0(file.path(output_directory,x$id), ".zip"))||
        file.exists(paste0(file.path(output_directory_tif,x$id), ".tif"))
    })
    existInFolder <- unlist(existInFolder)
    message_log("LOOP: ",q, ": ", sum(existInFolder), " already done out of ",
                length(matches[[q]]$results))

    if(length(matches[[q]]$results) == sum(existInFolder) ){
      message_log("LOOP: Done all ",length(matches[[q]]$results)," tiles!=======")
      break
    }

    matches2 <- matches[[q]]$clone(deep = T)
    matches2$results <- matches2$results[!existInFolder]

    message(length(matches2$results), " to do be done out of ", length(matches[[q]]$results))
    if(leftToDownload == length(matches2$results) ){
      warning_log("LOOP: still same number of files left to download again (",leftToDownload,"), strange... will break")
      break
    }

    outp <- tryCatch({
      matches2$download(output_directory, prompt = F, stop_at_failure = FALSE, verbose=TRUE)
      },
      error = function(e) {
        message_log("======== Error, will wait 1 hour from ",format(Sys.time(), "%Y-%m-%d %H:%M:%S"),  " then retry...
  the Wekeo API stops if more than 100 requests per hour...you can ask higher quotas")
        print(e)
        e
    })

    if(is.element("error", class(outp))) {
      i<-0
      message_log("LOOP: sleeping 1 hour as we reached the 100 tile quota... ")
      Sys.sleep(3601)
      next
    }

    existInFolder <- sapply(matches[[q]]$results, FUN = function(x) {
      file.exists(paste0(file.path(output_directory,x$id), ".zip"))||
        file.exists(paste0(file.path(output_directory_tif,x$id), ".tif"))
    })
    existInFolder <- unlist(existInFolder)

    if(any(!existInFolder)) {
      i<-0
      message_log("LOOP: sleeping 12 secs to retry to download the ", sum(!existInFolder)," files with errors")
      Sys.sleep(12)
      next
    }

    leftToDownload <- length(matches2$results)
    i<-1

    message_log("LOOP: FINISHED download")
  }
}



## step 3 create TIF and overviews  ----
for(q in names(matches)){

  message_log("Unzipping all and keeping only tif files")

  exist<-tools::file_path_sans_ext(list.files(output_directory_tif, pattern="\\.tif$"))
  zips <- list.files(output_directory, full.names = T, pattern = "\\.zip$")
  noret <- mclapply(zips,
                   mc.cores = 10,
         FUN = function(x) {
          if( is.element( filename(x) , exist ) ){
            return(TRUE)
          }
           tryCatch( {
             unzip(x,exdir = output_directory_tif)
             return(TRUE)
             },
             error = function(e) {
               warning_log(e$message)
               return(FALSE)
          })
           return("should not be here")
  })


  tifs<-tools::file_path_sans_ext(list.files(output_directory_tif, pattern = "\\.tif$"))
  zips<-tools::file_path_sans_ext(list.files(output_directory, pattern = "\\.zip$"))
  hasTif <- is.element( zips, tifs)
  if(any(!hasTif)){

    warning_log("Not all zip files are converted to TIFs!",
                sum(any(!hasTif)) ," not downloaded. Check problem!!")
  }

  message_log("Unzipped ", sum(unlist(noret)), " zip files" )

  message_log("Removing XMLs")
  sapply(list.files(output_directory_tif, full.names = T, pattern = "\\.xml$"),
         FUN = function(x) {
           file.remove(x)
  })

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
