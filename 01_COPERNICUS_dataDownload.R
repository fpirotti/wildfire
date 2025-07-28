if(!require("hdar")){install.packages("hdar")}
library(hdar)
library(jsonlite)
library(gdalUtilities)
library(terra)
library(parallel)
source("00_globals.R")
output_base_dir <- "/archivio/shared/geodati/raster"

ncores <- min(40, max(1,abs(parallel::detectCores()-2) ) )
## bounding box with
## West-most (lower) X, South-most Y,
## East-most (higher) X, North-most Y
bbox=c(-10,20,30,53)
startIndex=0
itemsPerPage=100
forceTifCreation <- FALSE
username.hdar <- "fpirotti"
password.hdar <-"XfLUrVLtfuSD94M!!!"
client <- Client$new(username.hdar, password.hdar, save_credentials = TRUE)

client <- Client$new()
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
  "productionStatus"= "ARCHIVED",
  "acquisitionType"= "NOMINAL",
  "platform"= "SENTINEL-1",
  "processingCenter"= "TU_WIEN",
  "resolution"= "1000",
  "startdate"= "2024-01-01T00:00:00.000Z",
  "enddate"= "2025-01-01T23:59:59.999Z"
 )
)

for(q in names(query)){
  qcont <- query[[q]]
  type <- query[[q]]$type
  query[[q]]$type <- NULL
  message_log("Querying ", q)
  qcont2 <-  qcont
  if(is.null(query[[q]]$bbox)) query[[q]]$bbox <- bbox
  if(is.null(query[[q]]$startIndex)) query[[q]]$startIndex <- startIndex
  if(is.null(query[[q]]$itemsPerPage)) query[[q]]$itemsPerPage <- itemsPerPage

  qcont <- jsonlite::toJSON(query[[q]],auto_unbox = T)

  output_directory <- sprintf("%s/%s", output_base_dir, q)
  if(file.exists(sprintf("%s/%s.tif",output_directory, q))){
    message_log("File TIF exists for ", q)
    next
  }

  matches <- client$search(qcont)
  if(length(matches$results) < 10) {
    browser()
  }
  if(!file.exists(output_directory)){
    message_log("Creating directory ", output_directory)
    dir.create(output_directory)
  }

  output_directory_tif <- file.path(output_directory, "TIFFs")

  if(!file.exists(output_directory_tif)){
    message_log("Creating TIF directory ", output_directory_tif)
    dir.create(output_directory_tif)
  }

i=0
leftToDownload=9999
while(i==0){

  existInFolder <- sapply(matches$results, FUN = function(x) {
    file.exists(paste0(file.path(output_directory_tif,x$id), ".tif"))
  })
  existInFolder <- unlist(existInFolder)
  message(sum(existInFolder), " already done out of ", length(matches$results))

  if(length(matches$results) == sum(existInFolder) ){
    message_log("Done!=======")
    break
  }

  matches2 <- matches$clone(deep = T)
  matches2$results <- matches2$results[!existInFolder]

  message(length(matches2$results), " to do be done out of ", length(matches$results))
  if(leftToDownload == length(matches2$results) ){
    message("still same number of files left to download (",leftToDownload,"), strange... will break")
    break
  }
  outp <- tryCatch({
    matches2$download(output_directory, prompt = F, stop_at_failure = FALSE)
    },
    error = function(e) {
      message_log("======== Error, will wait 10 min then retry...
the EU server stops when too many requests are
done too fast  (but does not provide a way to fix this...)")
      print(e)
  })

    if(is.element("error", class(outp))) {
      i<-0
      message("sleeping 8 minutes...")
      Sys.sleep(500)
      next
    }

    leftToDownload <- length(matches2$results)
    i<-1

    message_log("FINISHED download")
  }


message_log("Unzipping all and keeping only tif files")

exist<-tools::file_path_sans_ext(list.files(output_directory_tif))

noret <- mclapply(list.files(output_directory, full.names = T, pattern = "\\.zip$"),
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
             return(e)
        })
         return(TRUE)
})
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



  tifs<-tools::file_path_sans_ext(list.files(output_directory_tif, pattern = "\\.tif$"))
  zips<-tools::file_path_sans_ext(list.files(output_directory, pattern = "\\.zip$"))
  hasTif <- is.element( zips, tifs)
  if(any(!hasTif)){
    warning_log("Not all zip files are converted to TIFs! Check problem!!")
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
     sf:: gdal_utils(util = "translate", vrtPath,
                     destination = sprintf("%s/%s.tif",output_directory, q),
                     options = c("-ot", "Byte",
                                 "-co", "TILED=YES",
                                 "-co", "BIGTIFF=YES",
                                 "-co", "COMPRESS=DEFLATE")  )
       return(TRUE)
     },
     error = function(e) {
       return(FALSE)
     })

     if(!ret){
       warning_log("PROBLEM WRITING TIFs! Check problem!!")
     } else {
       message_log("FINISHED writing final file TIF for ", q)
       sf::gdal_addo(sprintf("%s/%s.tif",output_directory,  q),
                     read_only = T,
                     overviews = c(2, 4, 8, 16, 32, 64, 128, 256, 512),
                     config_options= c("GDAL_NUM_THREADS"=sprintf("%d", ncores))
       )
     }
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
