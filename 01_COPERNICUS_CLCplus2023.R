if(!require("hdar")){install.packages("hdar")}
library(hdar)
library(jsonlite)
library(terra)


username.hdar <- "fpirotti"
password.hdar <-"XfLUrVLtfuSD94M!!!"
client <- Client$new(username.hdar, password.hdar, save_credentials = TRUE)

client <- Client$new()
client$get_token()

bbox=c(-10,24,26,53)
startIndex=0
itemsPerPage=10

### COPERNICUS LAND COVER+ -----
query <- list(
"treeCover2021" = list(
  "dataset_id"=  "EO:EEA:DAT:HRL:TCF",
  "product_type"=  "Tree Cover Density",
  "resolution"=  "10m",
  "year"=  "2021"
),
"treeCover2021Conf" = list(
  "dataset_id"=  "EO:EEA:DAT:HRL:TCF",
  "product_type"=  "Tree Cover Density Confidence Layer",
  "resolution"=  "10m",
  "year"=  "2021"
),

"treeType2021" = list(
  "dataset_id"=  "EO:EEA:DAT:HRL:TCF",
  "product_type"=  "Dominant Leaf Type",
  "resolution"=  "10m",
  "year"=  "2021"
),
"treeType2021Conf" = list(
  "dataset_id"=  "EO:EEA:DAT:HRL:TCF",
  "product_type"=  "Dominant Leaf Type Confidence Layer",
  "resolution"=  "10m",
  "year"=  "2021"
),
  "cropTypes2021" = list(
  "dataset_id"=  "EO:EEA:DAT:HRL:CRL",
  "product_type"=  "Crop Types",
  "resolution"=  "10m",
  "year"=  "2021"
),
"cropTypes2021Conf" = list(
  "dataset_id"=  "EO:EEA:DAT:HRL:CRL",
  "product_type"=  "Crop Types",
  "resolution"=  "10m"
)
# ,"CLMS_CLCplus_RASTER_2023"=list(
#   "dataset_id"=  "EO:EEA:DAT:CLC-PLUS",
#   "product_type"=  "Raster Layer",
#   "resolution"=  "10m",
#   "year"=  "2023",
#   "bbox"=  [
#     -10,
#     24.0,
#     26,
#     53.0
#   ],
#   "itemsPerPage"=  10,
#   "startIndex"=  0
# ),
#  "CLMS_CLCplus_RASTER_2023confidence"= list(
#   "dataset_id"=  "EO:EEA:DAT:CLC-PLUS",
#   "product_type"=  "Confidence Layer",
#   "resolution"=  "10m",
#   "year"=  "2023",
#   "bbox"=  [
#     -10,
#     24.0,
#     26,
#     53.0
#   ],
#   "itemsPerPage"=  100,
#   "startIndex"=  0
# )
 )

for(q in names(query)){
  qcont <- query[[q]]

  message("Querying ", q)
  qcont2 <- jsonlite::fromJSON(qcont)
  if(is.null(query[[q]]$bbox)) query[[q]]$bbox <- bbox
  if(is.null(query[[q]]$startIndex)) query[[q]]$startIndex <- startIndex
  if(is.null(query[[q]]$itemsPerPage)) query[[q]]$itemsPerPage <- itemsPerPage

  jsonlite::toJSON(query[[q]],auto_unbox = T)

  matches <- client$search(qcont)

# client$generate_query_template("EO:EEA:DAT:CLC-PLUS")

output_directory <- sprintf("/archivio/shared/geodati/raster/%s", q)
if(!file.exists(output_directory)){
  message("Creating directory ", output_directory)
  dir.create(output_directory)
}
output_directory_tif <- file.path(output_directory, "unzipped")
if(!file.exists(output_directory_tif)){
  message("Creating TIF directory ", output_directory_tif)
  dir.create(output_directory_tif)
}

i=0
while(i==0){
  existInFolder <- sapply(matches$results, FUN = function(x) {
    file.exists(paste0(file.path(output_directory,x$id), ".zip"))
  })
  message(sum(existInFolder), " already done out of ", length(matches$results))

  matches2 <- matches$clone(deep = T)
  matches2$results <- matches2$results[!existInFolder]

  message(length(matches2$results), " to do be done out of ", length(matches$results))

  outp <- tryCatch({
    matches2$download(output_directory, prompt = F, stop_at_failure = FALSE)

  },
  error = function(e) {
    message("======== Error, will wait 10 min then retry... the EU server stops
            when too many requests are done too fast  (but does not provide a way to fix this...)")
    print(e)
  })

  if(is.element("error", class(outp))) {
    i<-0
    message("sleeping 8 minutes...")
    Sys.sleep(500)
    next
  }

  i<-1
}


exist<-tools::file_path_sans_ext(list.files(output_directory_tif))

noret <- sapply(list.files(output_directory, full.names = T, pattern = "\\.zip$"),
       FUN = function(x) {
        if( is.element( filename(x) , exist ) ){
          return(NA)
        }
         tryCatch( {
           unzip(x,exdir = output_directory_tif)
           },
           error = function(e) {
             print(e)
             stop()
             } )
})


sapply(list.files(output_directory_tif, full.names = T, pattern = "\\.xml$"),
       FUN = function(x) {
         file.remove(x)
       })

mosaic <- gdalUtilities::gdalbuildvrt(list.files(output_directory_tif, full.names = T, pattern = "\\.tif$"),
                                      output.vrt = sprinft("%s.vrt", q) )

# gdalUtilities::gdal_translate()
mosaic2 <- terra::rast(sprinft("%s.vrt", q))

terra::writeRaster(mosaic2, sprintf("%s.tif", q),
                   datatype="INT1U",
                   overwrite=T)
gdalUtils::gdaladdo(sprintf("%s.tif", q), ro=T)

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
