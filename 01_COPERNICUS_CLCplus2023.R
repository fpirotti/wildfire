if(!require("hdar")){install.packages("hdar")}
library(hdar)
library(jsonlite)
library(terra)


username.hdar <- "fpirotti"
password.hdar <-"XfLUrVLtfuSD94M!!!"
client <- Client$new(username.hdar, password.hdar, save_credentials = TRUE)

client <- Client$new()
client$get_token()

### COPERNICUS LAND COVER+ -----
query <- '{
  "dataset_id": "EO:EEA:DAT:CLC-PLUS",
  "product_type": "Raster Layer",
  "resolution": "10m",
  "year": "2023",
  "bbox": [
    -10,
    24.0,
    26,
    53.0
  ],
  "itemsPerPage": 10,
  "startIndex": 0
}'

query <- '{
  "dataset_id": "EO:EEA:DAT:CLC-PLUS",
  "product_type": "Confidence Layer",
  "resolution": "10m",
  "year": "2023",
  "bbox": [
    -10,
    24.0,
    26,
    53.0
  ],
  "itemsPerPage": 100,
  "startIndex": 0
}'

client$generate_query_template("EO:EEA:DAT:CLC-PLUS")

matches <- client$search(query)

output_directory <- "/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023confidence"


existInFolder <- sapply(matches$results, FUN = function(x) {
  file.exists(paste0(file.path(output_directory,x$id), ".zip"))
  })
sum(existInFolder)

matches2 <- matches$clone(deep = T)
matches2$results <- matches2$results[!existInFolder]
length(matches2$results)
matches2$download(output_directory, prompt = F,
                 stop_at_failure = FALSE)



output_directory_tif <- file.path(output_directory, "unzipped")

exist<-tools::file_path_sans_ext(list.files(output_directory_tif))

sapply(list.files(output_directory, full.names = T, pattern = "\\.zip$"),
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
                                      output.vrt = "CLCplus2023.vrt")

gdalUtilities::gdal_translate()
mosaic2 <- terra::rast("CLCplus2023.vrt")

terra::writeRaster(mosaic2, "corine2023plus10m.tif",datatype="INT1U",  overwrite=T)
gdalUtils::gdaladdo("corine2023plus10m.tif", ro=T)

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
