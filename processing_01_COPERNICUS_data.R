if(!require("hdar")){install.packages("hdar")}
library(hdar)
library(jsonlite)
library(terra)

username <- "fpirotti"
password <-"XfLUrVLtfuSD94M!!!"
client <- Client$new(username, password, save_credentials = TRUE)

client <- Client$new()
client$get_token()

### COPERNICUS LAND COVER+ -----
query <- '{
  "dataset_id": "EO:EEA:DAT:CLC-PLUS",
  "product_type": "Raster Layer",
  "resolution": "10m",
  "year": "2023",
  "bbox": [
    6.6008900105198265,
    43.57167703997222,
    17.57836121702079,
    50.29123679099156
  ],
  "itemsPerPage": 200,
  "startIndex": 0
}'

matches <- client$search(query)

output_directory <- "output"
matches$download(output_directory)


sapply(matches$results, FUN = function(x) { x$id })


sapply(list.files("output/", full.names = T, pattern = "\\.zip$"),
       FUN = function(x) {
         tryCatch( {
           unzip(x,exdir = "output/")
           },
           error = function(e) {
             stop()
             } )
      })


sapply(list.files("output/", full.names = T, pattern = "\\.zip$"),
       FUN = function(x) {
         file.remove(x)
       })

mosaic <- gdalUtilities::gdalbuildvrt(list.files("output", full.names = T, pattern = "\\.tif$"),
                                      output.vrt = "virtual.vrt")
mosaic2 <- terra::rast(mosaic)

terra::writeRaster(mosaic2, "corine2023plus10m.tif",datatype="INT1U",  overwrite=T)


library(ssh)

# Create SSH connection (will prompt for password unless SSH key is used)

# session <- ssh_connect("pirotti@www.cirgeo.unipd.it:5055")
session <- ssh_connect(
  host = "pirotti@www.cirgeo.unipd.it:5055",
  key = "~/.ssh/id_ed25519"  # or your actual key file path
)

scp_upload(
  session,
  files = "corine2023plus10m.tif",  # Local file path
  to = "./uploads/"                       # Remote destination folder (on the server)
)
