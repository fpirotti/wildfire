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


sapply(matches$results, FUN = function(x) { x$id })
mosaic <- terra::rast(list.files("output/", full.names = T))
