library(jsonlite)
# library(terra)
# library(parallel)
# source("00_globals.R")

`%+%` <- function(a, b) paste0(a, b)

output_base_dir <- "/archivio/shared/geodati/raster"

ncores <- min(40, max(1,abs(parallel::detectCores()-2) ) )
## bounding box with
## West-most (lower) X, South-most Y,
## East-most (higher) X, North-most Y
bbox=c(-10,20,30,53)
baseHansen <- "https://storage.googleapis.com/earthenginepartners-hansen/GFC-2024-v1.12/Hansen_GFC-2024-v1.12_lossyear_"
lats <- seq(40,60,10)
longs <-seq(-10,20,10)

ew <- ifelse(longs<0, sprintf("%03dW", abs(longs[longs<0])),  sprintf("%03dE", longs[!(longs<0) ]))
ns <- ifelse(lats<0, sprintf("%02dS", lats[lats<0]),  sprintf("%02dN", lats[!(lats<0) ]))

grid <- expand.grid(ns, ew)
grid$text <- sprintf("%s%s_%s.tif", baseHansen, grid$Var1, grid$Var2 )

### COPERNICUS DATA -----
 query <- list(
 "GFC_2024_Hansen_v1_12_lossyear" =grid$text
 )


for(q in names(query)){
  qcont <- query[[q]]

  message("Querying ", q)

  output_directory <- sprintf("%s/%s", output_base_dir, q)
  if(!file.exists(output_directory)){
    message("Creating directory ", output_directory)
    dir.create(output_directory)
  }

  if(file.exists(sprintf("%s/%s.tif",output_directory, q))){
    message("File TIF exists for ", q)
    next
  }


  output_directory_tif <- file.path(output_directory, "TIFFs")

  if(!file.exists(output_directory_tif)){
    message("Creating TIF directory ", output_directory_tif)
    dir.create(output_directory_tif)
  }

  for(link in qcont){
    message("Downloading ", basename(link))
    if(!file.exists(output_directory_tif %+% "/" %+% basename(link))){
      download.file(link, output_directory_tif %+% "/" %+% basename(link) )
    }
  }

}
