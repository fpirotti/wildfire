library(terra)
library(parallel)
source("00_globals.R")

clc <- terra::rast("/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023/CLMS_CLCplus_RASTER_2023.tif")
clcv <- terra::cells(clc)
shrubs <- clc[clcv] == 5
message_log("tot pixels with shrub in EU: ", length(shrubs) )
message_log("tot pixels with shrub in EU in ha: ", length(shrubs)*100/10000 )
message_log("tot pixels with shrub in EU in km2: ", length(shrubs)*100/1000000 )
#
#
# tmpFolder <- "tmps/"
# tileFolder <- "/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023/TIFFs"
#
# tifsInFullPath <- list.files(tileFolder, pattern = "\\.tif$",full.names = T)
# tifsIn<-tools::file_path_sans_ext(filename(tifsInFullPath))
# names(tifsInFullPath) <- tifsIn
# tifsOut<-tools::file_path_sans_ext(list.files(tmpFolder, pattern = "\\.tif$"))
#
# tifs2calculate <- setdiff(tifsIn, tifsOut)
#
# noret <- mclapply(tifsInFullPath[tifs2calculate] ,
#                   mc.cores = 30,
#                   FUN = function(x) {
#                     ret <- tryCatch( {
#                       r <- terra::rast(x)
#                       ## prod is kg*C/m^2/year 0.0001 so must be multiplied by 10e4 * 100 m2 in 10 m pixel
#                       prod <- terra::resample(terra::crop(terra::rast("/archivio/shared/geodati/raster/productivityYearlyModis500mEPSG3035.tif"),
#                                         r), r)
#                       bm <- terra::resample(terra::crop(terra::rast("/archivio/shared/geodati/raster/Biomass/biomass100mEPSG3035.tif"),
#                                         r), r)
#                       r[r[]!=5] = NA
#                       r2  <- r/r
#                       r2*prod
#                       terra::writeRaster(r, tmpFolder%+%"/"%+%filename(x),
#                                          datatype="INT4U")
#                       TRUE
#                     },
#                     error = function(e) {
#                       FALSE
#                     })
#                     ret
#                 })
#

