library(terra)
library(parallel)
source("00_globals.R")
clc <- terra::rast("/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023/CLMS_CLCplus_RASTER_2023.tif")



SHAPEcatalonia <- terra::project( terra::vect("input/Catalonia.shp"),
                                  clc)
# clc_catal <- terra::crop(clc, SHAPEcatalonia)
#
#
# clc_catalFit <- terra::mask(clc_catal, SHAPEcatalonia)
# terra::writeRaster(clc_catalFit, "CataloniaRasterCLC2023.tif",
#                    datatype="INT1U", overwrite=T)
clc_catalFit <- terra::rast( "CataloniaRasterCLC2023.tif")


clcv2 <- terra::cells(clc_catalFit)
clc_catalFitShrub <- clc_catalFit==5
shrubs2 <- clc_catalFitShrub[]
Area <- sum(shrubs2, na.rm = T)
message_log("tot pixels with shrub in Catalonia: ", )
message_log("tot pixels with shrub in Catalonia in ha: ", Area*100/10000 )
message_log("tot pixels with shrub in Catalonia in km2: ", Area*100/1000000 )

bm <- terra::resample(terra::crop(terra::rast("/archivio/shared/geodati/raster/Biomass/biomass100mEPSG3035.tif"),
                                  clc_catalFit), clc_catalFit)

boundsCatalonia <- terra::vect( terra::ext(clc_catalFit))
terra::crs(boundsCatalonia)<-terra::crs("EPSG:3035")
boundsCatalonia4326 <- terra::project(bounds, terra::crs(bm) )

bm<-terra::rast("/archivio/shared/geodati/raster/Biomass/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2022-fv6.0.nc")
bmCatalonia<-terra::crop(bm$agb,boundsCatalonia4326)
# plot(bmCatalonia)
bmp <- terra::resample(terra::project(bmCatalonia, terra::crs(clc_catalFit)),
                       clc_catalFit)

terra::writeRaster(bmCatalonia, "CataloniaRasterESACCIbiomass.tif",
                    overwrite=T)

terra::writeRaster(bmp, "CataloniaRasterESACCIbiomass.tif",
                   overwrite=T)
bmShrub <- clc_catalFitShrub*bmp / 100
Biomass <- sum(bmShrub[], na.rm = T)
message_log("Mg of  shrub in Catalonia: ", Biomass )

# clcv2 <- terra::cells(clc_catal, SHAPEcatalonia)
# shrubs2 <- clc_catal[ clcv2[,2] ] == 5
# sum(shrubs2)

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
#
