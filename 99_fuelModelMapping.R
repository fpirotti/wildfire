library(sf)
library(terra)
tempDir <- "intermediateRastersForFuelMap"
areaPolygon <- sf::read_sf("data/nutsProvincesPilotRegions.gpkg")
# 10 m CLC+ backbone 2023 -----
if(!dir.exists(tempDir)) dir.create(tempDir)
# saveWD <- getwd()
# setwd(tempDir)

# 1. 10 m CLC+ backbone 2023 -----
## translate much faster
if(!file.exists(tempDir%+%"/CLCplus.tif")) {
  gdalUtils::gdal_translate(src_dataset = "CLCplus2023.vrt",
                          dst_dataset = tempDir%+%"/CLCplus.tif",
                          projwin = sf::st_bbox(areaPolygon)[c(1,4,3,2)] )
}

if(!file.exists(tempDir%+%"/CLCplus2023confidence.tif")) {
  gdalUtils::gdal_translate(src_dataset = "CLCplus2023confidence.vrt",
                            dst_dataset = tempDir%+%"/CLCplus2023confidence.tif",
                            projwin = sf::st_bbox(areaPolygon)[c(1,4,3,2)] )
}

data.clc <- terra::rast(tempDir%+%"/CLCplusConf.tif")
data.clc.conf <-  terra::rast(tempDir%+%"/CLCplus2023confidence.tif")

# plot(data.clc)
# plot(areaPolygon, add=T)



# setwd(saveWD)
