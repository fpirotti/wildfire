library(sf)
library(terra)


source("00_globals.R")
### this script assumes that there are available rasters in the same
### aligned coordinate system. It will clip them.

tempDir <- "intermediateRastersForFuelMap"
areaPolygon <- sf::read_sf("data/nutsProvincesPilotRegions.gpkg")
## for testing small area
areaPolygon <- areaPolygon |> sf::st_union()  #|> sf::st_buffer(-53000)# |> sf::st_boundary()
sf::st_area(areaPolygon)/10000

inputData <- list()
inputDataConf <- list()
outputData <- list()
outputDataConf <- list()
###############################
# 10 m CLC+ backbone 2023 -----
if(!dir.exists(tempDir)) dir.create(tempDir)
# saveWD <- getwd()
# setwd(tempDir)

# 1. 10 m CLC+ backbone 2023 -----
## clip  much faster with GDAL!
#  tt <- terra::crop(terra::rast("CLCplus2023.vrt"), areaPolygon)
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

inputData[["CLC"]] <-      terra::rast(tempDir%+%"/CLCplus.tif")
inputDataConf[["CLC"]] <-  terra::rast(tempDir%+%"/CLCplus2023confidence.tif")
clc.classes = list(
"1" = "Sealed",
"2" = "Woody needle leaved trees",
"3" = "Woody broadleaved deciduous trees",
"4" = "Woody broadleaved evergreen trees",
"5" = "Low-growing woody plants",
"6" = "Permanent herbaceous",
"7" = "Periodically herbaceous",
"8" = "Lichens and mosses",
"9" = "Non and sparsely vegetated",
"10" = "Water",
"11" = "Snow and ice",
"253" = "Coastal seawater buffer",
"254"="Outside area",
"255"="No data"
)


terraOptions(threads = 4)  # set number of threads
# 91 Urban or suburban development; insufficient wildland fuel ----
outputData$a91 = (inputData[["CLC"]]==1 )*100

# 98 water ----
outputData$a98 = (inputData[["CLC"]]==10 )*100

# 99 barren ----
outputData$a99 = (inputData[["CLC"]] > 253 |  (inputData[["CLC"]] > 7 &  inputData[["CLC"]] < 10 )) *100

# 92 snow ice ----
outputData$a92 = (inputData[["CLC"]]==11 )*100


# 93 agriculture ----
outputData$a92 = (inputData[["CLC"]]==11 )*100


# WRITE ALL RASTERS as maps 0 or 100 !! ----
for(r in names(outputData)) {
  writeRaster(outputData[[r]], tempDir%+%"/fuelModelCl"%+%r%+%".tif",
            datatype="INT1U", overwrite=T)
  writeRaster(outputDataConf[[r]], tempDir%+%"/fuelModelCl"%+%r%+%"Conf.tif",
              datatype="INT1U", overwrite=T)
}
