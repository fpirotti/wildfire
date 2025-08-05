# src <- 'NETCDF:"/archivio/shared/geodati/raster/Biomass/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2022-fv6.0.nc":agb'
# mosaic <- sf::gdal_utils("warp",
#                          src,
#                          destination =  "/archivio/shared/geodati/raster/Biomass/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2022-fv6.0_3035.tif",
#                          options =  c(  "-t_srs", "EPSG:3035"  )
# )
glob.biom <- terra::rast('/archivio/shared/geodati/raster/Biomass/Biomass10m3035_CLCmask.tif')
glob.clc <- terra::rast('/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023/CLMS_CLCplus_RASTER_2023.tif')

glob.bmCount <- terra::zonal(bm3035, clcr, fun=sum)


clcplusClasses <- c( "0"="No Data", "1"= "Sealed",
                     "2"= "Woody – needle leaved trees",
                     "3"= "Woody – Broadleaved deciduous trees",
                     "4"= "Woody – Broadleaved evergreen trees",
                     "5"= "Low-growing woody plants - bushes shrubs",
                     "6"= "Permanent herbaceous",
                     "7"= "Periodically herbaceous","8"= "Lichens and mosses",
                     "9"= "Non- and sparsely-vegetated","10" = "Water ",
                     "11" = "Snow and ice",
                     "253"="None",
                     "254"="Empty")

BiomCalc<-function(path, Val=5, pathBiom, plotIt=F){
  out <- jsonlite::fromJSON(sf::gdal_utils("translate", path,
                                           "tmp.csv",
                                           options = c("-of", "xyz"  )))
  values <- out$stac$`raster:bands`$histogram$buckets[[1]][1:12]
  names(values) <- clcplusClasses
  as.list( values / 100)
  # plot(terra::rast(path))
}

AreaCalc<-function(path, plotIt=F){
  out <- jsonlite::fromJSON(sf::gdal_utils("info", path,
                 options = c("-hist",
                             "-json",
                             "-norat",
                             "-noct")))
  values <- out$stac$`raster:bands`$histogram$buckets[[1]][1:12]
  names(values) <- clcplusClasses
  as.list( values / 100)
  # plot(terra::rast(path))
}

library(tibble)
library(tableHTML)
library(terra)
message("Starting")
# path <-  "CataloniaRasterCLC2023.tif"
# clcAll <- function2apply("/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023/CLMS_CLCplus_RASTER_2023.tif")
# clc <- function2apply("CataloniaRasterCLC2023.tif")
clcr <-   terra::rast("CataloniaRasterCLC2023.tif")
# bm <- terra::rast("CataloniaRasterESACCIbiomass.tif")
bm3035 <-   terra::rast("CataloniaRasterESACCIbiomass3035.tif")

bmcatal.count <- terra::zonal( bm3035,  clcr, fun="notNA" )
names(bmcatal.count)<- c("ClassV",   "AGBn")
bmcatal.avg <- terra::zonal( bm3035,  clcr, fun="mean")
names(bmcatal.avg)<- c("ClassV", "AGBmean" )
# bmcatal.sum <- terra::zonal( bm3035,  clcr, fun="sum")
# names(bmcatal.sum)<- c("ClassV", "AGBsum" )
bmcatal <- merge(bmcatal.count,bmcatal.avg)
bmcatalt<-tibble(.rows = nrow(bmcatal))

bmcatalt$`CLCn`  <- bmcatal$ClassV
bmcatalt$`Corine Land Cover Plus 2023 Class` <- clcplusClasses[ as.character(bmcatal$ClassV) ]
bmcatalt$`Catalonia Tot. Area sum (ha)` <-  format(bmcatal$AGBn / 100, big.mark = "'")
bmcatalt$`Catalonia Tot. Biomass  (Mg)` <- format( round(bmcatal$AGBmean * bmcatal$AGBn / 100), big.mark = "'")
load(file="AllAreas.rda")
tot <- merge(bmcatalt,
             tibble( CLCn= 1:length(clcAll[-1]),
                       "All EU 2023 Tot. Area sum (ha)"=
                       format(as.numeric(unlist(clcAll[-1])),
                              big.mark= "'")
                    )
             )

writexl::write_xlsx(tot, "tot.xlsx")
save(tot, file="tot.rda")
# bmcatalt$`All EU 2023 Tot. Area sum (ha)`<- format( tot$AGBmeanTotEU , big.mark = "'")



 tableHTML(tot,rownames = F )


# save(bmcatalWithNames, file="AllAreasBiomass.rda")
