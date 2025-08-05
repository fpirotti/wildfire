# src <- 'NETCDF:"/archivio/shared/geodati/raster/Biomass/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2022-fv6.0.nc":agb'
# mosaic <- sf::gdal_utils("warp",
#                          src,
#                          destination =  "/archivio/shared/geodati/raster/Biomass/ESACCI-BIOMASS-L4-AGB-MERGED-100m-2022-fv6.0_3035.tif",
#                          options =  c(  "-t_srs", "EPSG:3035"  )
# )


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
#
# AreaCalc<-function(path, plotIt=F){
#   out <- jsonlite::fromJSON(sf::gdal_utils("info", path,
#                  options = c("-hist",
#                              "-json",
#                              "-norat",
#                              "-noct")))
#   values <- out$stac$`raster:bands`$histogram$buckets[[1]][1:12]
#   names(values) <- clcplusClasses
#   as.list( values / 100)
#   # plot(terra::rast(path))
# }
#
# library(tibble)
# library(tableHTML)
# library(terra)
# message("Starting")
# # path <-  "CataloniaRasterCLC2023.tif"
# # clcAll <- function2apply("/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023/CLMS_CLCplus_RASTER_2023.tif")
# clc <- function2apply("CataloniaRasterCLC2023.tif")
# clcr <-   terra::rast("CataloniaRasterCLC2023.tif")
# bm3035 <-   terra::rast("CataloniaRasterESACCIbiomass3035.tif")
# #
# bmcatal.count <- terra::zonal( bm3035,  clcr, fun="notNA" )
# names(bmcatal.count)<- c("ClassV",   "AGBn")
# bmcatal.avg <- terra::zonal( bm3035,  clcr, fun="mean")
# names(bmcatal.avg)<- c("ClassV", "AGBmean" )
# bmcatal.sum <- terra::zonal( bm3035,  clcr, fun="sum")
# names(bmcatal.sum)<- c("ClassV", "AGBsum" )
# bmcatal <- merge(bmcatal,bmcatal.avg)
# # bmcatal$AGBmeanNew<-NULL
#
# save(bmcatal, file="bmcatal.rda")
# bmcatalAll.avg
# load("AllAreas.rda")
# bmcatalAll.areaHa <- tibble( ClassV= 1:length(clcAll[-1]),
#                                "All EU 2023 Tot. Area sum (ha)"=
#                                as.numeric(unlist(clcAll[-1]))
#                             )
# bmcatalAll <-merge(bmcatalAll.avg , bmcatalAll.areaHa)
# bmcatalAll$AGBTot <- bmcatalAll$AGBmean * bmcatalAll$`All EU 2023 Tot. Area sum (ha)`
# save(bmcatalAll, file="bmcatalAll.rda")

load("bmcatalAll.rda")
load("bmcatal.rda")
names(bmcatal)<- c("ClassV",  "AGBnCata" ,   "AGBmeanCata", "totAGBCata" , "Class" )
bmcatalt.all <- merge(bmcatal, bmcatalAll)

bmcatalt.final   <- tibble(ClassV=bmcatalt.all$ClassV)
bmcatalt.final$`Corine Land Cover Plus 2023 Class` <- bmcatalt.all$Class
bmcatalt.final$`Catalonia Tot. Area sum (ha)` <-  format(bmcatalt.all$AGBnCata / 100, big.mark = "'")
bmcatalt.final$`Catalonia Tot. Biomass  (Mg)` <- format( round(bmcatalt.all$AGBmeanCata * bmcatalt.all$AGBnCata / 100), big.mark = "'")
bmcatalt.final$`EU* Tot. Area sum (ha)` <-  format(bmcatalt.all$`All EU 2023 Tot. Area sum (ha)`, big.mark = "'")
bmcatalt.final$`EU* Tot. Biomass  (Mg)` <- format( round(bmcatalt.all$`All EU 2023 Tot. Area sum (ha)` * bmcatalt.all$AGBmean  ), big.mark = "'")

writexl::write_xlsx(list(Values=bmcatalt.all, Table=bmcatalt.final), "tot.xlsx")


clcrA <-   terra::rast("/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023/CLMS_CLCplus_RASTER_2023.tif")
bm3035A <-   terra::rast("/archivio/shared/geodati/raster/Biomass/ESACCI_2022_Biomass10m3035.tif")
bmcatalAll.avg <- terra::zonal( bm3035A,  clcrA, fun="mean")
names(bmcatalAll.avg)<- c("ClassV", "AGBmean" )
save(bmcatalAll.avg, file="bmcatalAll.avg.rda")
#
#
#
#
# tot <- merge(bmcatalt,
#              tibble( CLCn= 1:length(clcAll[-1]),
#                        "All EU 2023 Tot. Area sum (ha)"=
#                        format(as.numeric(unlist(clcAll[-1])),
#                               big.mark= "'")
#                     )
#              )
#
# writexl::write_xlsx(tot, "tot.xlsx")
# save(tot, file="tot.rda")
# # bmcatalt$`All EU 2023 Tot. Area sum (ha)`<- format( tot$AGBmeanTotEU , big.mark = "'")
#
#
#

 tableHTML(bmcatalt.final,rownames = F,footer = "* Check CLC+ 2023 manual for specific information on which countires are included (e.g. Turkey is included) " )
#
#
# # save(bmcatalWithNames, file="AllAreasBiomass.rda")
