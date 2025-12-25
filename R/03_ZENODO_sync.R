
library(zen4R)
zenodo <- ZenodoManager$new(
  url = "https://sandbox.zenodo.org/api",
  token = Sys.getenv("TOKEN_ZENODO_SANDBOX"),
  logger = "INFO"
)

zenodo <- ZenodoManager$new(
  url = "https://zenodo.org/api",
  token = Sys.getenv("TOKEN_ZENODO"),
  logger = "INFO"
)

 myrec <- zenodo$getRecordById(7367871)

 myrecLatest <- zenodo$getRecordByDOI("10.5281/zenodo.7367871")
# myrec2 <- zenodo$ (myrec$getId() )
# myrec <- ZenodoRecord$new()
myrec$setTitle("Latest High Resolution Layers (HRL) from Copernicus Land Cover Services")
myrec$setDescription("We aggregate some High Resolution Layers (HRL) from the Copernicus Land Cover Service using either 1 or 2 byte rasters
to save space and make for an  easier download.
These data are provided by different Data and Information Access services (DIAS) and also by tile.
Nevertheless it is sometimes better to download the whole grid as one file, instead of the singl 799 tiles to cover the EU contries.
All data are aligned in the EPSG:3035 grid coordinate system.
Each layer has its respective confidence layer.
  - CLCplus Backbone 2023 (raster 10 m)
  - Tree Cover Density 2021 (raster 10 m)")
myrec$setResourceType("dataset")
myrec$setPublisher("Francesco Pirotti")
myrec$setPublicationDate(as.character(Sys.Date()))
# myrec$addCreator(firstname = "Francesco", lastname = "Pirotti",
#                  orcid="0000-0002-4796-6406")

myrec$setLicense("cc-by-1.0")
myrec$setPublicationDate(as.character(Sys.Date()))
myrec$setAccessPolicyRecord(access =  "public" )

myrec2 <- zenodo$depositRecordVersion(myrec,
                                      files = "/archivio/shared/geodati/raster/CLMS_TCF_TreeDensity_RASTER_2021/CLMS_TCF_TreeDensity_RASTER_2021.tif",
                                      delete_latest_files = F)

poll <- zenodo$uploadFile("/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023/CLMS_CLCplus_RASTER_2023.tif",
                          record=myrec  )

#
# poll <- zenodo$uploadFile("/archivio/shared/geodati/raster/CLMS_TCF_TreeDensity_RASTER_2021/CLMS_TCF_TreeDensity_RASTER_2021.tif",
#                           record=myrec2  )
myrecst <- zenodo$publishRecord(myrec2$getId())

