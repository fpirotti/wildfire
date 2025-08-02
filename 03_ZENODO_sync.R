
library(zen4R)
zenodo_sandbox <- ZenodoManager$new(
  url = "https://sandbox.zenodo.org/api",
  token = Sys.getenv("TOKEN_ZENODO_SANDBOX"),
  logger = "INFO"
)

zenodo <- ZenodoManager$new(
  url = "https://zenodo.org/api",
  token = Sys.getenv("TOKEN_ZENODO"),
  logger = "INFO"
)

myrec <- ZenodoRecord$new()
myrec$setTitle("CLCplus Backbone 2023 (raster 10 m), Europe, 2-yearly, May 2025")
myrec$setDescription("Here we aggregate the CLC+ 2023 in a single 1 byte raster
file for easier download.  ")
myrec$setResourceType("dataset")
myrec$setPublisher("Francesco Pirotti")
myrec$setPublicationDate(as.character(Sys.Date()))
myrec$addCreator(firstname = "Francesco", lastname = "Pirotti",
                 orcid="0000-0002-4796-6406")

myrec$setLicense("cc-by-1.0")
myrec$setAccessPolicyRecord(access =  "public" )
myrec <- zenodo$depositRecord(myrec)
poll <- zenodo$uploadFile("/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023/CLMS_CLCplus_RASTER_2023.tif",
                          record=myrec  )

myrecst <- zenodo$publishRecord(myrec$getId())

