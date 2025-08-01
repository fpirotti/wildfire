
library(zen4R)
zenodo <- ZenodoManager$new(
  url = "https://sandbox.zenodo.org/api",
  token = Sys.getenv("TOKEN_ZENODO_SANDBOX"),
  logger = "INFO"
)

myrec <- ZenodoRecord$new()
myrec$setTitle("CLCplus Backbone 2023 (raster 10 m), Europe, 2-yearly, May 2025")
myrec$setDescription("A description of my R package")
myrec$setResourceType("dataset")
myrec$setPublisher("Francesco Pirotti")
myrec$setPublicationDate(as.character(Sys.Date()))
myrec$addCreator(firstname = "Francesco", lastname = "Pirotti",
                 orcid="0000-0002-4796-6406")

myrec$setLicense("cc-by-1.0")
myrec$setAccessPolicyRecord(access =  "public" )
myrec <- zenodo$depositRecord(myrec)
poll <- zenodo$uploadFile("00_globals.R", record=myrec  )

myrecst <- zenodo$publishRecord(myrec$getId())

