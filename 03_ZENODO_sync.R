
library(zen4R)
zenodo <- ZenodoManager$new(
  url = "https://sandbox.zenodo.org/api",
  token = Sys.getenv("TOKEN_ZENODO"),
  logger = "INFO"
)

myrec <- ZenodoRecord$new()
myrec$setTitle("CLCplus Backbone 2023 (raster 10 m), Europe, 2-yearly, May 2025")
myrec$setDescription("A description of my R package")
myrec$setUploadType("software")
myrec$addCreator(firstname = "Francesco", lastname = "Pirotti",
                 orcid="0000-0002-4796-6406")
myrec$setLicense("mit")

myrec <- zenodo$depositRecord(myrec)

print(zenodo)
