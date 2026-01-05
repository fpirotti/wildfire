
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


myrec <- zenodo$getDepositions()
myrec <- zenodo$getRecordById(18077813)
myrec$
files <- myrec$files
downloaded <- (list.files("output/pilotRegions/", full.names = T))
nd <- which(! basename(downloaded) %in% names(files))
toupload <- grep("gpkg", downloaded[nd],value = T )

myrecEd <- zenodo$depositRecordVersion(record = myrec , delete_latest_files = FALSE)
myrecEd$
for (fto in toupload){
  poll <- zenodo$uploadFile(fto,
                            record=myrecEd  )
}

myrecEd$setPublicationDate(as.character(Sys.Date()))
myrecst <- zenodo$depositRecord(myrecEd, publish=TRUE)

myrec2 <- zenodo$depositRecordVersion(myrec,
                                      files = "/archivio/shared/geodati/raster/CLMS_TCF_TreeDensity_RASTER_2021/CLMS_TCF_TreeDensity_RASTER_2021.tif",
                                      delete_latest_files = F)

poll <- zenodo$uploadFile("/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023/CLMS_CLCplus_RASTER_2023.tif",
                          record=myrec  )

#
# poll <- zenodo$uploadFile("/archivio/shared/geodati/raster/CLMS_TCF_TreeDensity_RASTER_2021/CLMS_TCF_TreeDensity_RASTER_2021.tif",
#                           record=myrec2  )


