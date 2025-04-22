
library("sf")


sheetUrl = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRNluJSVc0aOzqLHRp_oomgCvQN8BFtZQkDvPk4aOnd592mosFUTxiYqis5A46w7-6U6FcnuBE2AXXt/pub?gid=0&single=true&output=csv';

validationPoints = read.csv(sheetUrl)
validationPoints$lat <- as.numeric(validationPoints$lat)
validationPoints$lon <- as.numeric(validationPoints$lon)
validationPoints <- na.omit(validationPoints)

load(file="validationPoints.rds")

if(identical( validationPoints.bkp, validationPoints)){
  cat(as.character(date()) , "------No updates\n", file = "updateValidationLog.log")
} else {

  library(rgee)
  ee_Initialize(user = 'cirgeo' )


  validationPoints.bkp <- validationPoints
  save(validationPoints.bkp, file="validationPoints.rds")

  cat(as.character(date()) , "Yes updates....\n", file = "updateValidationLog.log")

  points_sf <- st_as_sf(validationPoints, coords = c("lon", "lat"), crs = 4326)

  points_ee <- sf_as_ee(points_sf)

  assetid <-  'projects/progetto-eu-h2020-cirgeo/assets/wildfire/validationPoints'

  cat( "Starting upload asset\n", file = "updateValidationLog.log",append = T)
  task <- ee_table_to_asset(
    collection = points_ee,
    assetId = assetid,
    overwrite = TRUE  # optional
  )

  task$start()

  while (task$status()$state %in% c('READY', 'RUNNING')) {
    cat("Task status:", task$status()$state, "\n", file = "updateValidationLog.log",append = T)
    Sys.sleep(10)
  }

  bb <- system(sprintf("earthengine acl set public %s", assetid),intern = T)
  cat(bb, "\n", file = "updateValidationLog.log",append = T)

}


