
output_base_dir <- "/archivio/shared/geodati/raster"
ncores <- min(40, max(1,abs(parallel::detectCores()-2) ) )
filename <- function(fullname){
  basename(tools::file_path_sans_ext(fullname))
}

sameFileNoExt <- function(fullname, fullname2){
  filename(fullname) == filename(fullname2)
}

`%+%` <- function(a, b) paste0(a, b)

logfile <- "logs/"%+%Sys.getpid()%+%"_logMessage.log"
warnfile <- "logs/"%+%Sys.getpid()%+%"_logWarning.log"

if(file.exists(logfile)) file.remove(logfile)
if(file.exists(warnfile))file.remove(warnfile)

warning_log <- function(...,   call. = TRUE, immediate. = FALSE) {
  # Write to logfile
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", paste(..., concatenate=" "), "\n", file = warnfile, append = TRUE)
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", paste(..., concatenate=" "), "\n", file = logfile, append = TRUE)

  warning(..., call. = call., immediate. = immediate.)
}
message_log <- function(..., call. = TRUE, immediate. = FALSE) {
  # Write to logfile
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "-", paste(..., concatenate=" "), "\n", file = logfile, append = TRUE)
  # Emit warning
  message(...)
}


### COPERNICUS DATA -----
query <- list(

  "CLMS_CLCplus_RASTER_2023"=list(
    "dataset_id"=  "EO:EEA:DAT:CLC-PLUS",
    "product_type"=  "Raster Layer",
    "resolution"=  "10m",
    "year"=  "2023",
    "type"="Byte"
  ),
  "CLMS_CLCplus_RASTER_2023confidence"= list(
    "dataset_id"=  "EO:EEA:DAT:CLC-PLUS",
    "product_type"=  "Confidence Layer",
    "resolution"=  "10m",
    "year"=  "2023",
    "type"="Byte"
  ),
  "CLMS_TCF_TreeDensity_RASTER_2023" = list(
    "dataset_id"= "EO:EEA:DAT:HRL:TCF",
    "product_type"="Tree Cover Density",
    "resolution"= "10m",
    "year"= "2023",
    "type"="Byte"
  ),
  "CLMS_TCF_TreeDensity_RASTER_2021" = list(
    "dataset_id"= "EO:EEA:DAT:HRL:TCF",
    "product_type"="Tree Cover Density",
    "resolution"= "10m",
    "year"= "2021",
    "type"="Byte"
  ),
  "CLMS_TCF_TreeDensity_RASTER_2021_Conf" = list(
    "dataset_id"=  "EO:EEA:DAT:HRL:TCF",
    "product_type"=  "Tree Cover Density Confidence Layer",
    "resolution"=  "10m",
    "year"=  "2021",
    "type"="Byte"
  ),

  "CLMS_TCF_DominantLeafType_RASTER_2021" = list(
    "dataset_id"=  "EO:EEA:DAT:HRL:TCF",
    "product_type"=  "Dominant Leaf Type",
    "resolution"=  "10m",
    "year"=  "2021",
    "type"="Byte"
  ),

  "CLMS_TCF_DominantLeafType_RASTER_2021_Conf" = list(
    "dataset_id"=  "EO:EEA:DAT:HRL:TCF",
    "product_type"=  "Dominant Leaf Type Confidence Layer",
    "resolution"=  "10m",
    "year"=  "2021",
    "type"="Byte"
  ),

  "CLMS_CropTypes_RASTER_2021" = list(
    "dataset_id"=  "EO:EEA:DAT:HRL:CRL",
    "product_type"=  "Crop Types",
    "resolution"=  "10m",
    "year"=  "2021",
    "type"="ConvToByte"
  ),

  "CLMS_CropTypes_RASTER_2021_Conf" = list(
    "dataset_id"= "EO:EEA:DAT:HRL:CRL",
    "product_type"= "Crop Types Confidence Layer",
    "resolution"=  "10m",
    "year"=  "2021",
    "type"="ConvToByte"
  ),

  "CLMS_Grassland_RASTER_2021" = list(
    "dataset_id"=  "EO:EEA:DAT:HRL:GRA",
    "product_type"=  "Grassland",
    "resolution"=  "10m",
    "year"=  "2021"
  ),

  "CLMS_Grassland_RASTER_2021_Conf" = list(
    "dataset_id"= "EO:EEA:DAT:HRL:GRA",
    "product_type"= "Grassland Confidence Layer",
    "resolution"=  "10m",
    "year"=  "2021"
  ) #,
  # "CLMS_SurfaceSoilMoisture_2024"=list(
  #   "dataset_id"= "EO:CLMS:DAT:CLMS_GLOBAL_SSM_1KM_V1_DAILY_NETCDF",
  #   "productType"= "SSM1km",
  #   "resolution"= "1000",
  #   "startdate"= "2023-01-01T00:00:00.000Z",
  #   "enddate"= "2025-01-01T23:59:59.999Z"
  # )
)



makeGridStars <- function(){
  if(!require("stars")){install.packages("stars")}
  # Define the extent in EPSG:3035 (in meters)
  # Rough bounding box covering most of Europe
  bbox_3035 <- st_bbox(c(
    xmin = 2500000,  # western Portugal
    xmax = 7500000,  # eastern edge of Europe
    ymin = 1300000,  # southern edge (Greece)
    ymax = 5500000   # northern edge (Norway)
  ), crs = st_crs(3035))

  # Define dimensions (1 km = 1000 meters)
  res <- 1000
  nx <- (bbox_3035$xmax - bbox_3035$xmin) / res
  ny <- (bbox_3035$ymax - bbox_3035$ymin) / res

  # Create stars object with NA values (or 0s, or any dummy data)
  grid <- st_as_stars(
    st_dimensions(
      x = seq(bbox_3035$xmin + res/2, bbox_3035$xmax - res/2, by = res),
      y = seq(bbox_3035$ymax - res/2, bbox_3035$ymin + res/2, by = -res),  # note decreasing y
      raster = st_raster_dimension(x = "x", y = "y", dimensions = "raster")
    ),
    values = NA_real_
  )

  # Assign CRS
  st_crs(grid) <- 3035
  grid
}
