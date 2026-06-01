library(this.path)
source(file.path(this.path::this.dir(), "000_global.R"))
# 1. Set your file paths
asset_path <- "projects/progetto-eu-h2020-cirgeo/assets/wildfire/AT_test_case_wildfire2025"
ee_polygon <- ee$FeatureCollection(asset_path)

# 3. Bring it into R as a local 'sf' object (as requested)
sf_polygon <- ee_as_sf(ee_polygon)

input_folder <- "/archivio/shared/geodati/wildfire/zenodoV12"
local_destination <- file.path("/archivio/shared/geodati/wildfire/fireBehaviour",basename(asset_path))

# Create local folder if it doesn't exist
if(!dir.exists(local_destination)) dir.create(local_destination, recursive = T)



  raster_files_fuels <- list.files(input_folder, pattern = ".*Fuel.*\\.tif$", full.names = TRUE)
  # lut <- read.csv("https://raw.githubusercontent.com/fpirotti/C2F-W/refs/heads/main/data/ScottAndBurgan/Zona_60-tif/spain_lookup_table.csv")
  # 4. Loop through and align
  for (f in raster_files_fuels) {
    local_raster <- rast(f)
    sf_polygon_proj <- st_transform(sf_polygon[1], st_crs(local_raster))
    expanded_extent <- returnBufferedBounds(local_raster,sf_polygon)
    # 5. Crop the local raster to the expanded 500-pixel bounding box
    clipped_raster <-  tryCatch({
      crop(local_raster, expanded_extent)
    }, error=function(e){
      message(e$message)
      NULL
    })
    if(is.null(clipped_raster)){
      next
    }
    plot(clipped_raster)
    plot(sf_polygon_proj, add=T)
    clipped_raster[clipped_raster==0] <- NA
    # 6. Optional: Plot the result to verify
    out_name <- file.path(local_destination,"fuel.tif")
    writeRaster(clipped_raster, out_name,
                datatype = "INT2U",
                overwrite = TRUE)

    raster_files <- list.files(input_folder,
                               pattern = sprintf("%s.*\\.tif$", substr(basename(f),1,19)),
                               full.names = TRUE)
    rst <- setdiff(raster_files, f)
    for (f2 in rst) {
      r2 <- crop(rast(f2), clipped_raster)
      outname<- NULL
      if(grepl("slope", basename(f2), ignore.case = T)) outname<-"slope.tif"
      if(grepl("aspect", basename(f2), ignore.case = T)) outname<-"saz.tif"
      if(grepl("dem|dtm|dsm|elevation", basename(f2), ignore.case = T)) outname<-"dem.tif"
      if(grepl("canopy", basename(f2), ignore.case = T)){
        if(grepl("BaseHeight|cbh", basename(f2), ignore.case = T)) outname<-"cbh.tif"
        if(grepl("bulk|density|cbd", basename(f2), ignore.case = T)) outname<-"cbd.tif"
        if(grepl("cover", basename(f2), ignore.case = T)) outname<-"ccf.tif"
        if(grepl("probability", basename(f2), ignore.case = T)) outname<-"probabilityMap.tif"
        if(grepl("moist|fmc", basename(f2), ignore.case = T)) outname<-"fmc.tif"
        if(grepl("height|chm", basename(f2), ignore.case = T)) outname<-"ch.tif"
      }

      if(is.null(outname) && grepl("ch", basename(f2), ignore.case = T)) outname<-"ch.tif"
      if(is.null(outname)){
        browser()
      }

        out_name2 <- file.path(local_destination, outname)

      perfect_match <- compareGeom(clipped_raster, r2, stopOnError = FALSE, messages = TRUE)

      if (perfect_match) {
        print(paste(basename(f2), " safe to use! The rasters align perfectly."))
      } else {
        browser()
        print("STOP! The rasters do not align. Check the warnings above.")
      }

      writeRaster(r2, out_name2,
                  datatype = "FLT4S",
                  overwrite = TRUE)
    }

    message(paste("Processed:", basename(f)))
  }

#   print("Alignment complete.")
# }

