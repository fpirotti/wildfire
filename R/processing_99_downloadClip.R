library(this.path)
source(file.path(this.path::this.dir(), "000_global.R"))
# 1. Set your file paths

clipper <- "/archivio/shared/geodati/wildfire/zenodo/vector_PilotRegions.gpkg"
output_folder <- "/archivio/shared/geodati/wildfire/zenodoV11"
v <- vect(clipper) # Can be a Shapefile, GeoPackage, etc.
## DOWNLOAD ----
# 1. Define variables
target_folder_name <- "WildfireFM"
local_destination <- "/archivio/shared/geodati/wildfire/zenodoV11precursors"

# Create local folder if it doesn't exist
if(!dir.exists(local_destination)) dir.create(local_destination)

# 2. Find ALL folders matching your target name
# We specifically filter for the Google Drive folder mimeType
folders <- drive_find(
  q = paste0("name = '", target_folder_name, "' and mimeType = 'application/vnd.google-apps.folder'"),
  type = "folder"
)

# Check how many folders were found
cat("Found", nrow(folders), "folders matching the name:", target_folder_name, "\n")

# 3. Loop through each folder using its unique ID
for (i in 1:nrow(folders)) {
  folder_id <- folders$id[i]

  cat("Processing folder index", i, "with ID:", folder_id, "\n")

  # List all .tif files inside this specific folder
  tif_files <- drive_ls(
    path = as_id(folder_id),
    pattern = "\\.tif$"  # Regex to find files ending in .tif
  )

  if (nrow(tif_files) == 0) {
    cat("  No .tif files found in this folder.\n")
    next
  }

  # Loop through and download each .tif file
  for (j in 1:nrow(tif_files)) {
    file_to_download <- tif_files[j, ]

    # Construct a unique local file name to prevent files from overwriting each other
    # (Since multiple folders might have files with identical names)
    local_file_path <- file.path(
      local_destination,
      file_to_download$name
    )

    cat("  Downloading:", file_to_download$name, "->", local_file_path, "\n")

    # Execute download
    drive_download(
      file = as_id(file_to_download$id),
      path = local_file_path,
      overwrite = TRUE
    )
  }
}

cat("All downloads complete!")

# for(input_folder in list.dirs(local_destination, recursive = F)){
#
#   template_path <- list.files(input_folder, full.names = T, pattern = "FuelModel\\.tif$", recursive = T)
#   output_folder <- file.path(input_folder, "aligned")
#
#   # Create output folder if it doesn't exist
#   if (!dir.exists(output_folder)) dir.create(output_folder)
#   else{
#     unlink(output_folder,recursive = T)
#     dir.create(output_folder)
#   }
#
#   # 2. Load the template raster
#   # This defines the target CRS, resolution, and extent
#   template <- rast(template_path)
#
#   # 3. List all raster files in the input folder
  raster_files_fuels <- list.files(local_destination, pattern = ".*Fuel.*\\.tif$", full.names = TRUE)
  # lut <- read.csv("https://raw.githubusercontent.com/fpirotti/C2F-W/refs/heads/main/data/ScottAndBurgan/Zona_60-tif/spain_lookup_table.csv")
  # 4. Loop through and align
  for (f in raster_files_fuels) {
    r <- mask(rast(f), v)
    out_name <- file.path(output_folder, basename(f))
    r[r==0] <- NA
    writeRaster(r, out_name,
                datatype = "INT2U",
                gdal = c(
                  "DRIVER=COG",             # Enforce COG format
                  "OVERVIEWS=AUTO",         # Generate internal pyramids
                  "RESAMPLING=NEAREST",     # CRITICAL: Keep discrete categories intact during pyramiding
                  "COMPRESS=LZW"            # Best lossless compression for categories
                ),
                overwrite = TRUE)


    next
    raster_files <- list.files(local_destination,
                               pattern = sprintf("%s.*\\.tif$", substr(basename(f),1,19)),
                               full.names = TRUE)
    rst <- setdiff(raster_files, f)
    for (f2 in rst) {
      r2 <- mask(rast(f2), v)
      out_name2 <- file.path(output_folder, basename(f2))

      perfect_match <- compareGeom(r, r2, stopOnError = FALSE, messages = TRUE)

      if (perfect_match) {
        print(paste(basename(f2), " safe to use! The rasters align perfectly."))
      } else {
        browser()
        print("STOP! The rasters do not align. Check the warnings above.")
      }

      writeRaster(r2, out_name2,
                  datatype = "FLT4S",
                  gdal = c(
                    "DRIVER=COG",             # Enforce COG format
                    "OVERVIEWS=AUTO",         # Generate internal pyramids
                    "RESAMPLING=BILINEAR",   # CRITICAL: Smooth averaging for continuous heights
                    "COMPRESS=DEFLATE",      # Excellent compression for floating-point data
                    "PREDICTOR=3"            # Standard GDAL optimization for floating-point compression
                  ),
                  overwrite = TRUE)


    }

    message(paste("Processed:", basename(f)))
  }

#   print("Alignment complete.")
# }

