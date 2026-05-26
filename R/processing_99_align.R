library(terra)

source("R/000_global.R")
# 1. Set your file paths
input_folders <- "/archivio/shared/geodati/wildfire/pilotSites"

for(input_folder in list.dirs(input_folders, recursive = F)){

  template_path <- list.files(input_folder, full.names = T, pattern = "FuelModel\\.tif$", recursive = T)
  output_folder <- file.path(input_folder, "aligned")

  # Create output folder if it doesn't exist
  if (!dir.exists(output_folder)) dir.create(output_folder)
  else{
    unlink(output_folder,recursive = T)
    dir.create(output_folder)
  }

  # 2. Load the template raster
  # This defines the target CRS, resolution, and extent
  template <- rast(template_path)

  # 3. List all raster files in the input folder
  raster_files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

  # 4. Loop through and align
  for (f in raster_files) {

    # Load current raster
    r <- rast(f)

    # Align (Resample) to template
    # Method 'bilinear' for continuous data; use 'near' for categorical/discrete
    aligned_r <- resample(r, template, method = "bilinear")

    # Define output filename
    out_name <- file.path(output_folder, substr(basename(f), 18,56))

    # Save to disk
    writeRaster(aligned_r, out_name, overwrite = TRUE)

    message(paste("Processed:", basename(f)))
  }

  print("Alignment complete.")
}

