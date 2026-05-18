library(terra)

# 1. Set your file paths
input_folder <- "path/to/your/rasters"
template_path <- "path/to/template_raster.tif"
output_folder <- "path/to/aligned_rasters"

# Create output folder if it doesn't exist
if (!dir.exists(output_folder)) dir.create(output_folder)

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
  out_name <- file.path(output_folder, paste0("aligned_", basename(f)))

  # Save to disk
  writeRaster(aligned_r, out_name, overwrite = TRUE)

  message(paste("Processed:", basename(f)))
}

print("Alignment complete.")
