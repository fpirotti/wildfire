# Load required libraries
library(dplyr)
library(tools)  # For file extension extraction



# Define the root folder to search
root_folder <- "/archivio/home/pirotti/Nextcloud/Wildfire/SpatialData"  # Change this to your folder path

# List all files recursively
all_files <- list.files(root_folder, recursive = TRUE, full.names = TRUE)

# Define spatial data extensions of interest
spatial_extensions <- c("tif", "shp", "gpkg", "nc", "grd", "asc", "vrt", "geojson", "kml")

# Filter for spatial data files
spatial_files <- all_files#[file_ext(all_files) %in% spatial_extensions]

# Create a data frame with metadata
spatial_data_table <- data.frame(
  File_Path = spatial_files,
  File_Name = basename(spatial_files),
  Extension = file_ext(spatial_files),
  Directory = basename(dirname(spatial_files)),
  Country = substr(gsub(root_folder, "", spatial_files), 2, 3),
  File_Size_MB = round(file.info(spatial_files)$size / (1024^2), 2)  # Convert bytes to MB
)

# Print summary table

