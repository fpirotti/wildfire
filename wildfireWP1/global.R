library(DT)
library(shiny)
# Load required libraries
library(dplyr)
library(tools)  # For file extension extraction
library(sf)
library(osmextract)

# Read OSM administrative boundaries
# italy_osm <- oe_read("data/europe-latest.osm.pbf", layer = "multipolygons")

# Filter only administrative boundaries (municipalities = admin_level 8)
# italy_municipalities <- italy_osm[italy_osm$admin_level == "8", ]

is_supported <- tryCatch({
  sf::st_read(file_path, quiet = TRUE)
  TRUE  # If successful
}, error = function(e) {
  FALSE  # If reading fails
})

# Define the root folder to search
root_folder <- "/archivio/home/pirotti/Nextcloud/Wildfire/SpatialData"  # Change this to your folder path

# List all files recursively
all_files <- list.files(root_folder, recursive = TRUE, full.names = TRUE)

# Define spatial data extensions of interest
spatial_extensions <- c("tif", "shp", "gpkg", "nc", "grd", "asc", "vrt", "geojson", "kml")
# Define spatial data extensions of interest
spatial_extensions_exclude <- c("dbf","shx", "prj", "cpg",
                                "gdbtablx",
                                "horizon",
                                "spx",
                                "gdbindexes",
                                "gdbtabl",
                                "qmd", "sbx", "sbn")

# Filter for spatial data files
spatial_files <- all_files[!(file_ext(all_files) %in% spatial_extensions_exclude)]

# Create a data frame with metadata
spatial_data_table <- data.frame(
  File_Path = spatial_files,
  File_Name = basename(spatial_files),
  Extension = file_ext(spatial_files),
  Directory = dirname(substr(gsub(root_folder, "", spatial_files), 2, 300)),
  Country = substr(gsub(root_folder, "", spatial_files), 2, 3),
  File_Size_MB = round(file.info(spatial_files)$size / (1024^2), 2)  # Convert bytes to MB
)
