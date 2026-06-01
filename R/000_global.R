library(rgee)
library(rgeeExtra)
library(stars)
library(googledrive)
library(this.path)

### setting version ----
versionFuelModel  = 3

returnBufferedBounds <- function(local_raster, sf_polygon){
  pixel_size <- res(local_raster)[1] # Assumes square pixels (x resolution)
  sf_polygon_proj <- st_transform(sf_polygon, st_crs(local_raster))

  # 3. Extract pixel resolution and current polygon bounds
  pixel_size <- res(local_raster)[1]
  poly_bbox  <- st_bbox(sf_polygon_proj)
  # Current dimensions in pixels
  poly_w_px <- as.numeric((poly_bbox["xmax"] - poly_bbox["xmin"]) / pixel_size)
  poly_h_px <- as.numeric((poly_bbox["ymax"] - poly_bbox["ymin"]) / pixel_size)

  # 4. Enforce Boundary Logic (Min: 200px, Max: 900px)
  # Base coordinates start directly at the polygon's edges
  xmin_m <- poly_bbox["xmin"]
  xmax_m <- poly_bbox["xmax"]
  ymin_m <- poly_bbox["ymin"]
  ymax_m <- poly_bbox["ymax"]

  # --- Handle Width (X Axis) ---
  if (poly_w_px < 200) {
    # Expand outward equally from the edges to reach 200 pixels
    pad_x <- ((200 - poly_w_px) / 2) * pixel_size
    xmin_m <- xmin_m - pad_x
    xmax_m <- xmax_m + pad_x
  } else if (poly_w_px > 900) {
    # Crop inward equally from the edges to clamp at 900 pixels
    crop_x <- ((poly_w_px - 900) / 2) * pixel_size
    xmin_m <- xmin_m + crop_x
    xmax_m <- xmax_m - crop_x
  }

  # --- Handle Height (Y Axis) ---
  if (poly_h_px < 200) {
    # Expand outward equally from the edges to reach 200 pixels
    pad_y <- ((200 - poly_h_px) / 2) * pixel_size
    ymin_m <- ymin_m - pad_y
    ymax_m <- ymax_m + pad_y
  } else if (poly_h_px > 900) {
    # Crop inward equally from the edges to clamp at 900 pixels
    crop_y <- ((poly_h_px - 900) / 2) * pixel_size
    ymin_m <- ymin_m + crop_y
    ymax_m <- ymax_m - crop_y
  }

  # 5. Create the final constrained SpatExtent object
  ext(xmin_m, xmax_m, ymin_m, ymax_m)
}
########### THIS REQUIRES FIRST THAT THE processing_01_GEE_tileMeta.R!
# 1. Authenticate ----
drive_auth(email = "cirgeo@unipd.it")
# ee_Initialize(user = 'cirgeo'  )
scott_burgan_models <- c(
  # Grass Models (GR1 - GR9)
  101, 102, 103, 104, 105, 106, 107, 108, 109,
  # Grass-Shrub Models (GS1 - GS4)
  121, 122, 123, 124,
  # Shrub Models (SH1 - SH9)
  141, 142, 143, 144, 145, 146, 147, 148, 149,
  # Timber-Understory Models (TU1 - TU5)
  161, 162, 163, 164, 165,
  # Timber Litter Models (TL1 - TL9)
  181, 182, 183, 184, 185, 186, 187, 188, 189,
  # Slash-Blowdown Models (SB1 - SB4)
  201, 202, 203, 204,
  # Non-Burnable Models (Urban, Ag, Water, Rock)
  91, 92, 93, 98, 99
)
### setting scale ----
# proj3035_30m = ee$Projection('EPSG:3035')$atScale(scale);
proj_3035_30m <- list(
  crs = "EPSG:3035",
  crsTransform = c(30, 0, 4321000, 0, -30, 3210000)
)

proj_3035_30m_ee <- ee$Projection(
  crs = "EPSG:3035",
  transform = c(30, 0, 4321000, 0, -30, 3210000)
)

proj_3035_10m <- list(
  crs = "EPSG:3035",
  crsTransform = c(10, 0, 4321000, 0, -10, 3210000)
)
