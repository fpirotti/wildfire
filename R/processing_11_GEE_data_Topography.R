library(rgee)
library(rgeeExtra)
library(stars)
library(googledrive)

### setting version ----
versionFuelModel  = 3

########### THIS REQUIRES FIRST THAT THE processing_01_GEE_tileMeta.R!
# 1. Authenticate ----
drive_auth(email = "cirgeo@unipd.it")
ee_Initialize(user = 'cirgeo'  )
proj_3035_30m <- list(
  crs = "EPSG:3035",
  crsTransform = c(30, 0, 4321000, 0, -30, 3210000)
)

img <- rgee::ee$Image("projects/progetto-eu-h2020-cirgeo/assets/eu/dtm_elev_lowestmode_gedi_v03")

# FeatureCollections and Images
pilotRegions <- ee$FeatureCollection(
  "projects/progetto-eu-h2020-cirgeo/assets/wildfire/pilotRegions"
)

pilotSites <- ee$FeatureCollection(
  "projects/progetto-eu-h2020-cirgeo/assets/wildfire/wildfire_pilot_sites_v3"
)

### Pilot sites list -----------


for(reg in c("pilotRegions", "pilotSites")){
  obj <- get(reg)
  ps_list <- obj$toList(obj$size())
  n <- obj$size()$getInfo()
  tp = reg

  for (i2 in seq_len(n) - 1) {

    feat <- ee$Feature(ps_list$get(i2))
    # nm <- feat$get("ID")$getInfo()
    inf <- feat$get("pilot_id")$getInfo()
    if(is.null(inf)){
      inf <- feat$get("ID")$getInfo()
    }
    nm <- paste0(tp, "_", inf, "_topography" )
    geom <- feat$geometry()$buffer(90, 1)

    dem <- img$
      clip(geom)$divide(10L)$toFloat()$resample('bilinear')


    task <- ee_image_to_drive(
      image       = dem,
      description = paste0(nm, "_dem"),
      folder      = paste0(nm),
      region      = geom,
      scale       = 30,
      timePrefix = F,
      formatOptions =   list( cloudOptimized= TRUE),
      crs         = proj_3035_30m$crs,
      crsTransform = proj_3035_30m$crsTransform,
      maxPixels   = 1e13
    )
    task$start()

    slopeDeg = ee$Terrain$slope(dem);
    slopePct = slopeDeg$multiply( pi / 180)$tan()$multiply(100);

    task <- ee_image_to_drive(
      image       = slopePct,
      description = paste0( nm, "_slopePercent"),
      folder      = paste0( nm),
      region      = geom,
      scale       = 30,
      timePrefix = F,
      formatOptions =   list( cloudOptimized= TRUE),
      crs         = proj_3035_30m$crs,
      crsTransform = proj_3035_30m$crsTransform,
      maxPixels   = 1e13
    )
    task$start()
    aspect = ee$Terrain$aspect(dem);
    task <- ee_image_to_drive(
      image       = aspect,
      description = paste0( nm, "_aspectDegrees"),
      folder      = paste0(  nm),
      region      = geom,
      scale       = 30,
      timePrefix = F,
      crs         = proj_3035_30m$crs,
      crsTransform = proj_3035_30m$crsTransform,
      formatOptions =   list( cloudOptimized= TRUE),
      maxPixels   = 1e13
    )
    task$start()

  }
}
