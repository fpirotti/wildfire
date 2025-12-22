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

img <- rgee::ee$Image("projects/progetto-eu-h2020-cirgeo/assets/eu/dtm_elev_lowestmode_gedi_v03")

# FeatureCollections and Images
pilotRegions <- ee$FeatureCollection(
  "projects/progetto-eu-h2020-cirgeo/assets/wildfire/pilotRegions"
)

pilotSites <- ee$FeatureCollection(
  "projects/progetto-eu-h2020-cirgeo/assets/wildfire/wildfire_pilot_sites_v3"
)

### Pilot sites list -----------
ps_list <- pilotSites$toList(pilotSites$size())
n <- pilotSites$size()$getInfo()

for (i2 in seq_len(n) - 1) {

  feat <- ee$Feature(ps_list$get(i2))
  # nm <- feat$get("ID")$getInfo()
  nm <- feat$get("pilot_id")$getInfo()
  geom <- feat$geometry()$buffer(100, 1)

  dem <- img$
    clip(geom)$divide(10L)

  task <- ee_image_to_drive(
    image       = dem$toInt16(),
    description = paste0(nm, "_dem"),
    folder      = paste0("pilotSites_", nm),
    region      = geom,
    scale       = 30,
    timePrefix = F,
    crs         = "EPSG:3035",
    maxPixels   = 1e13
  )
  task$start()

  slopeDeg = ee$Terrain$slope(dem);
  slopePct = slopeDeg$multiply( pi / 180)$tan()$multiply(100);

  task <- ee_image_to_drive(
    image       = slopePct,
    description = paste0( nm, "_slopePercent"),
    folder      = paste0("pilotSites_", nm),
    region      = geom,
    scale       = 30,
    timePrefix = F,
    crs         = "EPSG:3035",
    maxPixels   = 1e13
  )
  task$start()
  aspect = ee$Terrain$aspect(dem);
  task <- ee_image_to_drive(
    image       = aspect,
    description = paste0( nm, "_aspectDegrees"),
    folder      = paste0("pilotSites_", nm),
    region      = geom,
    scale       = 30,
    timePrefix = F,
    crs         = "EPSG:3035",
    maxPixels   = 1e13
  )
  task$start()

}
