library(rgee)
library(rgeeExtra)
library(stars)
library(googledrive)
library(osmdata)
library(sf)
library(dplyr)
### setting version ----
versionFuelModel  = 3

########### THIS REQUIRES FIRST THAT THE processing_01_GEE_tileMeta.R!
# 1. Authenticate ----
drive_auth(email = "cirgeo@unipd.it")
ee_Initialize(user = 'cirgeo'  )


# FeatureCollections and Images
pilotRegions <- ee$FeatureCollection(
  "projects/progetto-eu-h2020-cirgeo/assets/wildfire/pilotRegions"
)

pilotSites <- ee$FeatureCollection(
  "projects/progetto-eu-h2020-cirgeo/assets/wildfire/wildfire_pilot_sites_v3"
)

### Pilot sites list -----------
# "pilotSites",
for(reg in c( "pilotRegions")){
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

    nm <- paste0(tp, "_", inf, "_roads.gpkg" )

    message(nm, " try...")
    if(file.exists(file.path("output", nm))) next

    aoi <- ee_as_sf(feat) |>
                          st_transform(3035) |>
                          st_buffer(100) |>
                          st_transform(4326)

    bbox <- st_bbox(aoi)
    message(nm, " start query")
    q <- opq(bbox = st_bbox(aoi),osm_types = c( "way" ), timeout = 300) |>
      add_osm_feature(key = "highway")

    # ---- DOWNLOAD DATA ----
    osm <- osmdata_sf(q)
    message(nm, " finish query")

    # ---- EXTRACT LINE FEATURES (ROADS) ----
    roads <- osm$osm_lines
    roads <- roads[,"highway"]
    # ---- CLIP TO POLYGON ----
    message(nm, " intersection of query")
    roads_clip <- st_intersection(roads, aoi)
    message(nm, " start clip ")
    output<-  roads_clip   |>
      st_transform(3035)

    message(nm, " start write tmp ")
    sf::write_sf(output,  "output/tmp.shp"  )

    message(nm, " start write gpkg ")
    sf::write_sf(sf::read_sf("output/tmp.shp"), file.path("output", nm) )
  }
}
