library(rgee)
library(rgeeExtra)
library(googledrive)

# ee_Initialize(quiet = T)
ee_Initialize(user = 'cirgeo'  )
pilotSites <- ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/wildfire/wildfire_pilot_sites_v3")
assetRoot = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire';
target = 'users/cirgeo/wildfire/canopyHeightFromMeta10m';

list = ee$data$listAssets(target);
tb <- as.data.frame(do.call(rbind, list$assets))

# grid = ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/copernicus/EEA_50km_grid_land")
exportCrs = 'EPSG:3035';  ##  target projection

  ##  ========================
    ##  2. Load Facebook Meta canopy-height raster
  ##  ========================
canopyColl = ee$ImageCollection("projects/sat-io/open-datasets/facebook/meta-canopy-height")$filterBounds(pilotSites$bounds());

  ##  Create mosaic using the 'cover_code' band
canopy = ee$Image(canopyColl$mosaic())$select('cover_code')$rename("b1")$setDefaultProjection(canopyColl$first()$projection());  ##  keep native CRS
combinedReducer = ee$Reducer$mean()$combine(
  reducer2= ee$Reducer$stdDev(),
  sharedInputs= T
  )$combine(
    reducer2= ee$Reducer$min(),
    sharedInputs= T
  )$combine(
      reducer2= ee$Reducer$max(),
      sharedInputs= T
  );

##  3$ Aggregate to 30 m  ----
canopy10m = canopy$reduceResolution(
    reducer= combinedReducer,
    maxPixels= 300,
    bestEffort= T
);

##  4. Tile Europe in EPSG:3035 coordinates ----
# filteredGrid = grid$filter(ee$Filter$eq('wildfire', 1));
# filteredGridList = filteredGrid$toList(filteredGrid$size());
# ntiles = filteredGrid$size()$getInfo();

ntiles = pilotSites$size()$getInfo();
nImagesInTolanCollection <- canopyColl$size()$getInfo()
assetIds <- list()
# for( i in 1:ntiles){
 for( i in 1:nImagesInTolanCollection){
  img <- ee$Image(canopyColl$toList(nImagesInTolanCollection)$get((i-1)))
  tileDict = img$getInfo();

  nm<-tileDict$properties$`system:index`
  if( any(grepl(nm, tb$name)) ) {
    message(nm, " exists")
    next
  }
  message(nm, " DOING")

  assetIds[[nm]] <- paste0(target,nm )

  ee_image_to_asset(
    image= ee$Image(canopy10m)$toByte(),
    description= nm,
    assetId= assetIds[[nm]],
     region= img$geometry(),
    scale= 10,
    # crs= 'EPSG:3035',
    maxPixels= 1e13
  )$start();
}


for(ids in assetIds) {
  ee$data$deleteAsset(ids )
  }

