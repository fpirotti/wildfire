library(rgee)
library(rgeeExtra)
library(googledrive)

## first requires that "___GEE_tileMeta.R" be run to produce the 10 m resampled height metrics
# ee_Initialize(quiet = T)
ee_Initialize(user = 'cirgeo'  )
pilotSites <- ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/wildfire/wildfire_pilot_sites_v3")

target = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/canopyHeightFromMeta30m';
from = 'users/cirgeo/wildfire/canopyHeightFromMeta10m'
list = ee$data$listAssets(target);
tb <- as.data.frame(do.call(rbind, list$assets))

##  ========================
##  2. Load Facebook Meta canopy-height raster
##  ========================
canopyColl = ee$ImageCollection(from)


# grid = ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/copernicus/EEA_50km_grid_land")
# exportCrs = 'EPSG:3035';  ##  target projection

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

reducers <- ee$List(c(
  ee$Reducer$mean(),
  ee$Reducer$sum(),
  ee$Reducer$min(),
  ee$Reducer$max() )
)


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
  img <- img$addBands(img$select("b1_stdDev")$pow(2), {}, T)
  nm<-tileDict$properties$`system:index`
  if( any(grepl(nm, tb$name)) ) {
    message(nm, " exists")
    next
  }
  message(nm, " DOING")

  assetIds[[nm]] <- file.path(target,nm )

  bandNames = img$bandNames();
  # bandNames$getInfo()
  # pair <- bandNames$zip(reducers)$get(0)
  out <- ee$ImageCollection(
    bandNames$zip(reducers)$map(
      ee_utils_pyfunc(
        function(pair) {

        pair <- ee$List(pair)
        band <- ee$String(pair$get(0))
        reducer <- ee$Reducer(pair$get(1))
        # band$getInfo()
        ee$Image(img$select(band)$reduceResolution(
          reducer= reducer,
          maxPixels= 300,
          bestEffort= T
        ))

      })
    )
  )$toBands()$rename(bandNames)

  out <- out$addBands(out$select("b1_stdDev")$sqrt(), {}, T)

   # ff<-(out$getInfo())
  # ff2<-(out$toBands()$getInfo())
  ee_image_to_asset(
    image= ee$Image(out)$toByte(),
    description= nm,
    assetId= assetIds[[nm]],
    region= img$geometry(),
    scale= 30,
    crs= 'EPSG:3035',
    maxPixels= 1e13
  )$start();
}


for(ids in assetIds) {
  ee$data$deleteAsset(ids )
  }

