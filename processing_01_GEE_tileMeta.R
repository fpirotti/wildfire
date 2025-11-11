library(rgee)
library(rgeeExtra)
library(googledrive)

# ee_Initialize(quiet = T)
ee_Initialize(user = 'cirgeo'  )

assetRoot = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire';
list = ee$data$listAssets(assetRoot);
tb <- as.data.frame(do.call(rbind, list$assets))

grid = ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/copernicus/EEA_50km_grid_land")
exportCrs = 'EPSG:3035';  ##  target projection

  ##  ========================
    ##  2. Load Facebook Meta canopy-height raster
  ##  ========================
canopyColl = ee$ImageCollection("projects/sat-io/open-datasets/facebook/meta-canopy-height")$filterBounds(grid);

  ##  Create mosaic using the 'cover_code' band
canopy = canopyColl$mosaic()$select('cover_code')$rename("b1")$setDefaultProjection(canopyColl$first()$projection());  ##  keep native CRS
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

  ##  ========================
    ##  3$ Aggregate to 30 m
  ##  ========================
    ##  Use mode for categorical data
canopy30m = canopy$reduceResolution(
    reducer= combinedReducer,
    maxPixels= 2048,
    bestEffort= T
);

  ##  ========================
    ##  4. Tile Europe in EPSG:3035 coordinates
  ##  ========================
    ## var europeProj = europe.transform(exportCrs, 1);  ##  reproject geometry for tiling
filteredGrid = grid$filter(ee$Filter$eq('wildfire', 1));
filteredGridList = filteredGrid$toList(filteredGrid$size());
ntiles = filteredGrid$size()$getInfo();

assetIds <- list()
for( i in 1:ntiles){
    ## for(var j=42; j< 52; ++j){
      ## var x1 = i+1;
      ## var y1 = j+1;
    tile = ee$Feature(filteredGridList$get(i-1))## $buffer(-10000);
    tileDict = tile$toDictionary()$getInfo();

    nm = paste0('canopyMeta30m_tile_E',
                 as.numeric(tileDict$eoforigin)/10000, '_N',
                 as.numeric(tileDict$noforigin)/10000);
    assetIds[[nm]] <- paste0('projects/progetto-eu-h2020-cirgeo/assets/wildfire/',nm)
    next
    if( any(grepl(nm, tb$name)) ) next

    ee_image_to_asset(
        image= canopy30m$clip(tile) ,
        description= nm,
        assetId= paste0('projects/progetto-eu-h2020-cirgeo/assets/wildfire/',nm),
        region= tile$geometry(),
        scale= 30,
        ## crs= 'EPSG:3035',
        maxPixels= 1e13
      )$start();
      ##  }
  }


target = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/canopyHeightFromMeta30m/';

for(ids in assetIds) {
  ee$data$copyAsset(ids,  paste0(target, basename(ids)) )
  }

