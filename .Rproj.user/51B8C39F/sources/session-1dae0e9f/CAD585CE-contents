library(rgee)
library(rgeeExtra)
library(googledrive)

## first requires that "___GEE_tileMeta.R" be run to produce the 10 m resampled height metrics
# ee_Initialize(quiet = T)
ee_Initialize(user = 'cirgeo'  )
pilotSites <- ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/wildfire/wildfire_pilot_sites_v3")

agbcoll <- ee$ImageCollection("projects/sat-io/open-datasets/ESA/ESA_CCI_AGB")
agb = agbcoll$filterDate('2021-01-01','2023-01-01')$first()$select('AGB');

# target = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/canopyHeightFromMeta100m';
target = 'users/cirgeo/wildfire/biomassCanopyHeightsUpsampleWeights10m'
from = 'users/cirgeo/wildfire/canopyHeightFromMeta10m'
list = ee$data$listAssets(target);
tb <- as.data.frame(do.call(rbind, list$assets))

##  ========================
##  2. Load Facebook Meta canopy-height raster
##  ========================
canopyColl = ee$ImageCollection(from)
#
# # sum_reducer   <- ee$Reducer$sum()$setOutputs(c("sum_band1"))
# # count_reducer <- ee$Reducer$count()$setOutputs(c("count_band1"))
# # mean_reducer  <- ee$Reducer$mean()$setOutputs(c("mean_band1"))
#
# # 2) Combine reducers
# combined <- sum_reducer$
#   combine(reducer2 = count_reducer, sharedInputs = TRUE)$
#   combine(reducer2 = mean_reducer, sharedInputs = TRUE)
#
#
# combinedReducer =   ee$Reducer$count() $combine(
#   reducer2= ee$Reducer$mean(),
#   sharedInputs= T
# )$combine(
#   reducer2= ee$Reducer$sum()$unweighted(),
#   sharedInputs= T,
#   outputPrefix="uw"
# ) $combine(
#   reducer2= ee$Reducer$sum(),
#   sharedInputs= T,
#   outputPrefix="w"
# )
#
# combinedReducer <- combinedReducer$setOutputs(c("b1_count", "b1_mean", "b1_sumuw", "b1_sumw"))

nImagesInTolanCollection <- canopyColl$size()$getInfo()
assetIds <- list()

crs_to_use <- agb$projection()$crs()
scale_to_use <- agb$projection()$nominalScale()

 for( i in 1:nImagesInTolanCollection){

  img <- ee$Image(canopyColl$toList(nImagesInTolanCollection)$get((i-1)))
  tileDict = img$getInfo();
  nm<-tileDict$properties$`system:index`
  if( any(grepl(nm, tb$name)) ) {
    message(nm, " exists")
    next
  }
  message(nm, " DOING")

  assetIds[[nm]] <- file.path(target,nm )
  out <- img$select("b1_mean")$reduceResolution(
                    reducer=  ee$Reducer$sum()$unweighted(),
                    maxPixels= 370,
                    bestEffort= T
                  )$reproject(crs=crs_to_use, scale = scale_to_use)

  ee_image_to_asset(
    image= img$select("b1_mean")$toFloat()$divide(ee$Image(out)$toFloat())$rename("b1_weight")$toFloat(),
    description= nm,
    assetId= assetIds[[nm]],
    region= img$geometry(),
    scale = img$projection()$nominalScale(), #scale_to_use,
    crs = img$projection()$crs() , #crs_to_use ,
    maxPixels= 1e13
  )$start();
}

ee_image_to_drive(
  image= ee$ImageCollection(unlist(assetIds) )$mosaic(),
  description= "all",
  scale= 10,
  #crs= crs_to_use ,
  region= pilotSites$first()$geometry()$buffer(-6000)$bounds(),
  maxPixels= 1e13
)$start();

for(ids in assetIds) {
  ee$data$deleteAsset(ids )
  }

