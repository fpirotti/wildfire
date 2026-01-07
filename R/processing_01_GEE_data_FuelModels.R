library(rgee)
library(rgeeExtra)
library(stars)
library(googledrive)
########### THIS REQUIRES FIRST THAT THE processing_01_GEE_tileMeta.R!
# 1. Authenticate ----
drive_auth(email = "cirgeo@unipd.it")
ee_Initialize(user = 'cirgeo'  )
## only forest points ----
points <- ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/wildfire/CzechGlobeDE_CZpts")$filter(ee$Filter$gt('class', 160))
withRand = points$randomColumn('rand');
train = withRand$filter(ee$Filter$lt('rand', 0.1));
valid = withRand$filter(ee$Filter$gt('rand', 0.1));
# print(valid$size()$getInfo())
#print(train$first()$getInfo())
cat(as.character(date()), "\n", file = "processing_01_GEE_data.log" )
# ee_install_upgrade()
### setting version ----
versionFuelModel  = 3
# ee_Initialize(quiet = T)
scale = 30
### setting scale ----
proj3035_30m = ee$Projection('EPSG:3035')$atScale(scale);

# 2. START ----
### setting tasks containers ----
task_img_container <- list()
task_img_containerAsset <- list()


# Function to mask clouds and shadows using the SCL band
maskS2clouds <- function(image) {
  scl = image$select('SCL');
  # because we need at least a bit of reflectance, we
  # also mask for red band above 10...
  # e.g. if B4 is 0 then NDVI will always be 1
  red = image$select('B4');
  red = image$select('B8');
  cloudShadowMask = red$gt(100)$And(nir$gt(100))$And(scl$gt(3))$And(scl$lt(6));
  return(image$updateMask(cloudShadowMask)$copyProperties(image, list('system:time_start') ))
}

# Function to compute NDVI and add it as a band
addNDVI <-function(image) {
  ndvi = image$normalizedDifference(c('B8', 'B4'))$rename('ndvi');
  return(ndvi);
}
addNBR <-function(image) {
  invnbr = image$normalizedDifference(c('B12', 'B8'))$rename('nbr');
  return(invnbr);
}
## reducer for Meta 1 m tree height to 30 m grid
# combinedReducer = ee$Reducer$mean()$combine(
#   reducer2= ee$Reducer$stdDev(),
#   sharedInputs= T
# ) $combine(
#   reducer2= ee$Reducer$min(),
#   sharedInputs= T
# ) $combine(
#   reducer2= ee$Reducer$max(),
#   sharedInputs= T
# );
#
# statsAgg <-function(image) {
#   agg = image$rename("b1")$reduceResolution(
#     reducer   = combinedReducer,
#     maxPixels = 2048L  )#$reproject(crs=proj3035_30m, scale = scale)
#   return(agg);
# }

# LAYERS ------
## pilot sites ----
inputVars <- list()
pilotSites = ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/wildfire/wildfire_pilot_sites_v3") ;
nPilots =  pilotSites$size()$getInfo()
bounds = pilotSites$geometry()$bounds()

# Time range for NDVI stack
startDate = '2023-01-01';
endDate = '2025-12-30';

# Load and process S2 collection
s2 = ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$filterDate(startDate, endDate)$filterBounds(bounds)$filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 50))$map(maskS2clouds);

##  NDVI -----
inputVars$ndviMax = s2$map(addNDVI)$qualityMosaic('ndvi')
##  NBR but inverse! higher values mean more burned, this is to use the qualityMosaic -----
inputVars$nbrMax = s2$map(addNBR)$qualityMosaic('nbr')


## DEM -----
inputVars$dem =  ee$Image("projects/progetto-eu-h2020-cirgeo/assets/eu/dtm_elev_lowestmode_gedi_v03");

terrain = ee$Terrain$products(inputVars$dem);
inputVars$slope = terrain$select('slope');
inputVars$aspect = terrain$select('aspect');

## Aridity -----
# aridityIndex =  ee$ImageCollection('projects/progetto-eu-h2020-cirgeo/assets/global/AridityIndex')$mosaic()$divide(10000);
## NEW! Tree Canopy Density from Copernicus  10 m 2021 -----
inputVars$canopy_cover = ee$ImageCollection("projects/progetto-eu-h2020-cirgeo/assets/copernicus/CLMS_TCF_TreeDensity_RASTER_2021")$mosaic()
## NEW! CLC+ backbone 10 m 2023 ----
clcplus = ee$Image('projects/progetto-eu-h2020-cirgeo/assets/copernicus/CLMS_CLCplus_RASTER_2023')$select('b1')
## NEW! crop map 10 m 2021 - we assume orchards and vineyards are not changed and lead to Fuel Type ??? ----
cropmap = ee$Image('projects/progetto-eu-h2020-cirgeo/assets/copernicus/CLMS_CropTypes_RASTER_2021')$select('b1')$unmask()
cropmapHighVeg = cropmap$gt(100)$And(cropmap$lt(200))
## very high threshold to consider all arid ? low values = arid, high values = humid
# aridityThreshold = 100;

## NEW!!  Canopy height META -----
# inputVars$canopy_height = ee$Image('users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1')$unmask()
canopy_heightColl =  ee$ImageCollection("projects/sat-io/open-datasets/facebook/meta-canopy-height")$filterBounds(bounds);
ch30m <- "projects/progetto-eu-h2020-cirgeo/assets/wildfire/canopyHeightFromMeta30m"
canopy_height =  ee$ImageCollection(ch30m)$mosaic()$setDefaultProjection(ee$ImageCollection(ch30m)$first()$projection()) #$clip(bounds) #$map(statsAgg)
# canopy_height.proj <- canopy_heightColl$first()$projection()$getInfo()

inputVars$canopy_height = canopy_height


### TRY OUTPUT ----
# for( i in 1:nPilots){
#   feature =ee$Feature( inputVars$pilotSites$toList(12)$get(i-1))
#   gg = feature$geometry();
#   idf = feature$get("pilot_id")$getInfo() ;
#   id = paste0(idf,'_CLCV', versionFuelModel)
#   idp = paste0(idf,'_ScottBurganFuelClassProbV', versionFuelModel)
#   folder <- sprintf("wildfireOutProgettoEUv3/%s",idf)
#   task_img_container[[  id ]] <- ee_image_to_drive(
#       # image= inputVars$canopy_height$toFloat()$clip(gg),
#       image= inputVars$clcplus$clip(gg),
#       description= id,
#       timePrefix = F,
#       folder=NULL,
#       region= gg ,
#       scale= 30,
#       crs= 'EPSG:3035',
#       maxPixels= 1e13
#     )
#     task_img_container[[id ]]$start()
#   }


outputStack_macroClass = { };
outputStack_scottBurgan = { };
outputStack_FBP = { };

## CANOPY LOSS MAP ----

canopy_cover = ee$Image("UMD/hansen/global_forest_change_2024_v1_12")
onlyNonDisturbedPixels =  canopy_cover$select("lossyear")$unmask()$eq(0);
hansenLossPost2018 =      canopy_cover$select("lossyear")$unmask()$gt(18L);
hansenLossPost2010  =     canopy_cover$select("lossyear")$unmask()$gt(10);
hansenLossPost2010upTo2019 = hansenLossPost2010$And( canopy_cover$select("lossyear")$unmask()$lt(19)  ) ;
hansenLossPost2000   =  canopy_cover$select("lossyear")$unmask()$gt(0)
hansenLossPost2000upTo2009   =  hansenLossPost2000$And( canopy_cover$select("lossyear")$unmask()$lt(11) ) ;


# RANDOM FOREST for FORESTS FM #####
## stack the predictors #####
predictors <- inputVars$ndviMax$rename("ndviMax")
output <- list()
for( k in names(inputVars) ){
  message(k)
  if(k=="ndviMax") next
  img1 = inputVars[[k]]
  newBnames <- gsub("b1", k, img1$bandNames()$getInfo() )
  predictors <- predictors$addBands(img1$rename(newBnames)) # nouse =  inputVars[[k]]$select(0)$projection()
}

bands = predictors$bandNames()

## training data from Cz -----
train = predictors$sampleRegions(
  collection= train,
  properties= list('class'),
  scale= 30
);

## train -----
classifier = ee$Classifier$smileRandomForest(
  numberOfTrees = 250
)$train(
  features = train,
  classProperty = 'class',
  inputProperties = bands
);

## export to asset -----
ee$batch$Export$classifier$toAsset(
  classifier = classifier,
  description= 'RF_classifier_export',
  assetId= 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/RF_classifier_FuelModelV3'
)$start();


## CREATE PREDICTORS STACK  -----------
assetRootPred = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelPredictors/';
assetRootClassified = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelPredictedRF/';


list = ee$data$listAssets(assetRootPred);
tb <-   data.frame(name=sapply(list$assets, function(x){x[["name"]]}))


for( i in 1:nPilots){

  feature =ee$Feature(pilotSites$toList(12)$get(i-1))
  gg = feature$geometry();
  idf = feature$get("pilot_id")$getInfo() ;

  id = paste0(idf,'_predictorsV', versionFuelModel)
  message(id)
  assetid <- paste0(assetRootPred,id)

  if( is.element(assetid, tb$name) ) ee$data$deleteAsset(assetid)

  task_img_container[[  id ]] <- ee_image_to_asset(
    image=  predictors,#$clip(gg),#$float(),
    description= id,
    assetId= assetid,
    region= gg,
    scale= 30,
    crs= 'EPSG:3035',
    maxPixels= 1e13
  )
  task_img_container[[id ]]$start()

}

## CREATE CLASSIFIED IMAGES   -----------
classifier <- ee$Classifier$load('projects/progetto-eu-h2020-cirgeo/assets/wildfire/RF_classifier_FuelModelV3');

list = ee$data$listAssets(assetRootClassified);
tb <- tryCatch({
  data.frame(name=sapply(list$assets, function(x){x[["name"]]}))
}, error = function(e){
  as.data.frame(list$assets)
} )

for( i in 1:nPilots){

  feature =ee$Feature(pilotSites$toList(12)$get(i-1))
  gg = feature$geometry();
  idf = feature$get("pilot_id")$getInfo() ;
  id = paste0(idf,'_predictorsV', versionFuelModel)
  idOut = paste0(idf,'_predictedV', versionFuelModel)
  message(id)
  assetid <- paste0(assetRootPred,id)
  assetidOut <- paste0(assetRootClassified,idOut)
  if( is.element(assetidOut, tb$name) ) ee$data$deleteAsset(assetidOut)
  task_img_container[[  id ]] <- ee_image_to_asset(
    image=  predictors$clip(gg)$classify(classifier),
    description= idOut,
    assetId= assetidOut,
    region= gg,
    scale= 30,
    crs= 'EPSG:3035',
    maxPixels= 1e13
  )
  task_img_container[[id ]]$start()

}

# train$first()$geometry()$projection()$getInfo()
### RANDOM FOREST FINISHED -----
### DOWNLOAD FOR VALIDATION N1 ----
imgcol <- ee$ImageCollection(file.path(dirname(assetRootClassified), basename(assetRootClassified)))

for( i in 1:imgcol$size()$getInfo()){

  feature =ee$Feature(pilotSites$toList(12)$get(i-1))
  gg = feature$geometry();
  idf = feature$get("pilot_id")$getInfo() ;
  id = paste0(idf,'_predictorsV', versionFuelModel)
  idOut = paste0(idf,'_predictedV', versionFuelModel)

  assetid <- paste0('projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelPredictors/',id)

  assetid <- paste0(assetRootPred,id)
  assetidOut <- paste0(assetRootClassified,idOut)
  img <-  ee$Image( assetidOut ) ## ee$Image(imgcol$toList(imgcol$size())$get(i-1))
  message(id)
  # task_img_container[[  id ]] <- ee_image_to_drive(
  #   image=  ee$Image(assetid)$toFloat()$clip(gg),
  #   description= id,
  #   region= gg,
  #   folder = NULL,
  #   fileNamePrefix = NULL,
  #   timePrefix = F,
  #   scale= 30,
  #   crs= 'EPSG:3035',
  #   maxPixels= 1e13
  # )
  # task_img_container[[id ]]$start()
  message(idOut)
  task_img_container[[  idOut ]] <- ee_image_to_drive(
    image=  img$clip(gg),
    description= idOut,
    folder = "rGEEout",
    fileNamePrefix = NULL,
    timePrefix = F,
    #assetId= assetidOut,
    region= gg,
    scale= 30,
    crs= 'EPSG:3035',
    maxPixels= 1e13
  )
  task_img_container[[idOut ]]$start()

}


for( i in 1:imgcol$size()$getInfo()){
  feature =ee$Feature(pilotSites$toList(12)$get(i-1))
  idf = feature$get("pilot_id")$getInfo() ;
  idOut = paste0(idf,'_predictedV', versionFuelModel)
  f <-   drive_find(q = sprintf("name contains '%s'", idOut) )
  googledrive::drive_download(f,overwrite = T, path =
                                file.path("data/validation/version3/v3b", f$name))
}
# outputStack$updatedHansen = canopy_cover$select(0)$unmask()$mask(onlyNonDisturbedPixels)


# 91 Urban or suburban development; insufficient wildland fuel ----
outputStack_scottBurgan$a91 = inputVars$clcplus$eq(1)
outputStack_macroClass$a91 = inputVars$clcplus$eq(1)
# 98 water ----
outputStack_scottBurgan$a98 = inputVars$clcplus$gt(253)$Or(inputVars$clcplus$eq(10))
outputStack_macroClass$a98 = inputVars$clcplus$gt(253)$Or(inputVars$clcplus$eq(10))

# 99 barren ----
outputStack_scottBurgan$a99 = inputVars$clcplus$gt(253)$Or(inputVars$clcplus$gt(7)$And(inputVars$clcplus$lt(10) ))
outputStack_macroClass$a99 = inputVars$clcplus$gt(253)$Or(inputVars$clcplus$gt(7)$And(inputVars$clcplus$lt(10) ))

# 92 snow ice ----
outputStack_scottBurgan$a92 = inputVars$clcplus$eq(11)
outputStack_macroClass$a92 = inputVars$clcplus$eq(11)

### PERCENTILES NDVI -----
## I need to know the percentiles of the NDVI that cover the grass/shrub/tree vegetation,
## in order to estimate a first idea for fuel load (more for grass/shrub as trees are
## saturated )
#
# percentilesGRASS10 = ndviMax$mask( clcplus$eq(6)$Or(clcplus$eq(7)) )$reduceRegions(
#   reducer= ee$Reducer$percentile( c(5, 10, 25, 50, 75, 90, 95) ),
#   collection= pilotSites,
#   scale= 10
# )$getInfo()
# percentilesShrub10 = ndviMax$mask( clcplus$eq(5) )$reduceRegions(
#   reducer= ee$Reducer$percentile( c(5, 10, 25, 50, 75, 90, 95) ),
#   collection= pilotSites,
#   scale= 10
# )$getInfo()
#
# load( file="percentilesGRASS.rda")
# aa <- lapply( names(percentiles) , function(ppn){
#   pp <- percentiles[[ppn]]
#   res<- sapply(pp$features, function(x){
#     gg<-unlist(x$properties[c(4,2,3,5,6,7,8)])
#   })
#   rr<-as.data.frame( t(apply(res, 1, function(s){
#     c(mean(s), sd(s) )
#   })) )
#   names(rr) <- c("Mean", "stdDev")
#   rr$Quantile <- rownames(res)
#   rr$Type <-ppn
#   rr
# })
# ndviStats <- do.call(rbind, aa)
# ndviStats$Quantile <- factor(ndviStats$Quantile,
#                              levels = (sprintf("p%d", c(5,10,25,50,75,90, 95) ) ) )
# library(ggplot2)
# ggplot2::ggplot(ndviStats, aes(fill=Type,  color=Type, y=Mean,x=Quantile,
#                                group=Type)) +
#   geom_errorbar(aes( ymin = Mean-stdDev, ymax=Mean+stdDev )) +
#   facet_wrap(vars( Type)) +
#   geom_line()
# percentiles <- list(grass10m=percentilesGRASS10, shurb100m=percentilesShrub100,
#                     shrub10m=percentilesShrub10)
# save(percentiles,
#      file="percentiles.rda")

## GRASS  ----
grassCLCplus=clcplus$eq(6)$Or(clcplus$eq(7))$byte()
## MACRO Classes -----
## Grass only if < 10% has vegetation > 1 m
outputStack_macroClass$a10 = grassCLCplus$And(
  inputVars$canopy_height$select("b1_min")$eq(0L)$And(
    inputVars$canopy_height$select("b1_mean")$lte(1L)
  )
)
## to make sure it is grass, we remove pixels that have any crop type canopy cover
## grass shrub only if > 10% has vegetation > 1 m
outputStack_macroClass$a12= grassCLCplus$And(
  inputVars$canopy_height$select("b1_min")$neq(0L)$Or(
    inputVars$canopy_height$select("b1_mean")$gt(1L)
  )
)

tmp <- outputStack_macroClass$a10$reduceRegions(
     reducer= ee$Reducer$sum(),
     collection= pilotSites,
     scale= 30
   )

a10a <- tmp$getInfo()
a10i2 <- lapply(a10$features, function(x){
  x[["properties"]][["sum"]]
} )


tmp <- outputStack_macroClass$a12$reduceRegions(
  reducer= ee$Reducer$sum(),
  collection= pilotSites,
  scale= 30
)
a12 <- tmp$getInfo()
a12i <- lapply(a12$features, function(x){
  x[["properties"]][["sum"]]
} )

##  shrub
outputStack_macroClass$a14=clcplus$eq(5)
##  tree timber understorey
outputStack_macroClass$a16=clcplus$eq(6)
##  tree1 timber litter
outputStack_macroClass$a18=clcplus$eq(5)
##  blowdown - forest with disturbance but with low inverse nbr (high inverse nbr = charred fuel)
outputStack_macroClass$a20=clcplus$eq(5)


onlyMacroClass <- T
## Grass
# 101 grasssparse ----
#grass= grassCLCplus$eq(6)$Or(grassCLCplus$eq(7))
if(!onlyMacroClass){
  outputStack_scottBurgan$a101=grassCLCplus
  # GRASS LOW load ----
  grassLowLoad=grassCLCplus$Or(grassProba)$And(ndviMax$lt(0.5))
  ## 102 dry ----
  outputStack_scottBurgan$a102=grassLowLoad$multiply(aridityIndex$lte(aridityThreshold))
  ## 105 wet ---- should not exist
  outputStack_scottBurgan$a105=grassLowLoad$multiply(aridityIndex$gt(aridityThreshold))

  # grass MOD load ----
  grassModerateLoad=grassCLCplus$Or(grassProba)$And(ndviMax$gte(0.2)$And(ndviMax$lt(0.4)))$multiply(99)
  ## 104 dry ----
  outputStack_scottBurgan$a104=grassModerateLoad$multiply(aridityIndex$lte(aridityThreshold))
  ## 106 wet ----
  outputStack_scottBurgan$a106=grassModerateLoad$multiply(aridityIndex$gt(aridityThreshold))

  # grass HIGH load ----
  grassHighLoad=grassCLCplus$Or(grassProba)$And(ndviMax$gte(0.4))$multiply(99)
  ## 107 dry ----
  outputStack_scottBurgan$a107=grassHighLoad$multiply(aridityIndex$lte(aridityThreshold))
  ## 108 wet ----
  outputStack_scottBurgan$a108=grassHighLoad$multiply(aridityIndex$gt(aridityThreshold))

  #SHRUB  ----
  shrubProba=proba$select('discrete_classification')$eq(20)
  shrubCLCplus=clcplus$eq(5)

  #SHRUB low load ----
  shrubLowLoad=shrubCLCplus$Or(shrubProba)$And(ndviMax$lt(0.2))$multiply(99)
  ## 141 dry ----
  outputStack_scottBurgan$a141=shrubLowLoad$multiply(aridityIndex$lte(aridityThreshold))
  ## 146 wet ----
  outputStack_scottBurgan$a146=shrubLowLoad$multiply(aridityIndex$gt(aridityThreshold))


  #SHRUB MODERATE load ----
  shrubModerateLoad=shrubCLCplus$Or(shrubProba)$And(ndviMax$gte(0.2)$And(ndviMax$lt(0.4)))$multiply(99)
  ## 142 dry ----
  outputStack_scottBurgan$a142=shrubModerateLoad$multiply(aridityIndex$lte(aridityThreshold))
  ## 143 wet ----
  outputStack_scottBurgan$a143=shrubModerateLoad$multiply(aridityIndex$gt(aridityThreshold))


  #SHRUB high load ----
  shrubHighLoad=shrubCLCplus$Or(shrubProba)$And(ndviMax$gte(0.4)$And(ndviMax$lt(0.6)))$multiply(99)
  ## 145 dry ----
  outputStack_scottBurgan$a145=shrubHighLoad$multiply(aridityIndex$lte(aridityThreshold))
  ## 148 wet ----
  outputStack_scottBurgan$a148=shrubHighLoad$multiply(aridityIndex$gt(aridityThreshold))

  #############
  # SHRUB VERY high load ----
  shrubVeryHighLoad=shrubCLCplus$And(shrubProba)$And(ndviMax$gte(0.6))$multiply(99)
  ## 147 dry ----
  outputStack_scottBurgan$a147=shrubVeryHighLoad$multiply(aridityIndex$lte(aridityThreshold))
  ## 149 wet ----
  outputStack_scottBurgan$a149=shrubVeryHighLoad$multiply(aridityIndex$gt(aridityThreshold))


  #############
  # TIMBER UNDERSTOREY  ----
  proba12x=proba$select('discrete_classification')$divide(10)$floor()$toByte()$eq(12)

  # TIMBER UNDERSTOREY LOW+MEDIUM MODERATE load  ----
  timberUnderstoreyLowMediumLoad=clcplus$gt(1)$And(clcplus$ lt(6))$And(proba12x)$And(ndviMax$lte(0.6))$multiply(99)
  ## 161 dry  ----
  outputStack_scottBurgan$a161=timberUnderstoreyLowMediumLoad$multiply(aridityIndex$lte(aridityThreshold))
  ## 162 wet  ----
  outputStack_scottBurgan$a162=timberUnderstoreyLowMediumLoad$multiply(aridityIndex$gt(aridityThreshold))

  #############
  #TIMBER UNDERSTOREY high load  ----
  timberUnderstoreyHighLoad=clcplus$gt(1)$Or(clcplus$ lt(6))$And(proba12x)$And(ndviMax$gt(0.6))$multiply(99)
  ## 165 dry ----
  outputStack_scottBurgan$a165=timberUnderstoreyHighLoad$multiply(aridityIndex$lte(aridityThreshold))
  ## 163 wet ----
  outputStack_scottBurgan$a163=timberUnderstoreyHighLoad$multiply(aridityIndex$gt(aridityThreshold))

  #############
  #TIMBER LITTER CONIFER  -----
  probaClass = proba$select('discrete_classification')
  proba11x=probaClass$divide(10)$floor()$toByte()$eq(11)

  fuelLoadEstimation = tcd$divide(100)$multiply(ndviMax)$multiply(canopy_height$divide(30))
  clcplusTimber = clcplus$gt(1)$And(clcplus$ lt(6))$And(proba12x$Or(proba11x))$And(fuelLoadEstimation$gt(0.1))

  clcplusTimberConifer = clcplusTimber$And( probaClass$eq(111)$Or(probaClass$eq(113))$Or(probaClass$eq(115))$Or(probaClass$eq(121))$Or(probaClass$eq(123))$Or(probaClass$eq(125))  )
  clcplusTimberBroadleaf = clcplusTimber$And( probaClass$eq(112)$Or(probaClass$eq(114))$Or(probaClass$eq(116))$Or(probaClass$eq(122))$Or(probaClass$eq(124))$Or(probaClass$eq(126))  )

  lowLoad = fuelLoadEstimation$gt(0)$And(fuelLoadEstimation$lt(0.33));
  medLoad = fuelLoadEstimation$gte(0.33)$And(fuelLoadEstimation$lt(0.66));
  highLoad = fuelLoadEstimation$gte(0.66);



  ##  181 Low Load Compact Conifer Litter -----
  outputStack_scottBurgan$a181=clcplusTimberConifer$add(lowLoad)$multiply(50)
  ##  182 Low Load Compact Broadleaves Litter -----
  outputStack_scottBurgan$a182=clcplusTimberBroadleaf$add(lowLoad)$multiply(50)

  ##  183 Moderate Load Conifer Litter -----
  outputStack_scottBurgan$a183=clcplusTimberConifer$add(medLoad)$multiply(50)

  ##  186 Moderate Load Broadleaf Litter -----
  outputStack_scottBurgan$a186= clcplusTimberBroadleaf$add(medLoad)$multiply(50)

  ##  185 High Load Conifer Litter -----
  outputStack_scottBurgan$a185=clcplusTimberConifer$And(highLoad)$multiply(50)

  ##  189 very High Load broadleaf Compact Litter -----
  outputStack_scottBurgan$a189= clcplusTimberBroadleaf$add(highLoad)$multiply(50)



  ########## SLASH BLOWDOWN USING HANSEN LOSS -------
  ## areas with 100% canopy cover and trees 25 meters or above
  ## will have class 204 high load.
  ## load is lowered depending on density and tree height
  sb = clcplusTimber$multiply(tcd)$multiply(canopy_height)$divide(2500)$multiply(4L)$multiply(hansenLossPost2018)$unmask()$toByte()

  outputStack_scottBurgan$a201 = sb$eq(1L)
  outputStack_scottBurgan$a202 = sb$eq(2L)
  outputStack_scottBurgan$a203 = sb$eq(3L)
  outputStack_scottBurgan$a204 = sb$eq(4L)

}

for( k in names(outputStack_scottBurgan) ){
  bv = as.integer(substr(k, 2,6))
  message(bv)
  if(is.na(bv)){
    browser()
  }
  nouse =  outputStack_scottBurgan[[k]]$select(0)$projection()
  newBand = ee$Image$constant( bv )$
    rename('new_band')$
    setDefaultProjection(nouse)$toByte();

  outputStack_scottBurgan[[k]] = outputStack_scottBurgan[[k]]$unmask()$toFloat()$addBands(newBand)$rename(c("prob","class") );

}

ScottBurganProbs=ee$ImageCollection( unname(outputStack_scottBurgan) )
nouse = ScottBurganProbs$first()$projection()

ScottBurgan=ScottBurganProbs$qualityMosaic('prob')$
  setDefaultProjection(nouse)$
  rename(c('scottburgan_cprob', 'scottburgan_class') )





## process Pilot areas -----------
for( i in 1:nPilots){

  feature =ee$Feature(pilotSites$toList(12)$get(i-1))
  gg = feature$geometry();
  idf = feature$get("pilot_id")$getInfo() ;


  ScottBurganFiltered = ScottBurgan$select('scottburgan_class')
  id = paste0(idf,'_ScottBurganFuelMapClassV', versionFuelModel)
  # task_img_containerAsset[[id ]] <-"s"
  # next
  ScottBurganProbFiltered = ScottBurgan$select('scottburgan_cprob')
  idp = paste0(idf,'_ScottBurganFuelClassProbV', versionFuelModel)

  if(0==0 && is.null(task_img_container[[id ]])){

    folder <- sprintf("wildfireOutProgettoEU/%s",idf)
    # 2. Get the folder
    folder2rm <- drive_get(folder)

    # 3. List files
    files_in_folder <- drive_ls(path = folder2rm)

    # 4. Remove all files
    drive_rm(files_in_folder)

    task_img_container[[  id ]] <- ee_image_to_drive(
      image= ScottBurganFiltered$toByte()$clip(gg),
      description= id,
      timePrefix = F,
      folder=NULL,
      region= gg ,
      scale= 30,
      crs= 'EPSG:3035',
      maxPixels= 1e13
    )
    task_img_container[[id ]]$start()
    #
    #     task_img_container[[  idp ]] <- ee_image_to_drive(
    #       image= ScottBurganProbFiltered$toByte()$clip(gg),
    #       description= idp,
    #       folder="wildfireOutProgettoEU",
    #       timePrefix = F,
    #       region= gg ,
    #       scale= 30,
    #       crs= 'EPSG:3035',
    #       maxPixels= 1e13
    #     )
    #     task_img_container[[idp ]]$start()

  }


  assetid <- paste0('projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelV2/',  id)
  message(assetid)

  bb <- system( sprintf("earthengine rm %s", assetid), intern = T)
  if(length(bb)) cat(bb, "\n", file = "processing_01_GEE_data.log",append = T) else cat(bb, "SUCCESS\n", file = "processing_01_GEE_data.log",append = T)

  if(is.null(task_img_containerAsset[[ assetid ]])){

    task_img_containerAsset[[id ]] <- ee_image_to_asset(
      image= ScottBurgan$toByte()$clip(gg) ,
      description= paste0(id, 'asset'),
      assetId= assetid,
      region= gg ,
      scale= 30,
      crs= 'EPSG:4326',
      maxPixels= 1e13
    )

    task_img_containerAsset[[id ]]$start()

  }
  # task_img_container[[assetid ]]$status()
  # task_img_container[[assetid ]]$cancel()

}


for( assetid in names(task_img_containerAsset)){
  id <- assetid

  folder <- sprintf("wildfireOutProgettoEU/%s", substr(assetid,1,5))
  message("checking ", assetid)

  cat("checking exports...", assetid, "\n", file = "processing_01_GEE_data.log",append = T)

  task <-  task_img_containerAsset[[assetid]]
  while (task$status()$state %in% c('READY', 'RUNNING')) {
    cat("Task status:", task$status()$state, "\n", file = "processing_01_GEE_data.log",append = T)
    Sys.sleep(10)
  }
  if(task$status()$state=="FAILED"){
    cat("Task status:", task$status()$error_message, "\n", file = "processing_01_GEE_data.log",append = T)
  } else{
    cat("Task status: ", task$status()$state, "\n", file = "processing_01_GEE_data.log",append = T)
  }

  task <-  task_img_container[[assetid]]
  while (task$status()$state %in% c('READY', 'RUNNING')) {
    cat("Task status:", task$status()$state, "\n", file = "processing_01_GEE_data.log",append = T)
    Sys.sleep(10)
  }
  if(task$status()$state=="FAILED"){
    cat("Task status:", task$status()$error_message, "\n", file = "processing_01_GEE_data.log",append = T)
  } else{

    drive_mv(sprintf("%s.tif", id), sprintf("%s/%s.tif", folder, id))


    cat("Task status: ", task$status()$state, "\n", file = "processing_01_GEE_data.log",append = T)
  }

}





bb <- system("earthengine acl set public projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelV2",intern = T)
if(length(bb)) cat(bb, "\n", file = "processing_01_GEE_data.log",append = T) else cat(bb, "SUCCESS\n", file = "processing_01_GEE_data.log",append = T)


classHistogram <- {}
for( assetid in names(task_img_containerAsset)){
  message("histogram ", assetid)

  cat("histogram ", assetid, "\n", file = "processing_01_GEE_data.log",append = T)
  imgname <- paste0('projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelV2/',  assetid)

  fuelModel = ee$Image(imgname)
  nm <- substr(assetid, 1, 5)
  classHistogram[[nm]] = fuelModel$select(1)$reduceRegion(
    reducer= ee$Reducer$frequencyHistogram(),
    geometry= fuelModel$geometry(),
    scale= 30,
    maxPixels= 1e13
  )$getInfo()
}

dt2 <- lapply(classHistogram, function(ll){
  ll <- as.data.frame(ll) / 1e6 * 900 * 100
  ll[ ll< 1 ]  <- NA

  message("tot ha ", sum(ll))

  names(ll) <- sprintf("ha_class%03d",
                       as.integer(gsub('[^(-?(\\d*\\.)?\\d+)]', "",
                                       gsub('\\.','',names(ll) ) ) ) )

  ll
} )

dt3 <- data.table::rbindlist(dt2, idcol = "site", fill = T)
dt4<- reshape2::melt(dt3)

dt3.dt <- as.data.frame(t(dt3[,-1]))
names(dt3.dt)<-dt3[,1][[1]]
dt3.dt$class<-  as.integer(gsub('[^(-?(\\d*\\.)?\\d+)]', "", rownames(dt3.dt) ) )

writexl::write_xlsx(dt3.dt, "fuelmodelPilotSite.xlsx")

featuresHist <- {}
for( i in 1:n){
  feature =ee$Feature(pilotSites$toList(12)$get(i-1))
  gg = feature$geometry();
  idf = feature$get("pilot_id")$getInfo() ;
  message(idf)
  llfeat <- as.list(dt2[[idf]])
  names(llfeat)<- sprintf("class%03d", as.integer(gsub('[^(-?(\\d*\\.)?\\d+)]', "", names(llfeat))) )
  llfeat[["site"]] <- idf
  featuresHist[[idf]] <- ee$Feature(gg,  )
}

amk_fc <-ee$FeatureCollection( unname(featuresHist)  )

## HISTOGRAM -----
assetIdh= sprintf('projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelV%dHFD', versionFuelModel)
task <- ee_table_to_asset(
  collection= amk_fc ,
  description="Histogram",
  assetId= assetIdh
)

bb <- system( sprintf("earthengine rm %s", assetIdh), intern = T)
if(length(bb)) cat(bb, "\n", file = "processing_01_GEE_data.log",append = T) else cat(bb, "SUCCESS\n", file = "processing_01_GEE_data.log",append = T)

task$start()

cat("Task status HISTOGRAM:", task$status()$state, "\n", file = "processing_01_GEE_data.log",append = T)
while (task$status()$state %in% c('READY', 'RUNNING')) {
  cat("Task status:", task$status()$state, "\n", file = "processing_01_GEE_data.log",append = T)
  Sys.sleep(10)
}
if(task$status()$state=="FAILED"){
  cat("Task status:", task$status()$error_message, "\n", file = "processing_01_GEE_data.log",append = T)
} else{
  cat("Task status: ", task$status()$state, "\n", file = "processing_01_GEE_data.log",append = T)
}
bb <- system( sprintf("earthengine acl set public %s", assetIdh), intern = T)
if(length(bb)) cat(bb, "\n", file = "processing_01_GEE_data.log",append = T) else cat(bb, "SUCCESS\n", file = "processing_01_GEE_data.log",append = T)


