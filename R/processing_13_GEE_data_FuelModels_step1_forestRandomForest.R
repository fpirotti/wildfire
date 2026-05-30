library(this.path)
source(file.path(this.path::this.dir(), "000_global.R"))


### setting version ----
versionFuelModel  = 3
########### THIS REQUIRES FIRST THAT THE processing_01_GEE_tileMeta.R!
# 1. Authenticate ----
drive_auth(email = "cirgeo@unipd.it")
ee_Initialize(user = 'cirgeo'  )
## only forest points ----
assetRootPredictorsStack  = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelPredictorsStack/';
assetRootClassified = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelPredictedRF/';
assetRootPredictedStack = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/predictedForestStack/';


# LAYERS ------
## pilot pilotRegions ----
pilotRegions <- ee$FeatureCollection(
  "projects/progetto-eu-h2020-cirgeo/assets/wildfire/pilotRegions"
)
bounds = pilotRegions$geometry()$bounds()
pilotRegionsROI <- pilotRegions$union()$geometry()$buffer(90, 1)

canopy_height <- ee$Image('users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1')$clip(pilotRegionsROI)
clcplus <- ee$Image(
  "projects/progetto-eu-h2020-cirgeo/assets/copernicus/CLMS_CLCplus_RASTER_2023"
)$clip(pilotRegionsROI)

tcd <- ee$Image(
  "projects/progetto-eu-h2020-cirgeo/assets/copernicus/CLMS_TCF_TreeDensity_RASTER_2023/tile4"
)$clip(pilotRegionsROI);

srtm = ee$Image("projects/progetto-eu-h2020-cirgeo/assets/eu/dtm_elev_lowestmode_gedi_v03");
slope = ee$Terrain$slope(srtm)$clip(pilotRegionsROI);
aspect = ee$Terrain$aspect(srtm)$clip(pilotRegionsROI);

# Time range for NDVI stack
startDate = '2021-01-01';
endDate = '2024-01-30';
# Function to mask clouds and shadows using the SCL band
maskS2clouds <- function(image) {
  scl = image$select('SCL');
  # because we need at least a bit of reflectance, we
  # also mask for red band above 10...
  # e.g. if B4 is 0 then NDVI will always be 1
  red = image$select('B4');
  nir = image$select('B8');
  cloudShadowMask = red$gt(100)$And(nir$gt(100))$And(scl$gt(3))$And(scl$lt(7));
  return(image$updateMask(cloudShadowMask)$copyProperties(image, list('system:time_start') ))
}
addNDVI <-function(image) {
  ndvi = image$normalizedDifference(c('B8', 'B4'))$rename('ndvi')$copyProperties(image, list('system:time_start') );
  return(ndvi);
}

addNBR <-function(image) {
  nbr = ee$Image(image$normalizedDifference(c('B8', 'B12'))$
                   rename('nbr'))
  nbr <- nbr$copyProperties(image, list('system:time_start') )$set('year', ee$Image(image)$date()$format('YYYY'))
  return(nbr);
}
# Load and process S2 collection
s2 = ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$filterDate(startDate, endDate)$filter(ee$Filter$calendarRange(7L, 9L, 'month'))$filterBounds(bounds)$filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 30))$map(maskS2clouds);

ndviMax = s2$map(addNDVI)$qualityMosaic("ndvi")$rename("ndviMax")$reproject( s2$first()$select("B8")$projection() )

# RANDOM FOREST for FORESTS FM #####
doRandomForest <- function(forceRecreation = T){

  source <- "CzGl.DE_CZ"
  points0 <- ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/wildfire/ptsCzechGlobeDE_CZ")$filter(ee$Filter$gt('class', 160))#$map(cast_to_source )
  source <- "CzGl.AT_CZ"
  points1 <- ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/wildfire/ptsCzechGlobeAT_CZ")$filter(ee$Filter$gt('class', 160))#$map(cast_to_source )
  source <- "Boku.AT_CZ"
  points2 <- ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/wildfire/ptsBokuAT_CZ")$filter(ee$Filter$gt('class', 160))#$map(cast_to_source )
  source <- "Boku.AT_IT"
  points3 <- ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/wildfire/ptsBokuAT_IT")$filter(ee$Filter$gt('class', 160))#$map(cast_to_source )


  tointFeature <- function(feature){
    # Extract the current class value
    currentClass <- feature$get("class")
    # Force it to be an integer
    intClass <- ee$Number(currentClass)$toInt()
    # Return the feature with the updated property
    return(feature$set("class", intClass))
  }

  points2train <- points0$merge(points1)$merge(points2)$merge(points3)$map(tointFeature)

  withRand = points2train$randomColumn('rand');
  filter_or <- ee$Filter$Or(
    ee$Filter$eq("class", 161),
    ee$Filter$eq("class", 164),
    ee$Filter$eq("class", 182),
    ee$Filter$eq("class", 185)
  )

  train = withRand$filter(ee$Filter$lte('rand', 0.4))$merge( points2train$filter(ee$Filter(filter_or)) )$select("class")$map(tointFeature)
  valid = withRand$filter(ee$Filter$gt('rand', 0.4))$select("class")$map(tointFeature)

  inputVars <- list()



  ##  NDVI -----
  inputVars$ndviMax = ndviMax
  inputVars$ndviMedian = s2$map(addNDVI)$median()$rename("ndviMedian")$reproject( s2$first()$select("B8")$projection() )

  ##  NBR  -----
  inputVars$nbrMin = s2$map(addNBR)$min()$rename("nbrMin")$reproject( s2$first()$select("B8")$projection() )
  inputVars$nbrMedian = s2$map(addNBR)$median()$rename("nbrMedian")$reproject( s2$first()$select("B8")$projection() )
  inputVars$nbrMax = s2$map(addNBR)$max()$rename("nbrMax")$reproject( s2$first()$select("B8")$projection() )

  ## DEM -----
  inputVars$dem =  ee$Image("projects/progetto-eu-h2020-cirgeo/assets/eu/dtm_elev_lowestmode_gedi_v03")$toFloat()$divide(10L);

  terrain = ee$Terrain$products(inputVars$dem);
  inputVars$slope = terrain$select('slope');
  inputVars$aspect = terrain$select('aspect');

  inputVars$canopy_cover = tcd;

  inputVars$clcplus <- clcplus
  inputVars$canopy_height = canopy_height

  ## ALOS -----
  alosC= ee$ImageCollection("JAXA/ALOS/PALSAR/YEARLY/SAR_EPOCH")$
    filterDate("2022-01-01", "2026-01-01")$
    select(c("HH", "HV"))

  alos_db = alosC$
    median()$
    reproject( alosC$first()$projection() )$
    log10()$multiply(10)$subtract(83)

  inputVars$alos_hh <- alos_db$select("HH")$rename("ALOS_HH")
  inputVars$alos_hv <- alos_db$select("HV")$rename("ALOS_HV")
  inputVars$alos_ratio <- alos_db$select("HV")$divide(alos_db$select("HH"))$rename("ALOS_L_ratio")

  ## SENTINEL 1 we keep only summer months -------
  s1C <- ee$ImageCollection("COPERNICUS/S1_GRD")$
    filterDate(startDate, endDate)$filter(ee$Filter$calendarRange(7L, 9L, 'month'))$
    filterBounds(bounds)$
    filter(ee$Filter$listContains('transmitterReceiverPolarisation', 'VV'))$
    filter(ee$Filter$listContains('transmitterReceiverPolarisation', 'VH'))$
    select(c("VV","VH"))

  s1 <- s1C$median()$
    reproject( s1C$first()$projection() )


  inputVars$s1_vv <- s1$select("VV")$rename("S1_VV")
  inputVars$s1_vh <- s1$select("VH")$rename("S1_VH")
  inputVars$s1_ratio <- s1$select("VH")$divide(s1$select("VV") )$rename("S1_C_ratio")
  inputVars$s1_alos_cross_diff <- alos_db$select("HV")$subtract(s1$select("VH"))$rename("ALOS_L_minus_S1_C_crossPol_dB")

  ## STACK creation of PREDICTORS #####
  predictors <- inputVars$clcplus$rename("clcplus")$toByte()

  for( k in names(inputVars) ){
    message(k)
    if(k=="clcplus") next
    img1 = inputVars[[k]]
    newBnames <- gsub("b1", k, img1$bandNames()$getInfo() )
    predictors <- predictors$addBands(img1$rename(newBnames)$resample('bilinear') ) # nouse =  inputVars[[k]]$select(0)$projection()
  }

  bands = predictors$bandNames()
  # bands$getInfo()
  list = ee$data$listAssets(assetRootPredictorsStack);
  tb <-   data.frame(name=sapply(list$assets, function(x){x[["name"]]}))
  forceRecreation = T
  # --- PREDICTORS TO ASSETS   -----
  for(reg in c("pilotRegions")){

    obj <- get(reg)
    ps_list <- obj$toList(obj$size())
    n <- obj$size()$getInfo()
    tp = reg

    # --- Loop over sites   ---
    for (i2 in seq_len(n) - 1) {

      feat <- ee$Feature(ps_list$get(i2))
      inf <- feat$get("pilot_id")$getInfo()
      if(is.null(inf)){
        inf <- feat$get("ID")$getInfo()
      }

      feature = feat
      gg = feature$geometry()$buffer(90,1);
      idf = inf ;
      id = paste0(idf,'_', reg, "_V", versionFuelModel)
      message(id)
      assetid <- paste0(assetRootPredictorsStack,id)
      # assetid2 <- paste0(assetRootPred2,id)assetRootPredictorsStack

      message(assetid)
      if( is.element(assetid, tb$name)  ) {
        if(forceRecreation)  {
          ee$data$deleteAsset(assetid)
        } else {
          message(assetid, " exists, skipping")
          next
        }
      }

     ee_image_to_asset(
        image=  predictors$clip(gg)$float(),
        description= id,
        assetId= assetid,
        region= gg,
        crs         = proj_3035_10m$crs,
        crsTransform = proj_3035_10m$crsTransform,
        maxPixels= 1e13
      )$start()
    }
  }



  ## TRAIN -----
  ### PREDICTOR VECTOR AT TRAINING POINTS  -----
  predictorsC <-  ee$ImageCollection(file.path(dirname(assetRootPredictorsStack), basename(assetRootPredictorsStack)))
  predictors <- predictorsC$mosaic()$setDefaultProjection( predictorsC$first()$projection() )$unmask()
  bands = predictors$bandNames()
  # bands$getInfo()
  ## training data from CzGlobe and BOKU - comment if you already have  -----
  trainPreds = predictors$sampleRegions(
    collection= train,
    properties= list('class'),
    geometries = TRUE
  );

  trainPointsAsset <- "projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelTrainPointsWithPredictors"
  ee$data$deleteAsset(trainPointsAsset)
  ee_table_to_asset(
    collection = trainPreds,
    description = "Export_Training_Points_ScottBurgan",
    assetId = trainPointsAsset
  )$start()

  ### START HERE IF YOU ALREADY HAVE THE ASSET OF TRAINING POINTS -----
  trainPreds <- ee$FeatureCollection(trainPointsAsset)
  ## train -----
  classifier = ee$Classifier$smileRandomForest(
    numberOfTrees = 150
  )$setOutputMode('MULTIPROBABILITY')$train(
    features = trainPreds,
    classProperty = 'class',
    inputProperties = bands
  );



  # classValuesH = train$aggregate_histogram('class')$getInfo();
  # trainPreds$first()$getInfo()
  # barplot(  unlist(classValuesH), names=names(classValuesH) )

  classValues = trainPreds$aggregate_array('class')$distinct()$sort();
  classValuesR <- classValues$getInfo()
  classValuesRbn <- sprintf("a%d", classValuesR)

  ## START CLASSIFIED STACK ------
  list = ee$data$listAssets(assetRootPredictedStack);
  tb <- tryCatch({
    data.frame(name=sapply(list$assets, function(x){x[["name"]]}))
  }, error = function(e){
    as.data.frame(list$assets)
  } )

  for(reg in c("pilotRegions")){
    obj <- get(reg)
    ps_list <- obj$toList(obj$size())
    n <- obj$size()$getInfo()
    tp = reg
    # --- Loop over sites   ---
    for (i2 in seq_len(n) - 1) {
      feat <- ee$Feature(ps_list$get(i2))
      # if(nm!="AT-IT") next
      inf <- feat$get("pilot_id")$getInfo()
      if(is.null(inf)){
        inf <- feat$get("ID")$getInfo()
      }

      feature = feat
      gg = feature$geometry()$buffer(90,1);
      idf = inf ;

      id = paste0(idf,'_', reg, "_V", versionFuelModel)
      message(id)
      assetid <- paste0(assetRootPredictedStack,id)
      message(assetid)

      if( is.element(assetid, tb$name)  ) {
         if(forceRecreation)  {
           ee$data$deleteAsset(assetid)
         } else {
           message(assetid, " exists, skipping")
           next
         }
      }

      #clcplus$gt(1)$And(clcplus$lt(5) )
       image_to_classify <- predictors$clip(gg)$ updateMask( tcd$gte(1L) )
       final_classification <- image_to_classify$classify(classifier)

      ee_image_to_asset(
        image= final_classification$arrayFlatten(list(as.list(classValuesRbn)) )$clip(gg),
        description= id,
        assetId= assetid,
        region= gg,
        # scale=1000L,
        crs         = proj_3035_10m$crs,
        crsTransform = proj_3035_10m$crsTransform,
        maxPixels= 1e13
      )$start()
    }
  }

  ## FINISHED CLASSIFIED STACK ------

  ## STARTED  final classification prob and class ------
  ## ##### NOT REALLY NEEDED BECAUSE SCRIPT 14 USES DIRECTLY THE LAYERS
#   list = ee$data$listAssets(assetRootClassified);
#   tb <- tryCatch({
#     data.frame(name=sapply(list$assets, function(x){x[["name"]]}))
#   }, error = function(e){
#     as.data.frame(list$assets)
#   } )
#
#   for(reg in c("pilotRegions")){
#     obj <- get(reg)
#     ps_list <- obj$toList(obj$size())
#     n <- obj$size()$getInfo()
#     tp = reg
#     # --- Loop over sites   ---
#     for (i2 in seq_len(n) - 1) {
#       feat <- ee$Feature(ps_list$get(i2))
#       # if(nm!="AT-IT") next
#       inf <- feat$get("pilot_id")$getInfo()
#       if(is.null(inf)){
#         inf <- feat$get("ID")$getInfo()
#       }
#
#       feature = feat
#       gg = feature$geometry()$buffer(90,1);
#       idf = inf ;
#
#       id = paste0(idf,'_', reg, "_V", versionFuelModel)
#       message(id)
#       assetid <- paste0(assetRootClassified,id)
#       message(assetid)
#
#       if( is.element(assetid, tb$name)  ) {
#         if(forceRecreation)  {
#           ee$data$deleteAsset(assetid)
#         } else {
#           message(assetid, " exists, skipping")
#           next
#         }
#       }
#
#       assetidIN <- paste0(assetRootPredictedStack,id)
#       #clcplus$gt(1)$And(clcplus$lt(5) )
#       final_classification <-  ee$Image(assetidIN)
#       mapbands <-ee_utils_pyfunc(function(band) {
#         bandNameString <- ee$String(band);
#         bandValue = ee$Number$parse(bandNameString$slice(1L));
#         constantBand = ee$Image$constant(bandValue)$byte()$rename('class');
#         return(final_classification$select(bandNameString)$rename('prob')$multiply(100L)$addBands(constantBand));
#       })
#       classifiedImage <- ee$ImageCollection(ee$List(final_classification$bandNames())$map(mapbands))$qualityMosaic('prob')
#
#       ee_image_to_asset(
#         image= classifiedImage$byte()$clip(gg),
#         description= id,
#         assetId= assetid,
#         region= gg,
#         # scale=1000L,
#         crs         = proj_3035_10m$crs,
#         crsTransform = proj_3035_10m$crsTransform,
#         maxPixels= 1e13
#       )$start()
#     }
#   }
#
#   ## FINISHED  final classification prob and class ------
#
# }
# train$first()$geometry()$projection()$getInfo()
### RANDOM FOREST FINISHED -----
###
###
############################
## FINAL DECISION TREE -----
############################
assetRootOutput = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelV4/';

outputStack_scottBurgan <- {}
outputStack_macroClass <- {}
# MACRO Classes -----
# 91 Urban or suburban development; insufficient wildland fuel ----
outputStack_scottBurgan$a91 = clcplus$eq(1)
outputStack_macroClass$a91 = clcplus$eq(1)
#outputStack_FBP$a101= clcplus$eq(1)

# 98 water ----
outputStack_scottBurgan$a98 = clcplus$eq(10)
outputStack_macroClass$a98 =  clcplus$eq(10)
#outputStack_FBP$a102 = clcplus$eq(10)

# 99 barren ----
outputStack_scottBurgan$a99 = clcplus$gt(7)$And(clcplus$lt(10) )
outputStack_macroClass$a99  = clcplus$gt(7)$And(clcplus$lt(10) )
#outputStack_FBP$a105     =    clcplus$gt(7)$And(clcplus$lt(10) )$Or(clcplus$eq(11) )

# 92 snow ice ----
outputStack_scottBurgan$a92 = clcplus$eq(11)
outputStack_macroClass$a92 = clcplus$eq(11)

# GRASS (10)  ----
grassCLCplus=clcplus$eq(6)$Or(clcplus$eq(7))
#outputStack_FBP$a32     =    clcplus$eq(6)$Or(clcplus$eq(7))

## Grass only if < 10% has vegetation > 1 m
outputStack_macroClass$a10 = grassCLCplus$And(
  # canopy_height$select("b1_min")$eq(0L)$And(
    canopy_height$select("b1")$lte(2L)
  # )
)
## to make sure it is grass, we remove pixels that have any crop type canopy cover
## grass shrub only if > 10% has vegetation > 1 m

# GRASS/SHRUB (12)
outputStack_macroClass$a12= grassCLCplus$And(
  # canopy_height$select("b1_min")$neq(0L)$Or(
    canopy_height$select("b1")$gt(2L)
  # )
)

# SHRUB (14)
outputStack_macroClass$a14=clcplus$eq(5)

plotNDVIgrassShrub <- function(){
  ## PERCENTILES NDVI -----
  # I need to know the percentiles of the NDVI that cover the grass/shrub vegetation,
  # in order to estimate a first idea for fuel load (not trees as trees are
  # saturated )
  ndvi <- list()
  for(i in names(outputStack_macroClass) ){
    if(!is.element(i, c("a10", "a12", "a14"))){
      next
    }
    message(i)
    ndvi[[i]] <- inputVars$ndviMax$multiply(outputStack_macroClass[[i]])$mask(outputStack_macroClass[[i]] )$reduceRegions(
      reducer=  ee$Reducer$fixedHistogram(-0.5, 1.0, 150),
      collection= pilotSites,
      scale= 30
    )$getInfo()


  }

  ndvi2 <- lapply( ndvi, function(ii){

    ff<- lapply( as.list(ii$features) , function(x){
      mm <- matrix(unlist(x$properties$histogram), ncol=2, byrow = T)
      df <- data.frame(pilotSite=x$properties$pilot_id , mid=mm[,1]+0.01 , count=mm[,2], density=mm[,2]/sum(mm[,2]))
      df$cdf <- cumsum(df$density)

      df
    })

    df <- data.table::rbindlist(ff)
    df
  })

  dtf <- data.table::rbindlist(ndvi2, idcol = "Class")


  pdf(file="NDVI.pdf", height=12, width=12)

    ggplot(dtf |> dplyr::filter(mid>0.3) , aes(x = mid, y = density, color = Class)) +
      geom_line(linewidth = 1.3) +
      facet_wrap(~pilotSite, scales = "free_x") +
      labs(
        x = "NDVI Value",
        y = "Relative Frequency",
        subtitle = "S&B Macro Classes: a10 = Grass, a12 = Grass/Shrub, a14=Shrub",
        title = "NDVI freq. distribution across Pilot sites and S&B Macro Class"
      ) +
      theme_minimal(base_size = 23)

  dev.off()


}

# TREES (16/18/20)
## CANOPY LOSS MAP ----
hansen = ee$Image("UMD/hansen/global_forest_change_2025_v1_13")
NonDisturbedPixels =  hansen$select("lossyear")$unmask()$eq(0L);
DisturbedPixels =  hansen$select("lossyear")$unmask()$gt(0L);
hansenLossYear =      hansen$select("lossyear")$unmask();
hansenLossPost2016 =      hansenLossYear$gt(16L);
hansenLossPost2010  =     hansenLossYear$gt(10);
hansenLossPost2010upTo2019 = hansenLossPost2010$And( hansenLossYear$lt(19)  ) ;
hansenLossPost2000   =  hansenLossYear$gt(0)
hansenLossPost2000upTo2009   =  hansenLossPost2000$And( hansenLossYear$lt(11) ) ;

forestLoss4fire = ee$Image("users/sashatyu/2001-2024_fire_forest_loss/EUR_fire_forest_loss_2001-24")
forestLoss4fire.fire = forestLoss4fire$unmask()$gt(1L)
forestLoss4fire.NonFire = forestLoss4fire$unmask()$eq(1L)

CLCtrees <- clcplus$eq(2)$Or( clcplus$eq(3) )$Or( clcplus$eq(4) )
CLCtrees.NonDisturbed <- CLCtrees$And( NonDisturbedPixels )
CLCtrees.DisturbedPost2016 <- CLCtrees$And( hansenLossPost2016 )
CLCtrees.NotDisturbedPost2016 <- CLCtrees$And( hansenLossPost2016$Not() )
CLCtrees.DisturbedPost2016.FIRE <- CLCtrees.DisturbedPost2016$And( forestLoss4fire.fire )
CLCtrees.DisturbedPost2016.notFIRE <- CLCtrees.DisturbedPost2016$And( forestLoss4fire.NonFire )
CLCtrees.Disturbed <- CLCtrees$And( DisturbedPixels )

NBRdistribution <- function(){

  type <- list(CLCtrees.NonDisturbed, CLCtrees.DisturbedPost2016, CLCtrees.Disturbed)
  names(type) <- c("Not Disturbed", "Disturbed Post 2016", "Disturbed Post 2000")

  dfs <- lapply( names(type) , function(d){

    ddd <- type[[d]]
    task <- ee_image_to_drive(
      image       = ee$ImageCollection(yearly_NBR_collection)$toBands()$clip(pilotSites$first()$geometry()),
      description = sprintf("yearlyNBR_%s", d),
      folder      = "expGEE",
      region      = pilotSites$first()$geometry(),
      scale       = 30,
      timePrefix = F,
      crs         = "EPSG:3035",
      maxPixels   = 1e13
    )
    task$start()
  #   inputVars$nbrMax$reduceRegions(
  #   reducer=  ee$Reducer$fixedHistogram(-0.76, 0.5, 100),
  #   collection= pilotSites,
  #   scale= 30
  # )$getInfo()

  })
  names(dfs) <- names(type)
  dfs2 <- lapply( dfs, function(ii){

    ff<- lapply( as.list(ii$features) , function(x){
      mm <- matrix(unlist(x$properties$histogram), ncol=2, byrow = T)
      df <- data.frame(mid=mm[,1]+0.01 , count=mm[,2], density=mm[,2]/sum(mm[,2]))
      df$cdf <- cumsum(df$density)
      df
      })

    names(ff) <- pilotSitesNames
    df <- data.table::rbindlist(ff,idcol = "site")
    df
  })

  # names(dfs2) <- c("disturbedPost2000", "disturbedPost2016", "nonDisturbed")

  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(purrr)

  cdf_dfMax <- data.table::rbindlist(dfs2, idcol = "Type")
  # bind them into one tall table, adding a column telling which group they came from
  ggplot(cdf_dfMax , aes(x = mid, y = density, color = Type)) +
    geom_line(linewidth = 1) +
    facet_wrap(~site, scales = "free_x") +
    labs(
      x = "Value (bin midpoint)",
      y = "CDF",
      title = "CDF per dNBR across Sites"
    ) +
    theme_minimal(base_size = 13)

}

##  blowdown - forest with disturbance but with low inverse nbr (high inverse nbr = charred fuel)
outputStack_macroClass$a20= CLCtrees$And( CLCtrees.DisturbedPost2016.notFIRE )
##  tree timber understorey
outputStack_macroClass$a16= CLCtrees$And( CLCtrees.DisturbedPost2016.notFIRE$Not() )
##  tree1 timber litter
outputStack_macroClass$a18= CLCtrees$And( CLCtrees.DisturbedPost2016.notFIRE$Not() )



onlyMacroClass <- F
if(!onlyMacroClass){

  ndviThresholds <- c(0.7, 0.8, 0.9)
  # GRASS SPARSE
  outputStack_scottBurgan$a101=outputStack_macroClass$a10$multiply( ndviMax$lt(ndviThresholds[[1]]))
  # GRASS LOW load ----
  outputStack_scottBurgan$a102=outputStack_macroClass$a10$multiply(ndviMax$gte(ndviThresholds[[1]])$And(ndviMax$lt(ndviThresholds[[2]]))) #$multiply(aridityIndex$lte(aridityThreshold))
  # GRASS MOD load ----
  outputStack_scottBurgan$a104  = outputStack_macroClass$a10$And( ndviMax$gte( ndviThresholds[[2]] ) )

  # grass HIGH load ----
  # grassHighLoad=grassCLCplus$And(ndviMax$gte(ndviThresholds[[3]]))
  ## 107 dry ----
  # outputStack_scottBurgan$a107=grassHighLoad$multiply(aridityIndex$lte(aridityThreshold))
  ## 108 wet ----
  # outputStack_scottBurgan$a108=grassHighLoad$multiply(aridityIndex$gt(aridityThreshold))

  # SHRUB GRASS ----
  # SHRUB GRASS LOW load ----
  outputStack_scottBurgan$a121  = outputStack_macroClass$a12$multiply( ndviMax$lt( ndviThresholds[[2]] ) )
  outputStack_scottBurgan$a122  = outputStack_macroClass$a12$multiply( ndviMax$gte( ndviThresholds[[2]] ) )

  #SHRUB  ----
  #SHRUB low load ----
  ## 141 dry ----
  outputStack_scottBurgan$a142  = outputStack_macroClass$a14$multiply( ndviMax$lt( ndviThresholds[[2]] ) )
  outputStack_scottBurgan$a145  = outputStack_macroClass$a14$multiply( ndviMax$gte( ndviThresholds[[2]] ) )

  ########## SLASH BLOWDOWN USING HANSEN LOSS -------
  ## areas with 100% canopy cover and trees 25 meters or above
  ## will have class 204 high load.
  ## load is lowered depending on density and tree height
  sb = outputStack_macroClass$a20$multiply( canopy_cover$select(0) )$
                                  multiply( canopy_height$select(0) )$
                                  divide(1000)$
                                  unmask()

  outputStack_scottBurgan$a201 = outputStack_macroClass$a20$multiply(sb$lt(1))
  outputStack_scottBurgan$a202 = outputStack_macroClass$a20$multiply(sb$gte(2))

}

outputStack_scottBurganStack <- list()


for(k in names(outputStack_scottBurgan)){
  bv = as.integer(substr(k, 2,4))
  message(bv)
  nouse =  outputStack_scottBurgan[[k]]$select(0)$projection()
  newBand = ee$Image$constant( bv )$toByte()$
    rename('new_band')$
    setDefaultProjection(nouse);

  outputStack_scottBurganStack[[k]] = outputStack_scottBurgan[[k]]$
    unmask()$multiply(100L)$
    toByte()$
    addBands(newBand)$
    rename(c("prob","class") )$
    reduceResolution(
      reducer   = ee$Reducer$mean(),
      maxPixels = 2048L  )

}


predictedC <- ee$ImageCollection(file.path(dirname(assetRootClassified), basename(assetRootClassified)))
predicted <- predictedC$mosaic()$setDefaultProjection(predictedC$first()$projection())
bn <- predicted$bandNames()$getInfo()
for( k in bn ){
  bv = as.integer(substr(k, 2,4))
  message(bv)

  if(is.na(bv)){
    browser()
  }
  nouse =  predicted$select(bn)$projection()
  newBand = ee$Image$constant( bv )$toByte()$
    rename('new_band')$
    setDefaultProjection(nouse)

    outputStack_scottBurganStack[[k]] = predicted$select(k)$
                                                unmask()$multiply(100L)$
                                                toByte()$
                                                addBands(newBand)$
                                                rename(c("prob","class") )$
                                                reduceResolution(
                                                    reducer   = ee$Reducer$mean(),
                                                    maxPixels = 2048L  )

}




ScottBurganProbs=ee$ImageCollection( unname(outputStack_scottBurganStack) )
ScottBurganProbs$size()$getInfo()
nouse = ScottBurganProbs$first()$projection()


ScottBurgan=ScottBurganProbs$qualityMosaic('prob')$
  setDefaultProjection(nouse)$
  rename(c('scottburgan_cprob', 'scottburgan_class') )

# for(reg in c("pilotRegions", "pilotSites")){

for(reg in c("pilotRegions")){
  obj <- get(reg)
  ps_list <- obj$toList(obj$size())
  n <- obj$size()$getInfo()
  tp = reg
  # --- Loop over sites & bands ---
  for (i2 in seq_len(n) - 1) {

    feat <- ee$Feature(ps_list$get(i2))
    # if(nm!="AT-IT") next
    inf <- feat$get("pilot_id")$getInfo()
    if(is.null(inf)){
      inf <- feat$get("ID")$getInfo()
    }

    nm <- paste0(tp, "_", inf, "_FuelModel" )
    geom <- feat$geometry()$buffer(90, 1)

    img_export <- ScottBurgan$clip(geom)
    # ScottBurganProbs$toBands()
    ee$data$deleteAsset(paste0( assetRootOutput, nm ))
    message(paste0( assetRootOutput, nm ))
    task1 <- ee_image_to_asset(
      image       = img_export$toInt16(),
      description = paste0(nm, "probs Asset" ),
      assetId      = paste0( assetRootOutput, nm ),
      region      = geom,
      scale       = 30,
      crs         = proj_3035_30m$crs,
      crsTransform = proj_3035_30m$crsTransform,
      maxPixels   = 1e13
    )$start()

    message(nm)


    task <- ee_image_to_drive(
      image       = img_export$toInt16(),
      description = paste0(nm, "probs" ),
      folder      = "WildfireProbsV4",
      region      = geom,
      timePrefix = F,
      scale       = 30,
      formatOptions =   list( cloudOptimized= TRUE),
      crs         = proj_3035_30m$crs,
      crsTransform = proj_3035_30m$crsTransform,
      maxPixels   = 1e13
    )$start()

  }
}

