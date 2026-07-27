library(this.path)
source(file.path(this.path::this.dir(), "000_global.R"))
########## LAST STEP -
########## REQUIRES ALL PREVIOUS FILES TO HAVE SUCCESFULLY CREATED OUTPUTS
##########
# 2. START ----
### setting previously classified forest S&B probs ----
fuelModelPredictedStackfinal = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelPredictedStackfinal/';
fuelModelPredictedFinal = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelPredictedFinal/';
assetRootClassifiedOnlyForestRF = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelPredictedRF';
fuelModelPredictors = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelPredictorsStack';



# LAYERS ------
## pilot pilotRegions ----
pilotRegions <- ee$FeatureCollection(
  "projects/progetto-eu-h2020-cirgeo/assets/wildfire/pilotRegions"
)

perfect_mask <- ee$Image$constant(1)$setDefaultProjection(proj_3035_30m_ee)
pilotRegionsROI <- pilotRegions$union()$geometry()$buffer(90, 1)
bounds = pilotRegionsROI$bounds()

outputStack_macroClass = { };
outputStack_scottBurgan = { };
# outputStack_FBP = { };
combined_reducerShared <- ee$Reducer$mean()$combine(
  reducer2 = ee$Reducer$min(),
  sharedInputs = TRUE
)


## PREDICTED FOREST CLASSES  -----------
predictedForestC <- ee$ImageCollection( assetRootClassifiedOnlyForestRF )
predictedForest       <- predictedForestC$mosaic()$setDefaultProjection(predictedForestC$first()$projection())

## CREATE PREDICTORS STACK  -----------
predictorsStackC <- ee$ImageCollection( fuelModelPredictors )
predictors       <- predictorsStackC$mosaic()$setDefaultProjection(predictorsStackC$first()$projection())
clcplus <- predictors$select("clcplus")$mask(perfect_mask)
# clcplus <- predictors$select("clcplus")
# canopy_height  <- predictors$select("canopy_height")$mask(perfect_mask)

# canopy_height30m <- canopy_height$mask(perfect_mask)$
#   reduceResolution(
#     reducer = combined_reducerShared,
#     maxPixels = 1024
#   )$mask(perfect_mask)$
#   reproject(
#     crs = canopy_height$select(0L)$projection()$crs(), # Keeps the original Coordinate Reference System
#     scale = 30           # Sets target resolution to 30 meters
#   )$mask(perfect_mask)


# canopy_height_min  <- canopy_height30m$select("canopy_height_min")$mask(perfect_mask)
# canopy_height_mean  <- canopy_height30m$select("canopy_height_mean")$mask(perfect_mask)
# predictors$bandNames()$getInfo()
ndviMax <- predictors$select("ndviMax")$mask(perfect_mask)
ndviMedian <- predictors$select("ndviMedian")$mask(perfect_mask)
canopy_cover <- predictors$select("canopy_cover")$mask(perfect_mask)


# cropmap = ee$Image('projects/progetto-eu-h2020-cirgeo/assets/copernicus/CLMS_CropTypes_RASTER_2021')$select('b1')
# cropmapHighVeg = cropmap$gt(100)$And(cropmap$lt(200))
# cropmapLowVeg = cropmapHighVeg$Not()
# MACRO Classes -----
# 91 Urban or suburban development; insufficient wildland fuel ----
outputStack_scottBurgan$a91 = clcplus$eq(1)
outputStack_macroClass$a91 = clcplus$eq(1)
# outputStack_FBP$a101= clcplus$eq(1)


# 98 water ----
outputStack_scottBurgan$a98 = clcplus$eq(10)
outputStack_macroClass$a98 =  clcplus$eq(10)
# outputStack_FBP$a102 = clcplus$eq(10)

# 99 barren ----
outputStack_scottBurgan$a99 = clcplus$gt(7)$And(clcplus$lt(10) )
outputStack_macroClass$a99  = clcplus$gt(7)$And(clcplus$lt(10) )
# outputStack_FBP$a105     =    clcplus$gt(7)$And(clcplus$lt(10) )$Or(clcplus$eq(11) )

# 92 snow ice ----
outputStack_scottBurgan$a92 = clcplus$eq(11)
outputStack_macroClass$a92 = clcplus$eq(11)

# GRASS (10)  ----
grassCLCplus=clcplus$eq(6)$Or(clcplus$eq(7))
# outputStack_FBP$a32     =    clcplus$eq(6)$Or(clcplus$eq(7))

predictorsC <-  ee$ImageCollection('projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelPredictorsStack')
predictors <- predictorsC$mosaic()$setDefaultProjection( predictorsC$first()$projection() )$unmask()
bands = predictors$bandNames()

trainedModel100vs120 <- ee$Classifier$load('projects/progetto-eu-h2020-cirgeo/assets/wildfire/output/classify100vs120');
classified100vs120 <- predictors$mask(grassCLCplus)$classify(trainedModel100vs120)
# pixel_counts <- classified100vs120$reduceRegion(
#   reducer = ee$Reducer$frequencyHistogram(),
#   geometry = predictorsC$first()$geometry(),
#   scale = 30, # Adjust scale based on your dataset resolution
#   maxPixels = 1e9
# )
# predictorsC$first()$getInfo()
# 4. Bring the data from the server to your local R session
# This extracts the dictionary properties into a named R list
# local_counts <- pixel_counts$getInfo()

## Grass only if < 10% has vegetation > 1 m
outputStack_macroClass$a10 = grassCLCplus$And(classified100vs120$mask(perfect_mask)$eq(100))
#   grassCLCplus$And(
#   # canopy_height_min$eq(0L)$And(
#     canopy_height_mean$lte(2L)
#   # )
# )
## to make sure it is grass, we remove pixels that have any crop type canopy cover
## grass shrub only if > 10% has vegetation > 1 m

# GRASS/SHRUB (12)
outputStack_macroClass$a12 = grassCLCplus$And(classified100vs120$mask(perfect_mask)$eq(120))
# outputStack_macroClass$a12= grassCLCplus$And(
#   # canopy_height_min$neq(0L)$Or(
#     canopy_height_mean$gt(2L)
#   # )
# )

# SHRUB (14)
outputStack_macroClass$a14=clcplus$eq(5)

# TREES (16/18/20)
## CANOPY LOSS MAP ----
hansen = ee$Image("UMD/hansen/global_forest_change_2025_v1_13")
NonDisturbedPixels =  hansen$select("lossyear")$mask(perfect_mask)$eq(0L);
DisturbedPixels =  hansen$select("lossyear")$mask(perfect_mask)$gt(0L);
hansenLossYear =      hansen$select("lossyear");
hansenLossPost2016 =      hansenLossYear$gt(16L)$mask(perfect_mask);
hansenLossPost2010  =     hansenLossYear$gt(10);
hansenLossPost2010upTo2019 = hansenLossPost2010$And( hansenLossYear$lt(17)  ) ;
hansenLossPost2000   =  hansenLossYear$gt(0)
hansenLossPost2000upTo2009   =  hansenLossPost2000$And( hansenLossYear$lt(11) ) ;

forestLoss4fire = ee$Image("users/sashatyu/2001-2024_fire_forest_loss/EUR_fire_forest_loss_2001-24")
forestLoss4fire.fire = forestLoss4fire$mask(perfect_mask)$gt(1L)
forestLoss4fire.NonFire = forestLoss4fire$mask(perfect_mask)$eq(1L)
CLCtrees <- predictedForest$select("class")$gt(0L)  #clcplus$eq(2)$Or( clcplus$eq(3) )$Or( clcplus$eq(4) )

CLCtrees.NonDisturbed <- CLCtrees$And( NonDisturbedPixels )
CLCtrees.DisturbedPost2016 <- CLCtrees$And( hansenLossPost2016 )
CLCtrees.NotDisturbedPost2016 <- CLCtrees$And( hansenLossPost2016$Not() )
CLCtrees.DisturbedPost2016.FIRE <- CLCtrees.DisturbedPost2016$And( forestLoss4fire.fire )
CLCtrees.DisturbedPost2016.notFIRE <- CLCtrees.DisturbedPost2016$And( forestLoss4fire.NonFire )
CLCtrees.Disturbed <- CLCtrees$And( DisturbedPixels )

##  blowdown - forest with disturbance but not from fire!
outputStack_macroClass$a20 = CLCtrees$And( forestLoss4fire.NonFire )$mask(perfect_mask)
##  tree timber understorey
outputStack_macroClass$a16= CLCtrees$And( forestLoss4fire.NonFire$Not()  )$mask(perfect_mask)
# ##  tree1 timber litter
outputStack_macroClass$a18= CLCtrees$And( forestLoss4fire.NonFire$Not()  )$mask(perfect_mask)


onlyMacroClass <- F
if(!onlyMacroClass){
  ndviThresholds <- c(0.7, 0.8, 0.9)
  # GRASS SPARSE
  outputStack_scottBurgan$a101=outputStack_macroClass$a10$multiply( ndviMax$lt(ndviThresholds[[1]]))
  # GRASS LOW load ----
  outputStack_scottBurgan$a102=outputStack_macroClass$a10$multiply(ndviMax$gte(ndviThresholds[[1]])$And(ndviMax$lt(ndviThresholds[[2]]))) #$multiply(aridityIndex$lte(aridityThreshold))
  # GRASS LOW load also agricultural arable land ----
  # outputStack_scottBurgan$a102= outputStack_scottBurgan$a102$add(cropmapLowVeg$mask(perfect_mask)$multiply(2L)) # this will become 3 were already 102+arable land, or 2 in arable land but not 102, giving cropType precedency over others
  # GRASS MOD load ----
  outputStack_scottBurgan$a104 = outputStack_macroClass$a10$And( ndviMax$gte( ndviThresholds[[2]] ) )

  # grass HIGH load ----
  # grassHighLoad=grassCLCplus$And(ndviMax$gte(ndviThresholds[[3]]))
  ## 107 dry ----
  # outputStack_scottBurgan$a107=grassHighLoad$multiply(aridityIndex$lte(aridityThreshold))
  ## 108 wet ----
  # outputStack_scottBurgan$a108=grassHighLoad$multiply(aridityIndex$gt(aridityThreshold))

  # SHRUB GRASS ----
  # SHRUB GRASS LOW load ----
  ## here we also fix the fact that bare land with rocks is mistakenly assigned to CLC class "1" "sealed" which does have
  ## instead some organic burnable material - so if it falls in clc class 1 but has some type of past (Hansen) or present (2023)
  ## tree height, we then assign class 121 or 122 depending on the max ndvi recorded
  outputStack_scottBurgan$a121  = outputStack_macroClass$a12$multiply( ndviMax$lt( ndviThresholds[[2]] ) )
  # outputStack_scottBurgan$a121  = outputStack_scottBurgan$a121$add(cropmapHighVeg$multiply(2L))$Or(
    # clcplus$eq(1)$And(forestLoss4fire$mask(perfect_mask)$Or( canopy_cover$gt(5) )$Or( canopy_height_mean$gt(1L) ) )$multiply( ndviMax$lt( ndviThresholds[[2]] ) )
  # ) # this will become 3 were already 102+arable land, or 2 in arable land but not 102, giving cropType precedency over others
  # outputStack_scottBurgan$a121  = clcplus$eq(1)
  outputStack_scottBurgan$a122  = outputStack_macroClass$a12$multiply( ndviMax$gte( ndviThresholds[[2]] ) )
  # $Or(
  #   clcplus$eq(1)$And(forestLoss4fire$mask(perfect_mask)$Or( canopy_cover$gt(5) )$Or( canopy_height_mean$gt(1L) ) )$multiply( ndviMax$gte( ndviThresholds[[2]] ) )
  # )



  #SHRUB  ----
  #SHRUB low load ----
  ## 141 dry ----
  outputStack_scottBurgan$a142  = outputStack_macroClass$a14$multiply( ndviMax$lt( ndviThresholds[[2]] ) )
  outputStack_scottBurgan$a145  = outputStack_macroClass$a14$multiply( ndviMax$gte( ndviThresholds[[2]] ) )

  # If a forested area has recently burned but still contains a light, compact
  # layer of remaining or newly fallen charred surface fuel that can propagate a
  # fire, the standard choice established by Scott and Burgan is TL1 (Model 181)
  # (Scott & Burgan, 2005)
  #
  #
  outputStack_scottBurgan$a181 = CLCtrees$And( forestLoss4fire.NonFire$Not()  )$And( clcplus$eq(2L)  )
  outputStack_scottBurgan$a182 = CLCtrees$And( forestLoss4fire.NonFire$Not()  )$And( clcplus$neq(2L)  )
  ########## SLASH BLOWDOWN USING HANSEN LOSS -------
  ##  depending on canopy density and tree height the load is inferred
  ##  - cover % multiplied by average canopy height, if above 200  it
  ##  will have class 202 high load, otherwise class 201. E.g. 100% cover and 2 m
  ## high trees, or 50% cover and 4 m trees will provide the boundary value of 200
  ##
  sb = outputStack_macroClass$a20$multiply(  canopy_cover )$
                                  # multiply( canopy_height_mean )$
                                  # divide(200)$
                                  unmask()
  ## lower weight to account for other classes
  outputStack_scottBurgan$a201 = outputStack_macroClass$a20$multiply(sb$lt(50))$multiply(0.5)
  outputStack_scottBurgan$a202 = outputStack_macroClass$a20$multiply(sb$gte(50))$multiply(0.5)

}


### FINAL STACK ------
clcConfidence <- ee$Image("projects/progetto-eu-h2020-cirgeo/assets/copernicus/CLMS_CLCplut_RASTER_2023confidence")

red_mean <- ee$Reducer$mean()$setOutputs(list("prob"))
red_mode <- ee$Reducer$mode()$setOutputs(list("class"))

combined_reducer <- red_mean$combine(
  reducer2 = red_mode,
  sharedInputs = FALSE
)


outputStack_scottBurganStack <- list()
for(k in names(outputStack_scottBurgan)){
  bv = as.integer(substr(k, 2,4))
  message(bv)
  nouse =  outputStack_scottBurgan[[k]]$select(0)$projection()
  newBand = ee$Image$constant( bv )$toByte()$
                          rename('new_band')$
                          setDefaultProjection(nouse);

  # print(outputStack_scottBurgan[[k]]$bandNames()$getInfo())
  outputStack_scottBurganStack[[k]] = outputStack_scottBurgan[[k]]$
    unmask()$multiply(100L)$toByte()$
    addBands(newBand)$
    rename(c("prob","class") )$mask(perfect_mask)$
    reduceResolution(
      reducer   = combined_reducer,
      maxPixels = 2048L  )$reproject( crs=proj_3035_30m$crs, crsTransform=proj_3035_30m$crsTransform )


  # outputStack_scottBurganStack[[k]]$bandNames()$getInfo()
}


predictedC <- ee$ImageCollection('projects/progetto-eu-h2020-cirgeo/assets/wildfire/predictedForestStack')
predictedForestStack <- predictedC$mosaic()$setDefaultProjection(predictedC$first()$projection())
bn <- predictedForestStack$bandNames()$getInfo()
for( k in bn ){
  bv = as.integer(substr(k, 2,4))
  message(bv)

  if(is.na(bv)){
    browser()
  }

  nouse =  predictedForestStack$select(bn)$projection()
  newBand = ee$Image$constant( bv )$toByte()$
    rename('new_band')$
    setDefaultProjection(nouse)

  if(!is.null(outputStack_scottBurganStack[[k]])){
    message(k, " not null")

    pp <- predictedForestStack$select(k)$
      unmask()$multiply(100L)$
      toByte()$
      addBands(newBand)$
      rename(c("prob","class") )

    outputStack_scottBurganStack[[k]] =
      ee$ImageCollection(
        list(outputStack_scottBurganStack[[k]]$updateMask(outputStack_scottBurganStack[[k]]$neq(0)),
             pp$updateMask(pp$neq(0)))
        )$mosaic()$mask(perfect_mask)$setDefaultProjection(predictedC$first()$projection())$reduceResolution(
                reducer   = combined_reducer,
                maxPixels = 2048L
            )
    } else {
      outputStack_scottBurganStack[[k]] = predictedForestStack$select(k)$
        unmask()$multiply(100L)$
        toByte()$
        addBands(newBand)$
        rename(c("prob","class") )$
        reduceResolution(
          reducer   = combined_reducer,
          maxPixels = 2048L  )
    }

  # outputStack_scottBurganStack[[k]]$bandNames()$getInfo()

}




ScottBurganProbs=ee$ImageCollection( unname(outputStack_scottBurganStack) )$map(function(img){ return(img$mask(perfect_mask))})

# ScottBurganProbs$size()$getInfo()
nouse = ScottBurganProbs$first()$projection()

ScottBurgan=ScottBurganProbs$qualityMosaic('prob')

## urban grass
lon <- 14.167767
lat <- 50.789719
## forest
# lon <- 14.161602
# lat <- 50.788607
point <- ee$Geometry$Point(c(lon, lat))
pixel_data <- ScottBurgan$sampleRegions(
  collection = point,
  projection = proj_3035_30m_ee,
  scale = 30,          # Set to your native/desired resolution in meters
  geometries = TRUE   # Keeps the output clean by dropping geometry properties
)
pixel_values <-   pixel_data$getInfo()
print(pixel_values)

# EXPORT TO ASSET STACK AND FINAL MODEL (CLASS+PROBABILITY)
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
    nmConf <- paste0(tp, "_", inf, "_FuelModelConfidence" )
    geom <- feat$geometry()$buffer(90, 1)

    idOut = paste0( nm,'V', versionFuelModel)
    assetidOutStack <- paste0(fuelModelPredictedStackfinal,idOut)
    assetidOutFinal <- paste0(fuelModelPredictedFinal,idOut)


    message("Try to delete ", assetidOutStack)
    tryCatch({
      ee$data$deleteAsset(assetidOutStack)
    }, error=function(e){
      message("did not delete ", assetidOutStack)
    })

    ## EXPORT STACK of fuel models with probability from 0 to 100 ---------
    img_export <- ScottBurganProbs$select("prob")$toBands()$rename(names(outputStack_scottBurganStack) )
    # img_export$bandNames()$getInfo()
    message(nm)
    task <- ee_image_to_asset(
      image       = img_export$clip(geom)$toByte(),
      description =  paste0(nm,"STACK") ,
      assetId= assetidOutStack,
      # folder      = "WildfireFM",
      region      = geom,
      # timePrefix = F,
      scale       = 30,
      # formatOptions =   list( cloudOptimized= TRUE),
      crs         = proj_3035_30m$crs,
      crsTransform = proj_3035_30m$crsTransform,
      maxPixels   = 1e13
    )$start()

    message("Try to delete ", assetidOutFinal)
    tryCatch({
      ee$data$deleteAsset(assetidOutFinal)
    }, error=function(e){
      message("did not delete ", assetidOutFinal)
    })
    ## EXPORT Fuel model with probability ---------
    img_export <- ScottBurgan$clip(geom)
    message(nm)
    task <- ee_image_to_asset(
      image       = img_export$toByte(),
      description =  basename(assetidOutFinal) ,
      assetId= assetidOutFinal,
      # folder      = "WildfireFM",
      region      = geom,
      # timePrefix = F,
      scale       = 30,
      # formatOptions =   list( cloudOptimized= TRUE),
      crs         = proj_3035_30m$crs,
      crsTransform = proj_3035_30m$crsTransform,
      maxPixels   = 1e13
    )$start()

  }
}


## FINAL EXPORT -----
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
    nm2 <- paste0(tp, "_", inf, "_FuelModelStack" )
    nmConf <- paste0(tp, "_", inf, "_FuelModelConfidence" )
    geom <- feat$geometry()$buffer(90, 1)

    idOut = paste0( nm,'V', versionFuelModel)
    assetidOutStack <- paste0(fuelModelPredictedStackfinal,idOut)
    assetidOutFinal <- paste0(fuelModelPredictedFinal,idOut)

    ## EXPORT stack fuel model  ---------
    # img_export <- ScottBurganProbs$select("prob")$toBands()$rename(names(outputStack_scottBurganStack) )
    # # img_export$bandNames()$getInfo()
    # message(nm)
    # task <- ee_image_to_drive(
    #   image       = ee$Image(assetidOutStack),
    #   description =  nm2 ,
    #   folder      = "WildfireFM",
    #   region      = geom,
    #   timePrefix = F,
    #   scale       = 30,
    #   formatOptions =   list( cloudOptimized= TRUE),
    #   crs         = proj_3035_30m$crs,
    #   crsTransform = proj_3035_30m$crsTransform,
    #   maxPixels   = 1e13
    # )$start()

    ## EXPORT Fuel model with probability ---------
    message(nm)
    task <- ee_image_to_drive(
      image       = ee$Image(assetidOutFinal)$select("class")$byte(),
      description =  nm ,
      folder      = "WildfireFM",
      region      = geom,
      timePrefix = F,
      scale       = 30,
      formatOptions =   list( cloudOptimized= TRUE),
      crs         = proj_3035_30m$crs,
      crsTransform = proj_3035_30m$crsTransform,
      maxPixels   = 1e13
    )$start()

    message(nmConf)
    task <- ee_image_to_drive(
      image       = ee$Image(assetidOutFinal)$select("prob")$resample('bilinear')$byte(),
      description =  nmConf ,
      folder      = "WildfireFM",
      region      = geom,
      timePrefix = F,
      scale       = 30,
      formatOptions =   list( cloudOptimized= TRUE),
      crs         = proj_3035_30m$crs,
      crsTransform = proj_3035_30m$crsTransform,
      maxPixels   = 1e13
    )$start()

    message( paste0(nmConf,"CLCconf"))
    task <- ee_image_to_drive(
      image       = ee$Image(assetidOutFinal)$select("prob")$multiply(clcConfidence$resample('bilinear'))$divide(100L)$byte(),
      description =  paste0(nmConf,"CLCconf") ,
      folder      = "WildfireFM",
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
    ndvi[[i]] <- ndviMedian$multiply(outputStack_macroClass[[i]])$mask(outputStack_macroClass[[i]] )$reduceRegions(
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


  pdf(file="NDVImedian.pdf", height=12, width=12)

  ggplot(dtf |> dplyr::filter(mid> 0) , aes(x = mid, y = density, color = Class)) +
    geom_line(linewidth = 1.3) +
    facet_wrap(~pilotSite, scales = "free_x") +
    labs(
      x = "NDVI Value",
      y = "Relative Frequency",
      subtitle = "S&B Macro Classes: a10 = Grass, a12 = Grass/Shrub, a14=Shrub",
      title = "Median NDVI (2021-2024) freq. distribution across Pilot sites and S&B Macro Class"
    ) +
    theme_minimal(base_size = 23)

  dev.off()


}


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

