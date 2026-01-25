library(rgee)
library(rgeeExtra)
library(stars)
library(googledrive)
########### THIS REQUIRES FIRST THAT THE processing_01_GEE_tileMeta.R!
# 1. Authenticate ----
drive_auth(email = "cirgeo@unipd.it")
ee_Initialize(user = 'cirgeo'  )
## only forest points ----

# ee_install_upgrade()
### setting version ----
versionFuelModel  = 3
# ee_Initialize(quiet = T)
scale = 30
### setting scale ----
# proj3035_30m = ee$Projection('EPSG:3035')$atScale(scale);
proj_3035_30m <- list(
  crs = "EPSG:3035",
  crsTransform = c(30, 0, 4321000, 0, -30, 3210000)
)
proj_3035_10m <- list(
  crs = "EPSG:3035",
  crsTransform = c(10, 0, 4321000, 0, -10, 3210000)
)
# 2. START ----
### setting previously classified forest S&B probs ----
fuelModelPredictedStackfinal = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelPredictedStackfinal/';
fuelModelPredictedFinal = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelPredictedFinal/';
predictedForestStack = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/predictedForestStack';

fuelModelPredictors = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelPredictors';



# LAYERS ------
## pilot sites ----
inputVars <- list()
pilotSites = ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/wildfire/wildfire_pilot_sites_v3") ;
nPilots =  pilotSites$size()$getInfo()
pilotSitesNames =  unlist(Map(function(x){ x$properties$pilot_id } , pilotSites$getInfo()$features) )

pilotRegions <- ee$FeatureCollection(
  "projects/progetto-eu-h2020-cirgeo/assets/wildfire/pilotRegions"
)
bounds = pilotRegions$geometry()$bounds()

outputStack_macroClass = { };
outputStack_scottBurgan = { };
outputStack_FBP = { };

## CREATE PREDICTORS STACK  -----------
predictorsStackC <- ee$ImageCollection( fuelModelPredictors )
predictors       <- predictorsStackC$mosaic()$setDefaultProjection(predictorsStackC$first()$projection())
clcplus <- predictors$select("clcplus")
clcplus <- predictors$select("clcplus")
canopy_height_mean <- predictors$select("canopy_height_mean")
canopy_height_min  <- predictors$select("canopy_height_min")
ndviMax <- predictors$select("ndviMax")
ndviMedian <- predictors$select("ndviMedian")
canopy_cover<- predictors$select("canopy_cover")

cropmap = ee$Image('projects/progetto-eu-h2020-cirgeo/assets/copernicus/CLMS_CropTypes_RASTER_2021')$select('b1')
cropmapHighVeg = cropmap$gt(100)$And(cropmap$lt(200))
cropmapLowVeg = cropmapHighVeg$Not()
# MACRO Classes -----
# 91 Urban or suburban development; insufficient wildland fuel ----
outputStack_scottBurgan$a91 = clcplus$eq(1)
outputStack_macroClass$a91 = clcplus$eq(1)
outputStack_FBP$a101= clcplus$eq(1)


# 98 water ----
outputStack_scottBurgan$a98 = clcplus$eq(10)
outputStack_macroClass$a98 =  clcplus$eq(10)
outputStack_FBP$a102 = clcplus$eq(10)

# 99 barren ----
outputStack_scottBurgan$a99 = clcplus$gt(7)$And(clcplus$lt(10) )
outputStack_macroClass$a99  = clcplus$gt(7)$And(clcplus$lt(10) )
outputStack_FBP$a105     =    clcplus$gt(7)$And(clcplus$lt(10) )$Or(clcplus$eq(11) )

# 92 snow ice ----
outputStack_scottBurgan$a92 = clcplus$eq(11)
outputStack_macroClass$a92 = clcplus$eq(11)

# GRASS (10)  ----
grassCLCplus=clcplus$eq(6)$Or(clcplus$eq(7))
outputStack_FBP$a32     =    clcplus$eq(6)$Or(clcplus$eq(7))

## Grass only if < 10% has vegetation > 1 m
outputStack_macroClass$a10 = grassCLCplus$And(
  canopy_height_min$eq(0L)$And(
    canopy_height_mean$lte(1L)
  )
)
## to make sure it is grass, we remove pixels that have any crop type canopy cover
## grass shrub only if > 10% has vegetation > 1 m

# GRASS/SHRUB (12)
outputStack_macroClass$a12= grassCLCplus$And(
  canopy_height_min$neq(0L)$Or(
    canopy_height_mean$gt(1L)
  )
)

# SHRUB (14)
outputStack_macroClass$a14=clcplus$eq(5)

# TREES (16/18/20)
## CANOPY LOSS MAP ----
hansen = ee$Image("UMD/hansen/global_forest_change_2024_v1_12")
hansen = ee$Image("UMD/hansen/global_forest_change_2024_v1_12")
NonDisturbedPixels =  hansen$select("lossyear")$unmask()$eq(0L);
DisturbedPixels =  hansen$select("lossyear")$unmask()$gt(0L);
hansenLossYear =      hansen$select("lossyear");
hansenLossPost2018 =      hansenLossYear$gt(18L)$unmask();
hansenLossPost2010  =     hansenLossYear$gt(10);
hansenLossPost2010upTo2019 = hansenLossPost2010$And( hansenLossYear$lt(19)  ) ;
hansenLossPost2000   =  hansenLossYear$gt(0)
hansenLossPost2000upTo2009   =  hansenLossPost2000$And( hansenLossYear$lt(11) ) ;

forestLoss4fire = ee$Image("users/sashatyu/2001-2024_fire_forest_loss/EUR_fire_forest_loss_2001-24")
forestLoss4fire.fire = forestLoss4fire$unmask()$gt(1L)
forestLoss4fire.NonFire = forestLoss4fire$unmask()$eq(1L)




CLCtrees <- clcplus$eq(2)$Or( clcplus$eq(3) )$Or( clcplus$eq(4) )

CLCtrees.NonDisturbed <- CLCtrees$And( NonDisturbedPixels )
CLCtrees.DisturbedPost2018 <- CLCtrees$And( hansenLossPost2018 )
CLCtrees.NotDisturbedPost2018 <- CLCtrees$And( hansenLossPost2018$Not() )
CLCtrees.DisturbedPost2018.FIRE <- CLCtrees.DisturbedPost2018$And( forestLoss4fire.fire )
CLCtrees.DisturbedPost2018.notFIRE <- CLCtrees.DisturbedPost2018$And( forestLoss4fire.NonFire )
CLCtrees.Disturbed <- CLCtrees$And( DisturbedPixels )

##  blowdown - forest with disturbance but with low inverse nbr (high inverse nbr = charred fuel)
outputStack_macroClass$a20 = CLCtrees$And( forestLoss4fire.NonFire )

##  tree timber understorey
 outputStack_macroClass$a16= CLCtrees$And( CLCtrees.DisturbedPost2018.notFIRE$Not() )
# ##  tree1 timber litter
 outputStack_macroClass$a18= CLCtrees$And( CLCtrees.DisturbedPost2018.notFIRE$Not() )



onlyMacroClass <- F
if(!onlyMacroClass){
  ndviThresholds <- c(0.7, 0.8, 0.9)
  # GRASS SPARSE
  outputStack_scottBurgan$a101=outputStack_macroClass$a10$multiply( ndviMax$lt(ndviThresholds[[1]]))
  # GRASS LOW load ----
  outputStack_scottBurgan$a102=outputStack_macroClass$a10$multiply(ndviMax$gte(ndviThresholds[[1]])$And(ndviMax$lt(ndviThresholds[[2]]))) #$multiply(aridityIndex$lte(aridityThreshold))
  # GRASS LOW load also agricultural arable land ----
  outputStack_scottBurgan$a102= outputStack_scottBurgan$a102$add(cropmapLowVeg$unmask()$multiply(2L)) # this will become 3 were already 102+arable land, or 2 in arable land but not 102, giving cropType precedency over others
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
  outputStack_scottBurgan$a121  = outputStack_scottBurgan$a121$add(cropmapHighVeg$multiply(2L))$Or(
    clcplus$eq(1)$And(forestLoss4fire$unmask()$Or( canopy_cover$gt(5) )$Or( canopy_height_mean$gt(1L) ) )$multiply( ndviMax$lt( ndviThresholds[[2]] ) )
  ) # this will become 3 were already 102+arable land, or 2 in arable land but not 102, giving cropType precedency over others
  # outputStack_scottBurgan$a121  = clcplus$eq(1)
  outputStack_scottBurgan$a122  = outputStack_macroClass$a12$multiply( ndviMax$gte( ndviThresholds[[2]] ) )$Or(
    clcplus$eq(1)$And(forestLoss4fire$unmask()$Or( canopy_cover$gt(5) )$Or( canopy_height_mean$gt(1L) ) )$multiply( ndviMax$gte( ndviThresholds[[2]] ) )
  )



  #SHRUB  ----
  #SHRUB low load ----
  ## 141 dry ----
  outputStack_scottBurgan$a142  = outputStack_macroClass$a14$multiply( ndviMax$lt( ndviThresholds[[2]] ) )
  outputStack_scottBurgan$a145  = outputStack_macroClass$a14$multiply( ndviMax$gte( ndviThresholds[[2]] ) )

  ########## SLASH BLOWDOWN USING HANSEN LOSS -------
  ##  depending on canopy density and tree height the load is inferred
  ##  - cover % multiplied by average canopy height, if above 200  it
  ##  will have class 202 high load, otherwise class 201. E.g. 100% cover and 2 m
  ## high trees, or 50% cover and 4 m trees will provide the boundary value of 200
  ##
  sb = outputStack_macroClass$a20$multiply(  canopy_cover )$
                                  multiply( canopy_height_mean )$
                                  divide(200)$
                                  unmask()
  ## lower weight to account for other classes
  outputStack_scottBurgan$a201 = outputStack_macroClass$a20$multiply(sb$lt(1))$multiply(0.2)
  outputStack_scottBurgan$a202 = outputStack_macroClass$a20$multiply(sb$gte(1))$multiply(0.2)

}


### FINAL STACK ------
outputStack_scottBurganStack <- list()

clcConfidence <- ee$Image("projects/progetto-eu-h2020-cirgeo/assets/wildfire/CLMS_CLC_Confidence")

red_mean <- ee$Reducer$mean()$setOutputs(list("prob"))
red_mode <- ee$Reducer$mode()$setOutputs(list("class"))

combined_reducer <- red_mean$combine(
  reducer2 = red_mode,
  sharedInputs = FALSE
)

for(k in names(outputStack_scottBurgan)){
  bv = as.integer(substr(k, 2,4))
  message(bv)
  nouse =  outputStack_scottBurgan[[k]]$select(0)$projection()
  newBand = ee$Image$constant( bv )$toByte()$
                          rename('new_band')$
                          setDefaultProjection(nouse);

  outputStack_scottBurganStack[[k]] = outputStack_scottBurgan[[k]]$
    unmask()$multiply(100L)$toByte()$
    toByte()$
    addBands(newBand)$
    rename(c("prob","class") )$
    reduceResolution(
      reducer   = combined_reducer,
      maxPixels = 2048L  )


}


predictedC <- ee$ImageCollection(predictedForestStack)
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

    outputStack_scottBurganStack[[k]] = predictedForestStack$select(k)$
                                                unmask()$multiply(100L)$
                                                toByte()$
                                                addBands(newBand)$
                                                rename(c("prob","class") )$
                                                reduceResolution(
                                                    reducer   = combined_reducer,
                                                    maxPixels = 2048L  )

}




ScottBurganProbs=ee$ImageCollection( unname(outputStack_scottBurganStack) )

# ScottBurganProbs$size()$getInfo()
nouse = ScottBurganProbs$first()$projection()

ScottBurgan=ScottBurganProbs$qualityMosaic('prob')$
  setDefaultProjection(nouse)$
  rename(c('prob', 'class') )



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
      description =  nm ,
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
    # img_exportConf <- ScottBurgan$select(1)$clip(geom)
    # message(nmConf)
    # task <- ee_image_to_drive(
    #   image       = img_exportConf$toByte(),
    #   description =  nmConf ,
    #   folder      = "WildfireFM",
    #   region      = geom,
    #   timePrefix = F,
    #   scale       = 30,
    #   formatOptions =   list( cloudOptimized= TRUE),
    #   crs         = proj_3035_30m$crs,
    #   crsTransform = proj_3035_30m$crsTransform,
    #   maxPixels   = 1e13
    # )$start()
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

    ## EXPORT STACK of fuel models with probability from 0 to 100 ---------
    img_export <- ScottBurganProbs$select("prob")$toBands()$rename(names(outputStack_scottBurganStack) )
    # img_export$bandNames()$getInfo()
    message(nm)
    task <- ee_image_to_drive(
      image       = ee$Image(assetidOutStack),
      description =  nm2 ,
      folder      = "WildfireFM",
      region      = geom,
      timePrefix = F,
      scale       = 30,
      formatOptions =   list( cloudOptimized= TRUE),
      crs         = proj_3035_30m$crs,
      crsTransform = proj_3035_30m$crsTransform,
      maxPixels   = 1e13
    )$start()

    ## EXPORT Fuel model with probability ---------
    img_export <- ScottBurgan$clip(geom)
    message(nm)
    task <- ee_image_to_drive(
      image       = ee$Image(assetidOutFinal),
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

  type <- list(CLCtrees.NonDisturbed, CLCtrees.DisturbedPost2018, CLCtrees.Disturbed)
  names(type) <- c("Not Disturbed", "Disturbed Post 2018", "Disturbed Post 2000")

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

  # names(dfs2) <- c("disturbedPost2000", "disturbedPost2018", "nonDisturbed")

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

