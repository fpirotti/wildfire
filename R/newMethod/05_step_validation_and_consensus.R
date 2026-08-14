source(file.path(this.path::this.dir(), "00_functions.R"))


#-------------------------------------------------------#
#-------------------------------------------------------#
#-------------------------------------------------------#
consensusMatch <- function(rids,
                          fuel,
                          fuelConf,
                          rm,
                          rmConf,
                          pred_prob,
                          pred_class){
  # CLC=>S&B:  11 => 92, 10 => 98, 9 => 99, 8=>103, 7 => 93,
  from <- c(1L, 11L, 10L, 9L, 8L, 7L)
  to   <- c(91L,92L, 98L, 99L, 103L, 93L)

  message("Assigning ",  terra::varnames(fuel)[[1]] )

  length(pred_prob)
  fuelConf[rids] <-  pred_prob
  fuel[ rids ] <-  pred_class
  message("Writing ",  terra::varnames(fuel)[[1]] )
  writeRaster(fuel, sprintf("%sPre/fuelSB_%s.tif", outdir,
                            substr(terra::varnames(fuel)[[1]], 19,40 ) ),
              datatype="INT1U", overwrite=T)
  writeRaster(fuelConf, sprintf("%sConfidencePre/fuelSBCL_%s.tif", outdir,
                                substr(terra::varnames(fuelConf)[[1]], 21,42 ) ),
              datatype="INT1U", overwrite=T)
  return()

  ## get all the corine values
  clc <- rm[[1]][rids][,1]
  ## cross check which belong to the LUT rule-based ones
  i <- which(clc%in%from)

  clcConf <- rmConf[[1]][rids[i] ][,1]

  iindex <- match(clc[i], from)
  pred_class[i] <- to[iindex]
  pred_prob <- pred_prob*100
  pred_prob[i] <- clcConf

  i <- which(clc%in%from)

  fuelConf[rids] <<-  pred_prob
  fuel[ rids ] <<-  pred_class
  ## ASSIGN CLASSES FROM XGBOOST -----
  # levels(fuel) <- data.frame(
  #   value = fuel_models$number,
  #   class = fuel_models$code
  # )

  coltab(fuel) <- data.frame(value=fuel_models$number, color=fuel_models$hex)
  message("Writing ",  terra::varnames(fuel)[[1]] )
  writeRaster(fuel, sprintf("%s/fuelSB_%s.tif", outdir,
                            substr(terra::varnames(fuel)[[1]], 19,40 ) ),
              datatype="INT1U", overwrite=T)
  writeRaster(fuelConf, sprintf("%sConfidence/fuelSBCL_%s.tif", outdir,
                            substr(terra::varnames(fuelConf)[[1]], 21,42 ) ),
              datatype="INT1U", overwrite=T)
}

extractAndPredict <- function(ids, path){

  r <- terra::rast(path[[1]])
  xy <- xy4326[ids,]
  pts <- sf::sf_project(
    from = st_crs(4326)$wkt,
    to   = st_crs(r)$wkt,
    pts  = xy
  )
  dt <- terra::extract(r, pts)
  chm <- terra::extract(path.CHM$rast.values, xy)[[2]]
  chm[is.na(chm)] <- 0
  dt$treeHeight.values <-chm
  p <- predict(final_model, dt )
  data.frame(pred_class=max.col(p),
             pred_prob= apply(p, 1, max)
             )

}

extractOnly <- function(i, ids, path){
  r <- terra::rast(path[[1]])
  xy <- xy4326[ids,]
  pts <- sf::sf_project(
    from = st_crs(4326)$wkt,
    to   = st_crs(r)$wkt,
    pts  = xy
  )
  out <- terra::extract(r, pts)
  out$id <- ids
  arrow::write_parquet(
    out,
    sprintf("tmp/result_%05d.parquet", i)
  )
  NULL
}
#-------------------------------------------------------#
######################## APPLY MODEL  ##################
#-------------------------------------------------------#



outdir <- "/archivio/shared/geodati/raster/wildfire/CEfuelMap"
dir.create(outdir, showWarnings = F, recursive = T)
dir.create(sprintf("%sConfidence",outdir),showWarnings = F, recursive = T)
setwd(this.path::this.dir())
## get tilen ---
getTileCode <- function(name){
  name<-basename(name)
  sub(".*_(E[0-9]{2}N[0-9]{2})_.*", "\\1", name)
}
## chunk input

clcFiles <- list.files( dirname(terra::sources(path.CLCplus$`Raster Layer`)), full.names = T, pattern="\\.tif$")
clcFilesConf <- list.files( dirname(terra::sources(path.CLCplus$`Confidence Layer`)), full.names = T, pattern="\\.tif$")

getTileCode(clcFiles)

predFiles <- list.files(paste0(outdir, "Pre"), full.names = T, pattern="\\.tif$")
predFilesConf <- list.files(paste0(outdir, "PreConfidence"), full.names = T, pattern="\\.tif$")
studyArea <- terra::vect(geometry |> st_transform(sf::st_crs(terra::rast(predFiles[[1]]))))
for(predFile in predFiles){
  break
  clcFile <- grep(getTileCode(predFile), clcFiles, value=T)
  clcFileConf <- grep(getTileCode(predFile), clcFilesConf, value=T)
  predFileConf <- grep(getTileCode(predFile), predFilesConf, value=T)
  r <- terra::rast(predFile)
  rm <- terra::mask(r, studyArea)
  rCLC <- terra::rast(clcFile)
  rCLCconf <- terra::rast(clcFileConf)
  finalFuel <- terra::rast(rm, vals=98)
  finalFuelConf <- terra::rast(rm, vals=0)

}



