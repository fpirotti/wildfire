
#########################################################
######################## LOAD TRAINING ##################
#########################################################
## training data -----
extractTrainAndValidationData <- function(){
  train <- list()
  validation <- list()
  ### from CzechGlobe ----

  ## DE CZ for training -----
  train.czg <- terra::rast("validation/WildfireCE_Fuel_map_validation_DE-CZ_CzechGlobe_model.tif")
  train.czg.values <- sf::st_as_sf(as.data.frame(train.czg,  xy=TRUE, na.rm=T, wide=T ), coords=c("x","y"),
                                   crs=terra::crs(train.czg))   |> select(1,2) |> rename(class=1)
  train[["CzechGlobe"]] <- train.czg.values

  ## AT-CZ for validation -----
  valid.czg <- terra::rast("validation/WildfireCE_Fuel_map_validation_AT-CZ_CzechGlobe_model.tif")
  valid.czg.values <- sf::st_as_sf(as.data.frame(valid.czg,  xy=TRUE, na.rm=T, wide=T ), coords=c("x","y"),
                                   crs=terra::crs(valid.czg))   |> select(1,2) |> rename(class=1)
  validation[["CzechGlobe"]] <- valid.czg.values

  ### from BOKU ----
  train.boku <- terra::rast("validation/carinthia_forest_fuel_3416.tif")
  train.boku2 <- terra::aggregate(train.boku, fact=3, fun="mean")
  train.boku2[] <- ifel(train.boku2 %% 1 == 0, train.boku2, NA)
  train.boku.values.sf <- sf::st_as_sf(as.data.frame(train.boku2,
                                                     xy=TRUE, na.rm=T,
                                                     wide=T ),  coords=c("x","y"),
                                   crs=terra::crs(train.boku2)
                                   )

  train.bokuClean <- terra::extract(train.boku, train.boku.values.sf, ID=F, mat=F)
  train.boku.values.sf$match <- train.boku.values.sf[,1][[1]] == train.bokuClean[,1]
  train.boku.values.sf <- train.boku.values.sf |> dplyr::filter(match)  |> select(1,2) |> rename(class=1)
  train[["BOKU"]] <- train.boku.values.sf

  validation.boku <- terra::rast("validation/thayatal_forest_fuel_3416.tif")
  validation.boku2 <- terra::aggregate(validation.boku, fact=3, fun="mean")
  validation.boku2[] <- ifel(validation.boku2 %% 1 == 0, validation.boku2, NA)
  valid.boku.values.sf <- sf::st_as_sf(as.data.frame(validation.boku2,
                                                     xy=TRUE, na.rm=T,
                                                     wide=T ),  coords=c("x","y"),
                                       crs=terra::crs(validation.boku2)
  )

  valid.bokuClean <- terra::extract(validation.boku, valid.boku.values.sf, ID=F, mat=F)
  valid.boku.values.sf$match <- valid.boku.values.sf[,1][[1]] == valid.bokuClean[,1]
  valid.boku.values.sf <- valid.boku.values.sf |> dplyr::filter(match)  |> select(1,2) |> rename(class=1)
  validation[["BOKU"]] <- valid.boku.values.sf
  list(train=train, validation=validation)
}

dt <- extractTrainAndValidationData()

#########################################################
######################## SAMPLE TESSERA ON TRAINING ##################
#########################################################


sampleTrainingAndValidationData <- function(){

  rsample <- function(geom, path){
    r <- terra::rast(path[[1]])
    terra::extract(r, terra::vect(geom),  ID=F)
  }



  dt.predictors.final <- list()
  for(tnSup in names(dt)){
    message(tnSup)

    dt.predictors <- list()
    for(tn in names(dt[[tnSup]])){
      message(tn)

      t <- dt$train[[tn]]
      t.tr <- st_transform(t, st_crs(path.TesseraTiles))
      # t.tr
      result <- st_join(t.tr, path.TesseraTiles, join = st_intersects)
      result$id <- seq_len(nrow(result))
      message("Sampling" )
      system.time({
        clc.values <- terra::extract(path.CLCplus$`Raster Layer`, terra::vect(result), raw=T, ID=F )
      })
      if(sum(is.na(clc.values))>0){
        browser()
      }
      message("NA values in CLC value point extraction: ",  sum(is.na(clc.values)) )
      result$clc.values <- clc.values[,1]
      DT <- data.table(
        location = result$location,
        idx = result$id
      )
      groups <- DT[, .(idx = list(idx)), by = location]
      # parallel extraction with progress bar
      ll2 <- pbmclapply(
        seq_len(nrow(groups)),
        function(i) {
           path <- groups$location[i]
           out <- rsample(result[groups$idx[[i]], "geometry"], path)
           out$id <- groups$idx[[i]]
           out
        },
        mc.cores = 32
      )
      names(ll2)<-groups$location
      fn2 <- rbindlist(ll2)
      setDT(result)     # geometry column is preserved
      setDT(fn2)
      setkey(fn2, id)
      result2 <- fn2[sf::st_drop_geometry(result), on = "id"]
      result2$location<-NULL
      result2$id<-NULL
      result2$geometry<-NULL
      result2$class <- as.factor(result2$class)
      dt.predictors[[tn]] <- result2
    }


     Data <- rbindlist(dt.predictors)
    # # sum(
    #   table(Data$class)
    # # )
    # # sum(
    #   table(Data$clc.values)
    # # )
    dt.predictors.final[[tnSup]] <- Data

  }
  dt.predictors.final
}

TandV<-sampleTrainingAndValidationData()
