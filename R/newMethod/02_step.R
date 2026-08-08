
#########################################################
######################## LOAD TRAINING ##################
#########################################################
## training data -----
extractTestData <- function(){
  tud <- terra::rast("validation/fueltype_bohemiansaxonswitzerland_postfire.nc")
  ## requires complicated LUT as things do not match XLSX in https://zenodo.org/records/8159023

  tud[tud > 10000 ] <- trunc(tud[tud > 10000 ]/10)
  tud[tud == 1305 ] <- 1303
  tud[tud == 1306 ] <- 1303

  tud[tud == 1304 ] <- 1302
  tud[tud == 1303 ] <- 1302

  tud[tud == 1302 ] <- 1301

  # unique(values(tud))
  # tud[tud > 10000 ]

  ## LUT from https://essd.copernicus.org/articles/15/1287/2023/#section4
  ## not enough
  lookup_A <- c(
    "1111" = 147,  "1112" = 161, "1113" = 147,
    "1121" = 145, "1122" = 165, "1123" = 145,
    "1211" = 147, "1212" = 161, "1213" = 147,
    "1221" = 145, "1222" = 165, "1223" = 145,
    "1301" = 147, "1302" = 165,
    "1303" = 147,
    "21"   = 142, "22"   = 147,
    "23"   = 145, "31"   = 102, "32"   = 104, "33"   = 107,
    "41"   = 104, "42"   = 102, "51"   = 147, "52"   = 145,
    "53"   = 107, "61"   = 91,  "62"   = 142, "7"    = 91, "71"    = 91, "72"    = 91,
     "8"   = 98,  "81"   = 98,  "82"   = 99,
    "63"   = 98,  "61"   = 98,  "62"   = 98

  )
  reclassed_raster_A <- subst(
    x    = tud,
    from = as.numeric(names(lookup_A)),
    to   = as.numeric(lookup_A)
  )
  unique(values(reclassed_raster_A))
  writeRaster(reclassed_raster_A, "validation/sb_crosswalk_A.tif", overwrite = TRUE)
}

## training data -----
extractTrainAndValidationData <- function(force=F){
  if(file.exists("extractTrainAndValidationData.rda")){
    load("extractTrainAndValidationData.rda", envir = .GlobalEnv)
    return(dt)
  }
  train <- list(reclassed_raster_A)
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
  dt <- list(train=train, validation=validation)
  save(dt, file="extractTrainAndValidationData.rda")
  dt
}

dt <- extractTrainAndValidationData()

#########################################################
######################## SAMPLE TESSERA ON TRAINING ##################
#########################################################
basename(path.TesseraTiles$location)

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

      t <- dt[[tnSup]][[tn]]
      t.tr <- st_transform(t, st_crs(path.TesseraTiles))
      # t.tr
      result <- st_join(t.tr, path.TesseraTiles, join = st_intersects)
      result$id <- seq_len(nrow(result))
      message("Sampling" )
      system.time({
        clc.values <- terra::extract(path.CLCplus$`Raster Layer`, terra::vect(result), raw=T, ID=F )
      })
      system.time({
        CH.values <- terra::extract(path.CHM$rast.values, terra::vect(result), raw=T, ID=F )
      })
      if(sum(is.na(clc.values))>0){
        browser()
      }
      message("NA values in CLC value point extraction: ",  sum(is.na(clc.values)) )
      result$clc.values <- clc.values[,1]
      result$treeHeight.values <- CH.values[,1]
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
      result2$class <-  result2$class
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
DT.all <- rbind(TandV$train,
                TandV$validation)
DT.all$clc.values <- factor(
  DT.all$clc.values,
  levels = 1:11,
  labels = levels(clc_classes)
)
DT.all$class <- as.factor(DT.all$class)
DT.all$macro.class<- NULL




plotsNmatrices()  <- function(){
  mat <- table( trunc(DT.all$class/10),
                DT.all$clc.values  )



  writexl::write_xlsx(
    list(Table = cm_wide <- cbind( rownames(mat),
                                   as.data.frame.matrix(as.matrix(mat))
    )  ) ,  "table.xlsx"
  )

  names(clcplus_colors) <- levels(clc_classes)
  library(ggplot2)

  DT.all$treeHeight.values[ is.na(DT.all$treeHeight.values) ] <- 0
  DT.all$macro.class <- factor( trunc((DT.all$class/10)),
                             levels=sort(unique(trunc(as.numeric(DT.all$class/10)))),
                             labels=c("NB", "GR", "GS", "SH", "TU", "TL", "SB")
  )
  DT.all$clc.values <- factor(
    DT.all$clc.values,
    levels = 1:11,
    labels = levels(clc_classes)
  )

  p1 <- ggplot(DT.all,
               aes(x=clc.values, y=treeHeight.values )
  ) + ggplot2::geom_violin(aes(color=cut(as.numeric(clc.values), breaks=c(0,1,5,8,9)))) +
    theme_classic() +
    xlab("CLC+ Classes") +
    ylab("Tree Heights (m) ") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )

  ggsave("violin.png", plot = p1, width = 9, dpi = 300)
  plot_data <- DT.all |>
    # filter(class != 142) |>
    group_by(macro.class, clc.values) |>
    summarise(
      median_height = median(treeHeight.values, na.rm = TRUE),
      n = (n()),
      .groups = "drop"
    )

  p2 <- ggplot(plot_data,
         aes(x = macro.class,
             y = median_height,
             fill = clc.values,
             width = log10(n)/10,
             height=0.76
             )) +
    geom_tile(color="black") +
    scale_fill_manual(values = clcplus_colors) +
    theme_classic() +

    labs(
      x = "S&B Classes",
      y = "Median Canopy Heights (m)",
      fill = "CLC+ Class",
      width = "Number of cells"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave("a.png", plot = p2, width = 9, dpi = 300)

}

