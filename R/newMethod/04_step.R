source(file.path(this.path::this.dir(), "00_functions.R"))


#-------------------------------------------------------#
#-------------------------------------------------------#
#-------------------------------------------------------#
consenusMatch <- function(rids,
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

  browser()
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

extractOnly <- function(ids, path){
  r <- terra::rast(path[[1]])
  xy <- xy4326[ids,]
  pts <- sf::sf_project(
    from = st_crs(4326)$wkt,
    to   = st_crs(r)$wkt,
    pts  = xy
  )
  dt <- terra::extract(r, pts)
  dt
}
#-------------------------------------------------------#
######################## APPLY MODEL  ##################
#-------------------------------------------------------#



outdir <- "/archivio/shared/geodati/raster/wildfire/CEfuelMap"
dir.create(outdir, showWarnings = F, recursive = T)
dir.create(sprintf("%sConfidence",outdir),showWarnings = F, recursive = T)
setwd(this.path::this.dir())
## read model ---

if(!file.exists("xgb_final.model")) {
 stop("Model not found! Did you run the previous steps?")
}

message("Loading xgboost model...")
final_model <- xgboost::xgb.load("xgb_final.model")
if(!exists("DT.all")){
  message("importing parquet ")
  DT.all <- arrow::read_parquet( "DT.all.parquet" )
  message("imported parquet")
}
levs<-as.integer(levels(DT.all$class))


## chunk input

clcFiles <- list.files( dirname(terra::sources(path.CLCplus$`Raster Layer`)), full.names = T, pattern="\\.tif$")
clcFilesConf <- list.files( dirname(terra::sources(path.CLCplus$`Confidence Layer`)), full.names = T, pattern="\\.tif$")
clcFile <- clcFiles[[1]]
r <- terra::rast(clcFile)
# write_sf(st_buffer(st_union(by_feature = F, st_buffer(path.TesseraTiles,20,nQuadSegs = 0) ), -40, nQuadSegs = 0), "unionTessera.gpkg")
poly_rcrs <- st_transform(st_buffer(st_union(by_feature = F, st_buffer(path.TesseraTiles,20,nQuadSegs = 0) ), -40, nQuadSegs = 0), crs(r))
poly_v <- vect(poly_rcrs)

#--------------------------------------------#
#-------  write color table -----------------#
#--------------------------------------------#
# which(fuel_models$number%in%levs)
fuel_modelsPart <- fuel_models[fuel_models$number%in%levs,]
rgb <- t(col2rgb(fuel_modelsPart$hex))
clr <- data.frame(
  value = fuel_modelsPart$number,
  R = rgb[, 1],
  G = rgb[, 2],
  B = rgb[, 3],
  A = 255,
  label = fuel_modelsPart$code
)
write.table(
  clr,
  file.path(outdir, "000_QGIS_fuel_colors.clr"),
  row.names = FALSE,
  col.names = FALSE,
  quote = FALSE,
  sep = " "
)
#------------------------#
for(clcFile in clcFiles){
  r <- terra::rast(clcFile)

  # path <- grep(pathpart, path.TesseraTiles$location, value = T)
  clcFileConf <- grep( substr(basename(clcFile), 22,34), clcFilesConf, value = T)
  if(length(clcFileConf)!=1){
    browser()
  }
  rConf <- terra::rast(clcFileConf)
  # check intersection
  intersects <- terra::relate(ext(r), poly_v, relation = "intersects")
  if(!intersects[[1]][[1]]) {
    # message(basename(clcFile), " NOT  interecting... skipping")
    next
  }

  if(file.exists(sprintf("%sPre/fuelSB_%s.tif", outdir,
                         substr(basename(clcFile), 19,40 ) ) )
  ){

    message(basename(clcFile), " interecting but exists... SKIPPING...")
    # browser()
    next
  }


    message(basename(clcFile), "  intersecting... starting")

    # remove partially covered parts
    message("...Masking 1")
    rm <- terra::mask(r, poly_v)
    ## get cellids with values
    clc.ids <- terra::cells(rm)
    if(length(clc.ids) < 2) {
      message(basename(clcFile), "  NOT interecting really, skipping... ")
      next
    }
    message("...Masking 2")
    rmConf<- terra::mask(rConf, poly_v)
    message("...Preparing")
    fuel <- terra::rast(rm, vals=98L)
    fuelConf <- terra::rast(rmConf)
    outer <- 1:2
    while(length(outer)!=0){
      clc.xy <- terra::xyFromCell(rm, clc.ids)
      message("......", length(clc.xy), " cell centers to lat long")
      xy4326 <- as.data.table(sf::sf_project(
        from = st_crs(rm)$wkt,
        to   = st_crs(4326)$wkt,
        pts  = clc.xy
      ) )
      names(xy4326) <- c("x","y")
      outer <- which(xy4326$x < bbox[[1]] | xy4326$x > bbox[[3]] |
                       xy4326$y < bbox[[2]] | xy4326$y > bbox[[4]]
      )
      if(length(outer)!=0) {
        clc.ids <- clc.ids[-1*outer]
      } else {
        ## here means we get the final data because loop will exit
        clc.xy <- terra::xyFromCell(rm, clc.ids)
        message("...", nrow(clc.xy), " cell centers to lat long")
        xy4326 <- as.data.table(sf::sf_project(
          from = st_crs(rm)$wkt,
          to   = st_crs(4326)$wkt,
          pts  = clc.xy
        ) )
        names(xy4326) <- c("x","y")
        message("......", length(clc.xy), "==", nrow(xy4326),
                " FINAL NUMBER OF cell centers to lat long")
      }
      if(length(clc.ids)==0){
        stop("Error here , there should be some ids")
      }
    }

    message("...", nrow(clc.xy), "==", nrow(xy4326), " diff is = ",
            nrow(clc.xy) - nrow(xy4326),
            " (should be zero) ... FINAL NUMBER OF cell centers to lat long")
    # xy4326[outer,]
    ## group by tile
    message("...group by Geotessera tile")
    groups <- xy4326[
      ,
      .(idx = list(.I)),
      by = .(
        lon = trunc(x * 10) / 10 + 0.05,
        lat = trunc(y * 10) / 10 + 0.05
      )
    ]

    message("...predict tiles")
    # parallel extraction with progress bar pbmc
    ll2 <- pbmclapply(
     # for(i in
      seq_len(nrow(groups))
      # )
       , function(i)
         {
       # message(i)
        pathpart <- sprintf("%.2f_%.2f", groups$lon[i], groups$lat[i])

        message("......", pathpart)
        path <- grep(pathpart, path.TesseraTiles$location, value = T)
        message(basename(path))
        if(length(path)==0){
          return( warningCondition( sprintf("Pathpart=%s not found", pathpart) ))
        }
        out <- extractOnly(groups$idx[[i]], path )
        # if(is.null(out) || nrow(out)==0){
        #   browser()
        # }
        out
     }
    , mc.cores = 100
      )

    # dt <- rbindlist(ll2)


    ## ASSIGN CLASSES FROM XGBOOST -----
    message("...assign class values")

    isData <- which(unlist(lapply(ll2, is.data.frame)))
    groupsFinal <- groups[isData]

    if(length(isData)==0){
      message(" ###################################  SHOULD NOT BE HERE!! ")
      next
    }
    fin <- data.table::rbindlist(ll2[isData])

    if(nrow(fin)==0){
      message(" ################################### 0 rows... SHOULD NOT BE HERE!! ")
      next
    }
    xy4326Final <- xy4326[ unlist( groupsFinal$idx)]
    chm <- terra::extract(path.CHM$rast.values, xy4326Final)[[2]]
    chm[is.na(chm)] <- 0
    fin$treeHeight.values <-chm
    rm(chm)
    ######## do prediction here ###
    rids <- clc.ids[ unlist( groupsFinal$idx)]
    message("...consensus")

    consenusMatch(rids,
                  fuel,
                  fuelConf,
                  rm,
                  rmConf,
                  fin$pred_prob,
                  levs[as.integer(fin$pred_class)] )





  }


