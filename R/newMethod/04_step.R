
################################################################################
################################################################################
################################################################################



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
#########################################################
######################## APPLY MODEL  ##################
#########################################################



outdir <- "/archivio/shared/geodati/raster/wildfire/CEfuelMap"
dir.create(outdir, showWarnings = F, recursive = T)
setwd(this.path::this.dir())
## read model ---

if(!file.exists("xgb_final.model")) {
 stop("Model not found! Did you run the previous steps?")
}

final_model <- xgboost::xgb.load("xgb_final.model")

DT.all <- arrow::read_parquet( "DT.all.parquet", col_select = "class")

## chunk input
sf_use_s2(FALSE)
clcFiles <- list.files( dirname(terra::sources(path.CLCplus$`Raster Layer`)), full.names = T, pattern="\\.tif$")
clcFilesConf <- list.files( dirname(terra::sources(path.CLCplus$`Confidence Layer`)), full.names = T, pattern="\\.tif$")
clcFile <- clcFiles[[1]]
r <- terra::rast(clcFile)
poly_rcrs <- st_transform(geometry, crs(r))
poly_v <- vect(poly_rcrs)

## for each corine tile
for(clcFile in clcFiles){
  r <- terra::rast(clcFile)

  # path <- grep(pathpart, path.TesseraTiles$location, value = T)
  clcFileConf <- grep( substr(basename(clcFile), 22,34), clcFilesConf, value = T)
  if(length(clcFileConf)!=1){
    browser()
  }
  rConf <- terra::rast(clcFileConf)
  # check intersection
  intersects <- terra::relate(ext(r), ext(poly_v), relation = "intersects")
  if(!intersects[[1]][[1]]) {
    message(clcFile, " NON interecting")
    next
  }

    message(clcFile, "  interecting... starting")

    # remove partially covered parts
    message("Masking 1")
    rm <- terra::mask(r, poly_v)
    message("Masking 2")
    rmConf<- terra::mask(rConf, poly_v)
    message("Prep")
    fuel <- terra::rast(rm)
    fuelConf <- terra::rast(rm)
    ## get cellids with values
    clc.ids <- terra::cells(rm)
    # xyv <- as.data.frame(rm, xy=TRUE, na.rm=T, wide=T )
    # clc.values <- rm[[1]][clc.ids][,1]
    # clc.values <- factor(
    #   clc.values,
    #   levels = 1:11,
    #   labels = levels(clc_classes)
    # )
    clc.xy <- terra::xyFromCell(rm, clc.ids)
    # pts <- sf::st_as_sf(xyv,  coords=c("x","y"),   crs=terra::crs(rm)   )

    ## project to Lat long to get the tessera tile easily
    xy4326 <- as.data.table(sf::sf_project(
      from = st_crs(rm)$wkt,
      to   = st_crs(4326)$wkt,
      pts  = clc.xy
    ) )

    names(xy4326) <- c("x","y")

    ## group by tile
    message("group by tile")
    groups <- xy4326[
      ,
      .(idx = list(.I)),
      by = .(
        lon = trunc(x * 10) / 10 + 0.05,
        lat = trunc(y * 10) / 10 + 0.05
      )
    ]

    message("Predict")
    # parallel extraction with progress bar pbmc
    ll2 <- lapply(
     # for(i in
      seq_len(nrow(groups))
      # )
       , function(i)
         {
       # message(i)
        pathpart <- sprintf("%.2f_%.2f", groups$lon[i], groups$lat[i])
        path <- grep(pathpart, path.TesseraTiles$location, value = T)
        message(basename(path))
        if(length(path)==0){
          return( warningCondition( sprintf("Pathpart=%s not found", pathpart) ))
        }
        out <- extractAndPredict(groups$idx[[i]], path )
        out

     }
    # , mc.cores = 100
      )

    message("Assign")
    fin <- data.table::rbindlist(ll2)
    rids <- clc.ids[ unlist( groups$idx)]
    fuelConf[rids] <- fin$pred_prob
    fuel[ rids ] <- levs[as.integer(fin$pred_class)]

    levs<-as.integer(levels(DT.all$class))
    levels(fuel) <- data.frame(
      value = fuel_models$number,
      class = fuel_models$code
    )
    coltab(fuel) <- data.frame(value=fuel_models$number, color=fuel_models$hex)
     plot(fuel)


  }


