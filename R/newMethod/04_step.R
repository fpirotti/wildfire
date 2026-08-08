
################################################################################
################################################################################
################################################################################


getPtsAndPredictors <- function(){

  sf::st_as_sf(as.data.frame(train.boku2,
                             xy=TRUE, na.rm=T,
                             wide=T ),  coords=c("x","y"),
               crs=terra::crs(train.boku2)
  )
}

predH2O <- function(){

}


extractAndPredict <- function(ids, path){
  browser()
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
  dt$clc.values <- clc.values[ids]
  h2oDataFrame <- h2o::as.h2o(dt)
  preds <- h2o.predict(object = best_model, newdata = h2oDataFrame)
  idx <- h2o.which_max(preds[, -1], axis = 1)
  pred_class <- as.data.frame(preds[,1])
  p <- preds[, -1]
  prob <- p[, 1]

  for (i in 2:ncol(p)) {
    prob <- h2o.ifelse(
      idx == i,
      p[, i],
      prob
    )
  }
  pred_prob <- as.data.frame(prob)
  dt <- cbind(pred_class[,1], pred_prob[,1])
  names(dt) <- c("predicted", "probability")
  dt

}
#########################################################
######################## APPLY MODEL  ##################
#########################################################

library(h2o)

h2o.shutdown(F)
# Start H2O
h2o.init()
#   nthreads = -1,
#   max_mem_size = "500G"
# )

outdir <- "/archivio/shared/geodati/raster/wildfire/CEfuelMap"
dir.create(outdir, showWarnings = F, recursive = T)
setwd(this.path::this.dir())
## read model ---
best_model <- h2o.loadModel(path = file.path(this.path::this.dir(), "model", "finalModel"))





## chunk input
sf_use_s2(FALSE)
clcFiles <- list.files( dirname(terra::sources(path.CLCplus$`Raster Layer`)), full.names = T, pattern="\\.tif$")
clcFile <- clcFiles[[1]]
r <- terra::rast(clcFile)
poly_rcrs <- st_transform(geometry, crs(r))
poly_v <- vect(poly_rcrs)

## for each corine tile
for(clcFile in clcFiles){
  r <- terra::rast(clcFile)

  # check intersection
  intersects <- terra::relate(ext(r), ext(poly_v), relation = "intersects")
  if(!intersects[[1]][[1]]) {
    message(clcFile, " NON interecting")
    next
  }

  message(clcFile, "  interecting... starting")
    browser()
    next
    # remove partially covered parts
    rm <- terra::mask(r, poly_v)
    fuel <- terra::rast(rm)
    ## get cellids with values
    clc.ids <- terra::cells(rm)
    # xyv <- as.data.frame(rm, xy=TRUE, na.rm=T, wide=T )
    clc.values <- rm[[1]][clc.ids][,1]
    clc.values <- factor(
      clc.values,
      levels = 1:11,
      labels = levels(clc_classes)
    )
    clc.xy <- terra::xyFromCell(rm, clc.ids)
    # pts <- sf::st_as_sf(xyv,  coords=c("x","y"),   crs=terra::crs(rm)   )

    ## project to Lat long to get the tessera tile easily
    xy4326 <- as.data.table(sf::sf_project(
      from = st_crs(rm)$wkt,
      to   = st_crs(4326)$wkt,
      pts  = clc.xy
    ) )

    names(xy4326) <- c("x","y")

    # setDT(xy4326)
    # xy4326[1:4,]
    # trunc(xy4326*10)/10 + 0.05

    ## group by tile
    groups <- xy4326[
      ,
      .(idx = list(.I)),
      by = .(
        lon = trunc(x * 10) / 10 + 0.05,
        lat = trunc(y * 10) / 10 + 0.05
      )
    ]
    # parallel extraction with progress bar
    ll2 <- pbmclapply(
      seq_len(nrow(groups)),
      function(i) {
        pathpart <- sprintf("%.2f_%.2f", groups$lon[i], groups$lat[i])
        path <- grep(pathpart, path.TesseraTiles$location, value = T)
        if(length(path)==0){
          return( warningCondition( sprintf("Pathpart=%s not found", pathpart) ))
        }
        out <- extractAndPredict(groups$idx[[i]], path )
        out$id <- groups$idx[[i]]
        out
      },
      mc.cores = 80
    )

  }


