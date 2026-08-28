source(file.path(this.path::this.dir(), "00_functions.R"))


#-------------------------------------------------------#
#-------------------------------------------------------#
#-------------------------------------------------------#
createPredRaster <- function(rids,
                          # fuel,
                          # fuelConf,
                          clcFile_,
                          clcFileConf_,
                          pred_prob,
                          pred_class,
                          pred_prob2,
                          pred_class2){

  message("Assigning ",  terra::varnames(fuel)[[1]] )

  fuel <- terra::rast(terra::rast(clcFile_) )
  fuelConf <- terra::rast(terra::rast(clcFileConf_))

  fuelConf[rids] <-  pred_prob
  fuel[ rids ] <-  pred_class
  coltab(fuel) <- clr[,1:5]

  message("Writing ",  terra::varnames(fuel)[[1]] )
  writeRaster(fuel, sprintf("%s/fuelSB_%s.tif", outdir,
                            substr(terra::varnames(fuel)[[1]], 19,40 ) ),
              datatype="INT1U", overwrite=T)
  writeRaster(fuelConf, sprintf("%sConfidence/fuelSBCL_%s.tif", outdir,
                                substr(terra::varnames(fuelConf)[[1]], 21,42 ) ),
              datatype="INT1U", overwrite=T)


  fuel <- terra::rast(terra::rast(clcFile_) )
  fuelConf <- terra::rast(terra::rast(clcFileConf_))

  fuelConf[rids] <-  pred_prob2
  fuel[ rids ] <-  pred_class2
  coltab(fuel) <- clr[,1:5]

  writeRaster(fuel, sprintf("%s2/fuelSB_%s.tif", outdir,
                            substr(terra::varnames(fuel)[[1]], 19,40 ) ),
              datatype="INT1U", overwrite=T)
  writeRaster(fuelConf, sprintf("%sConfidence2/fuelSBCL_%s.tif", outdir,
                                substr(terra::varnames(fuelConf)[[1]], 21,42 ) ),
              datatype="INT1U", overwrite=T)

  NULL
}


# xgb.model.parameters(final_model) <- list(nthread = 1)
# extractAndPredict <- function(i, ids, path){
#   # browser()
#   r <- terra::rast(path[[1]])
#   xy <- xy4326[ids,c("x","y")]
#   pts <- sf::sf_project(
#     from = st_crs(4326)$wkt,
#     to   = st_crs(r)$wkt,
#     pts  = xy
#   )
#   dt <- terra::extract(r, pts)
#   chm <- terra::extract(path.CHM$rast.values, xy)[[2]]
#   chm[is.na(chm)] <- 0
#   dt$treeHeight.values <-chm
#   p <- predict(final_model, dt )
#   data.frame(pred_class=as.integer(max.col(p)),
#              pred_prob= as.integer(apply(p, 1, max)*100)
#             )
#
# }

extractOnly <- function(i, ids, path, xy){

  r <- terra::rast(path[[1]])

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
  rm(out)
  rm(pts)
  NULL
}
#--------------------'''--------------------------------#
######################## APPLY MODEL  ##################
#--------------------'''-----------------------------------#

outdir <- "/archivio/shared/geodati/raster/wildfire/CEfuelMapPre"
dir.create(outdir, showWarnings = F, recursive = T)
dir.create(sprintf("%sConfidence",outdir),showWarnings = F, recursive = T)
dir.create(sprintf("%s2",outdir), showWarnings = F, recursive = T)
dir.create(sprintf("%sConfidence2",outdir),showWarnings = F, recursive = T)
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

rm(DT.all)
gc()
## chunk input

clcFiles <- list.files( dirname(terra::sources(path.CLCplus$`Raster Layer`)), full.names = T, pattern="\\.tif$")
clcFilesConf <- list.files( dirname(terra::sources(path.CLCplus$`Confidence Layer`)), full.names = T, pattern="\\.tif$")
clcFile <- clcFiles[[1]]
r <- terra::rast(clcFile)
# write_sf(st_buffer(st_union(by_feature = F, st_buffer(path.TesseraTiles,20,nQuadSegs = 0) ), -40, nQuadSegs = 0), "unionTessera.gpkg")
poly_rcrs <- st_transform(st_buffer(st_union(by_feature = F, st_buffer(path.TesseraTiles,20,nQuadSegs = 0) ), -40, nQuadSegs = 0), crs(r))
poly_v <- vect(poly_rcrs)


clcFilesThatIntersect<-list()

message( " Getting only intersecting CLC tiles")
for(clcFile in clcFiles){
  r <- terra::rast(clcFile)
  clcFileConf <- grep( substr(basename(clcFile), 22,34), clcFilesConf, value = T)
  if(length(clcFileConf)!=1){
    browser()
  }
  # check intersection
  intersects <- terra::relate(ext(r), poly_v, relation = "intersects")
  if(!intersects[[1]][[1]]) {
    next
  }

  # if(grepl("E48N30", basename(clcFile))){
  #   clcFilesThatIntersect[[basename(clcFile)]]<- clcFile
  #   break
  # }
  if(file.exists(sprintf("%s/fuelSB_%s.tif", outdir,
                         substr(basename(clcFile), 19,40 ) ) )
  ){
    message("Should not be here")
    clcFilesThatIntersect[[basename(clcFile)]]<- TRUE
    next
  }
  clcFilesThatIntersect[[basename(clcFile)]]<- clcFile
}

if(length(clcFilesThatIntersect)==0){
  stop("Problem, no CLC Files found  !")
}
message( length(clcFilesThatIntersect), " CLC tiles to process found")

clcFilesThatIntersect <- clcFilesThatIntersect[order(names(clcFilesThatIntersect))]
i <- 0
for(clcFileN in names(clcFilesThatIntersect) ){
    clcFile<- clcFilesThatIntersect[[clcFileN]]
    i <- i + 1
    if(clcFile==T){
      message("Done ", basename(clcFileN))
      next
    }
    break
    message(basename(clcFile), "  ", i , " of ", length(clcFilesThatIntersect))
    r <- terra::rast(clcFile)
    clcFileConf <- grep( substr(basename(clcFile), 22,34), clcFilesConf, value = T)
    if(length(clcFileConf)!=1){
      stop(paste0(length(clcFileConf), " clcFileConf files... should be one only") )
    }
    # rConf <- terra::rast(clcFileConf)
    # remove partially covered parts
    message("...Masking 1")
    rm <- terra::mask(r, poly_v)
    ## get cellids with values
    clc.ids <- terra::cells(rm)
    if(length(clc.ids) < 2) {
      message(basename(clcFile), "  NOT interecting really, skipping... ")
      next
    }
    # message("...Masking 2")
    # rmConf<- terra::mask(rConf, poly_v)
    # message("...Preparing")
    # fuel <- terra::rast(rm)
    # fuelConf <- terra::rast(rmConf)

    # outer <- 1:2
    # while(length(outer)!=0 ){
      # clc.xy <- terra::xyFromCell(rm, clc.ids)
    message("......converting ", length(clc.ids), " cell centers to lat long")
    xy4326 <- as.data.table(sf::sf_project(
      from = st_crs(rm)$wkt,
      to   = st_crs(4326)$wkt,
      pts  = terra::xyFromCell(rm, clc.ids)
    ) )
    message("......converting FINISHED ", length(clc.ids), " cell centers to lat long")

    names(xy4326) <- c("x","y")
    outer <- which(xy4326$x < bbox[[1]] | xy4326$x > bbox[[3]] |
                     xy4326$y < bbox[[2]] | xy4326$y > bbox[[4]] )
    ## if length outer is zero then all cells are in the box!
    if(length(outer)!=0) {
      clc.ids <- clc.ids[-1*outer]
      xy4326 <- xy4326[-1*outer,]
    }



    if(length(clc.ids)==0){
      message("... no cells falling in study area, will skip")
      next
    }
    message("... N cells diff is = ",
            length(clc.ids) - nrow(xy4326),
            " (should be zero)")
    message("...group by Geotessera tile")
    groups <- xy4326[
      ,
      .(idx = list(.I)),
      by = .(
        lon = trunc(x * 10) / 10 + 0.05,
        lat = trunc(y * 10) / 10 + 0.05
      )
    ]


    message("...predict ", nrow(groups), " tiles")

    memlog("start pbmclapply")

    if(!dir.exists("tmp")) dir.create("tmp", showWarnings = F, recursive = T)
    file.remove(list.files("tmp", pattern = "\\.parquet$", full.names = T) )
    ll2 <- pbmclapply(
      seq_len(nrow(groups)), function(i)
         {

        path <- grep(sprintf("%.2f_%.2f", groups$lon[i], groups$lat[i]), path.TesseraTiles$location, value = T)
        #
        # if(length(path)==0){
        #   return( warningCondition( sprintf("Pathpart=%s not found", pathpart) ))
        # }
         extractOnly(i, groups$idx[[i]], path,  xy =xy4326[groups$idx[[i]],] )

     }, mc.cores = 30
    )
    rm(ll2)
    message("...extraction finished")
    # dt <- rbindlist(ll2)

    files <- list.files(
      "tmp",
      pattern = "^result_[0-9]+\\.parquet$",
      full.names = TRUE
    )
    files <- files[order(files)]
    ds <- arrow::open_dataset(files, format = "parquet")
    batch_size <- 4e7
    n<- ds$num_rows
    # idx <- vector("integer", n)
    nn <- 0
    tot <- ceiling(n/batch_size)
    pred <- list()
    memlog("......before predict")
    for (start in seq(1, n, by = batch_size)) {
      nn<-nn+1
      message("......chunk ", nn, " of ", tot)
      end <- min(start + batch_size - 1, n)
      message("......reading parquet")
      fin <- as.data.frame(ds[start:end,])
      xy4326Final <- xy4326[fin$id,]
      message("......extracting CHM")
      chm <- terra::extract(path.CHM$rast.values, xy4326Final)[[2]]
      chm[is.na(chm)] <- 0
      fin$treeHeight.values <-chm
      ids <- clc.ids[fin$id]
      fin$id<-NULL
      message("......predict")
      p <- predict(final_model, fin )
      # memlog("......after predict")
      # predo  <-     data.frame(pred_class=max.col(p),
      #                         pred_prob= apply(p, 1, max),
      #                         ids=ids )

      p1 <- max.col(p)

      # Replace the maximum with -Inf, then find the second maximum
      p_tmp <- p
      p_tmp[cbind(seq_len(nrow(p_tmp)), p1)] <- -Inf

      p2 <- max.col(p_tmp)

      pred[[nn]] <- data.frame(
        pred_class = p1,
        pred_prob  = p[cbind(seq_len(nrow(p)), p1)],
        second_class = p2,
        second_prob  = p[cbind(seq_len(nrow(p)), p2)],
        ids = ids
      )


    }

    memlog("... before removing stuff")
    rm(fin)
    rm(p)
    rm(p1)
    rm(p2)
    rm(chm)
    rm(clc.ids, p_tmp)
    rm(xy4326, groups)
    rm(xy4326Final, groups)
    rm(ids)
    memlog("... after removing stuff")

    pred <- data.table::rbindlist(pred)


    createPredRaster( pred$ids,
                  # fuel,
                  # fuelConf,
                  clcFile,
                  clcFileConf,
                  pred$pred_prob*100,
                  levs[as.integer(pred$pred_class)],
                  pred$second_prob*100,
                  levs[as.integer(pred$second_class)] )

    memlog("... before pred rem")
    rm(pred)
    rm(rm)
    rm(ids)
    # rm(rmss)
    memlog("... after pred rem")

  }

system(sprintf("gdalbuildvrt  %s/000_mosaic.vrt %s/*.tif",
               outdir, outdir) )
system(sprintf("gdalbuildvrt  %s/000_mosaic.vrt %s/*.tif",
               sprintf("%sConfidence", outdir),  sprintf("%sConfidence", outdir)) )
