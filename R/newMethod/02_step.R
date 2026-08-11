checkAutocorrelation <- function(){
  class_autocorrelation <- function(r, max_distance = 3000) {

  stopifnot(nlyr(r) == 1)

  # Raster dimensions
  nr <- nrow(r)
  nc <- ncol(r)

  # Pixel size
  rx <- res(r)[1]
  ry <- res(r)[2]

  # Raster as matrix
  z <- matrix(values(r), nrow = nr, ncol = nc)

  # Classes
  classes <- sort(unique(z[!is.na(z)]))

  # Maximum lag in pixels
  max_lag_x <- floor(max_distance / rx)
  max_lag_y <- floor(max_distance / ry)

  results <- vector("list", length(classes))

  results <- pbmclapply(seq_along(classes), function(cc) {

    cls <- classes[cc]

    # Binary indicator
    b <- z == cls

    # NA handling
    b[is.na(z)] <- NA

    # Mean and SD
    mu <- mean(b, na.rm = TRUE)

    result <- vector("list", max(max_lag_x, max_lag_y))

    for (k in seq_len(max(max_lag_x, max_lag_y))) {
      message(k)
      correlations <- c()

      ## horizontal
      if (k <= max_lag_x) {

        a <- b[, 1:(nc-k)]
        d <- b[, (k+1):nc]

        ok <- !is.na(a) & !is.na(d)

        if (sum(ok) > 100) {

          aa <- a[ok]
          dd <- d[ok]

          correlations <- c(
            correlations,
            cor(aa, dd)
          )
        }
      }

      ## vertical
      if (k <= max_lag_y) {

        a <- b[1:(nr-k), ]
        d <- b[(k+1):nr, ]

        ok <- !is.na(a) & !is.na(d)

        if (sum(ok) > 100) {

          aa <- a[ok]
          dd <- d[ok]

          correlations <- c(
            correlations,
            cor(aa, dd)
          )
        }
      }

      ## diagonal \
      if (k <= max_lag_x && k <= max_lag_y) {

        a <- b[1:(nr-k), 1:(nc-k)]
        d <- b[(k+1):nr, (k+1):nc]

        ok <- !is.na(a) & !is.na(d)

        if (sum(ok) > 100) {

          correlations <- c(
            correlations,
            cor(a[ok], d[ok])
          )
        }
      }

      ## diagonal /
      if (k <= max_lag_x && k <= max_lag_y) {

        a <- b[1:(nr-k), (k+1):nc]
        d <- b[(k+1):nr, 1:(nc-k)]

        ok <- !is.na(a) & !is.na(d)

        if (sum(ok) > 100) {

          correlations <- c(
            correlations,
            cor(a[ok], d[ok])
          )
        }
      }

      result[[k]] <- data.frame(
        class = cls,
        lag = k,
        distance = k * mean(c(rx, ry)),
        correlation = mean(correlations, na.rm = TRUE)
      )
    }

    rbindlist(result)
  },
  mc.cores = 30)

  rbindlist(results)
}
ac <- class_autocorrelation(
  r,
  max_distance = 3000
)
library(ggplot2)

ggplot(ac, aes(distance, correlation)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~ class) +
  labs(
    x = "Distance (m)",
    y = "Binary indicator correlation"
  ) + theme_bw()
}
extractAndPredict2 <- function(ids, path){

  r <- terra::rast(path[[1]])
  xy <- xy4326[ids,]
  pts <- sf::sf_project(
    from = st_crs(4326)$wkt,
    to   = st_crs(r)$wkt,
    pts  = xy
  )
  dt <- terra::extract(r, pts)
  if(sum(is.na(dt[,1]))>0){
    cat(path, file="output.log", append = T, sep="\n")
    cat(capture.output(print(xy[1:10,])), file="output.log", append = T, sep="\n")
  }
  chm <- terra::extract(path.CHM$rast.values, as.matrix(xy) )[[1]]
  chm[is.na(chm)] <- 0
  dt$treeHeight.values <-chm
  dt$class <- clc.values[ ids ]

  dt
}

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
  if(!force && file.exists("extractTrainAndValidationData.rda")){
    load("extractTrainAndValidationData.rda", envir = .GlobalEnv)
    return(trainers.vals)
  }

  ### from CzechGlobe ----

  ## DE CZ for training -----
  trainers <- list(
    terra::rast("validation/WildfireCE_Fuel_map_validation_DE-CZ_CzechGlobe_model.tif"),
    terra::rast("validation/WildfireCE_Fuel_map_validation_AT-CZ_CzechGlobe_model.tif"),
    terra::rast("validation/carinthia_forest_fuel_3416.tif"),
    terra::rast("validation/thayatal_forest_fuel_3416.tif")
  )

  trainers.vals <- list()
  cat( as.character(Sys.time()) , file="output.log", sep="\n")
  for(trainer in trainers){

    trainern <- terra::sources(trainer)
    message(basename(trainern))
    cat( "#####################", file="output.log", sep="\n", append=T)
    cat( basename(trainern) , file="output.log", sep="\n", append=T)
    clc.ids <- terra::cells(trainer)
    clc.values <-  trainer[[1]][clc.ids][,1]
    clc.xy <- terra::xyFromCell(trainer, clc.ids)
    xy4326 <- as.data.table(sf::sf_project(
      from = st_crs(trainer)$wkt,
      to   = st_crs(4326)$wkt,
      pts  = clc.xy
    ) )

    names(xy4326) <- c("x","y")

    ## group by tile
    groups <- xy4326[
      ,
      .(idx = list(.I)),
      by = .(
        lon = trunc(x * 10) / 10 + 0.05,
        lat = trunc(y * 10) / 10 + 0.05
      )
    ]

    message(nrow(groups))
    # ll2 <- pbmclapply(
    ll2 <- pbmclapply(
      # for( i in
           seq_len(nrow(groups)),
       function(i)
        {
        pathpart <- sprintf("%.2f_%.2f", groups$lon[i], groups$lat[i])
        path <- grep(pathpart, path.TesseraTiles$location, value = T)
        if(length(path)!=1){
          return( warningCondition( sprintf("Pathpart=%s not found", pathpart) ))
        }
        out <- extractAndPredict2(groups$idx[[i]], path[[1]] )
        out$latTile <- trunc(groups$lat[[i]]*10)
        out
      }
      ,
      mc.cores = 20
    )
    tt <- data.table::rbindlist(ll2)
    trainers.vals[[basename(trainern)]] <- tt

  }

  save(trainers.vals, file="extractTrainAndValidationData.rda")
  trainers.vals
}

if(!file.exists("DT.all.parquet")){
  message("File DT.all does not exist, creating...")
  DT <- extractTrainAndValidationData(T)
  DT.all <-  data.table::rbindlist(DT)
  DT.all <-  na.omit(DT.all)
  # for(ii in 1:129) message(sum(is.na(DT.all[,..ii])))
  DT.all$class <- as.factor(DT.all$class)
  DT.all$macro.class<- NULL
  arrow::write_parquet(DT.all, "DT.all.parquet")
  # DT.all <- arrow::read_parquet( "DT.all.parquet")
  # length(DT.all$class)
  # save(DT.all, file="DT.all.rda")
}

message("File DT.all does exists, loading...")
DT.all <- arrow::read_parquet( "DT.all.parquet")
#########################################################
######################## SAMPLE TESSERA ON TRAINING ##################
#########################################################




plotsNmatrices  <- function(){
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

