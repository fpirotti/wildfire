source(file.path(this.path::this.dir(), "00_functions.R"))

library(terra)

plotIt <- function(rown=T, title=NA, tile="", tableTB=NA){


  if(rown) {
    tb_pct <- sweep(tableTB, 1, rowSums(tableTB), "/") * 100
    titadd <- "rowWise"
  } else {
    tb_pct <- sweep(tableTB, 2, colSums(tableTB), "/") * 100
    titadd <- "colWise"
  }
  df <- as.data.frame(as.table(tb_pct))
  names(df) <- c("vCLC", "vPredsMacro", "pct")

  p <- ggplot(df, aes(x = vPredsMacro, y = vCLC, fill = pct)) +
    geom_tile(color = "white", linewidth = 0.3) +
    geom_text(
      aes(label = sprintf("%.1f", pct)),
      size = 3.5
    ) +
    scale_fill_gradientn(
      colours = c(
        "#FFFFE5",  # 0
        "#F7FCB9",  # 25
        "#D9F0A3",  # 50
        "#ADDD8E",  # 75
        "#78C679"   # 100
      ),
      limits = c(2, 100),
      name = "%", na.value = "#eaeaea"
    ) +
    scale_y_discrete(limits = rev) +
    labs(
      x = "Predicted S&B class",
      y = "CLC+ reference class",
      title = sprintf("Matrix CLC tile %s", tile),
      subtitle = ifelse(rown, "Row-normalized percentages", "Column-normalized percentages")
    ) +
    coord_equal()+
    theme_bw(base_size = 13)  +
    theme(
      panel.grid = element_blank(),
      axis.text.x = ggtext::element_markdown(
        angle = 45,
        hjust = 1
      )
      # strip.background = element_rect(fill = "grey90"),
      # axis.text.y = ggtext::element_markdown(face = "bold",size = 12,
      #                                        # fill = "black",
      #                                        padding = unit(c(10, 10, 10, 10), "pt"),
      #   colour = clcplus_colors[rev(as.integer(rownames(tb_pct)))] )
    )

  # p

  if(!is.na(title)){
    if(!dir.exists("plots")) dir.create("plots", showWarnings = F)
    w <- 4 + 1 * length(unique(df$vPredsMacro))
    h <- 4 + 1 * length(unique(df$vCLC))

    ggsave(
      file.path("plots",
                sprintf("%s_%s.png", gsub(" ", "_", title),
                        titadd) ),
      p,
      width = w,
      height = h,
      units = "cm",
      dpi = 300
    )
  } else{
    print(p)
  }
}
# CLC+ classes (rows)------
clc <- c(1, 2, 3, 4, 5, 6, 7, 8,  9, 10, 11)

# Scott & Burgan classes (columns) ----
sb <- c(91, 92, 98, 99, 10, 12, 14, 16, 18, 20)

# LUT for assigning conifer classes to broadleaves and viceversa to
# match CLC+ classes 2 and 3
specialClassCLC23 <- list(conifer= c(181,183,184,185,188),
                          broadlvs=c(182,186,186,187,189) )
# Final S&B class MATRIX ------- 999 means "leave as is" and skip check.
# - 1 and 2 indicates that LUT for Timber Litter conifer vs broadleaves
# is to be used (specialClassCLC23)

M <- matrix(c(
  # 91  92  98  99   10    12    14   16   18   20
    999, 91, 99, 99, 101,  121,  141, 161, 181,  201,   # CLC 1
    181,181,181,181, 181,  181,  181, 181,   1,  201,     # CLC 2
    182,182,182,182, 122,  122,  145, 182,   2,  201,     # CLC 3
    182,182,182,182, 122,  122,  145, 182,   2,  201,  # CLC 4
    121,121,121,121, 122,  999,  999, 999, 145,  999,        # CLC 5
    101,101,101,101, 999,  999, 121,  121, 121,  121,        # CLC 6
    101,101,101,101, 999,  999, 121,  121, 121,  121,        # CLC 7
    101,101,101,999, 101,  101, 101,  101, 101,  101,        # CLC 8 lichens and mosses
    99, 99, 99, 999, 101,  101, 101,  101, 101,  101,         # CLC 9
    98, 98, 999, 98,  98,   98,  98,   98,   98,  98,         # CLC 10
    92, 999, 92, 92,  92,   92,  92,   92,   92,  92          # CLC 11
), nrow = length(clc), byrow = TRUE )


CLCplus2023userAccuracy <- list(
  ALP=c(67, 88.8  , 83.2 , 50, 59.9, 83.2 , 89.1, 81.2, 79.9, 93.7, 80.0),
  CON=c(72.7, 91.5 , 94.6 , 50, 67.5, 85.5 , 97.5, 81, 45.6, 96.2 , 80),
  PAN=c(65.5 , 82.1 , 94.3 , 50, 38.6, 73.2 , 98.0, 81, 45.6, 93.8, 80)
)

SBuserAccuracy <- c("91"=0.7075,
                    "92"=0.9999, ##snow ice
                    "98"=0.8187,
                    "99"=0.4727,
                    "10"=0.881,
                    "12"=0.709,
                    "14"=0.695,
                    "16"=0.669,
                    "18"=0.860,
                    "20"=0.660)


######################## APPLY MODEL  ##################
## OUTDIR is in 00_globals.R ----

dir.create(outdir, showWarnings = F, recursive = T)
dir.create(sprintf("%sConfidence",outdir),showWarnings = F, recursive = T)
setwd(this.path::this.dir())
## get tilen ---
getTileCode <- function(name){
  name<-basename(name)
  sub(".*_(E[0-9]{2}N[0-9]{2})_.*", "\\1", name)
}
## CLC+ source files  ----
rootPathCLC <- "/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023/TIFFs"
rootPathCLCconf <- "/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023confidence/TIFFs"
clcFiles <- list.files( rootPathCLC, full.names = T, pattern="\\.tif$")
clcFilesConf <- list.files( rootPathCLCconf, full.names = T, pattern="\\.tif$")

## S&B source files from XGBoost ('Pre') ----
rootPathSBfuelPredictedML <- "/archivio/shared/geodati/raster/wildfire/CEfuelMapPre"
rootPathSBfuelPredictedMLconf <- "/archivio/shared/geodati/raster/wildfire/CEfuelMapPreConfidence/"
predFiles <- list.files(rootPathSBfuelPredictedML, full.names = T, pattern="\\.tif$")
predFilesConf <- list.files(rootPathSBfuelPredictedMLconf, full.names = T, pattern="\\.tif$")

## convert study area boundaries to CRS of S&B ----
studyArea <- terra::vect(geometry |> st_transform(sf::st_crs(terra::rast(predFiles[[1]]))))


gc()
# for(predFile in predFiles){
# if(grepl("E46N30", predFile)) break
# next
# }
stats <- pbmclapply(predFiles, function(predFile)
    {
  ## START ----

  message(getTileCode(predFile))
  if( any(grepl(getTileCode(predFile), list.files(outdir, pattern="\\.tif$"))) ){
    message(getTileCode(predFile), " - EXISTS")
    # return(NULL)
  }
  clcFile <- grep(getTileCode(predFile), clcFiles, value=T)
  clcFileConf <- grep(getTileCode(predFile), clcFilesConf, value=T)
  predFileConf <- grep(getTileCode(predFile), predFilesConf, value=T)
  if(length(clcFile)!=1 || length(clcFileConf)!=1 || length(predFileConf)!=1){
    message(getTileCode(predFile), " - Problem with length of CLC or CLCPred or predFileConf")
    return(NULL)
  }
  rPredPre <- terra::rast(predFile)

  rPredConfPre <- terra::rast(predFileConf)
  rm <- terra::mask(rPredPre, studyArea)

  ## all ids ----
  cells.ids <- getCellsIDS(rm)

  ## all S&B values ----
  vPreds <- rm[cells.ids][,1]
  vPredsF <- vPreds > 100
  vPredsMacro <- as.integer(vPreds)
  vPredsMacro[vPredsF] <- as.integer(trunc(vPreds[vPredsF]/10))


  ## all CLC+ values ----
  rCLC <- terra::rast(clcFile)
  vCLC <-  rCLC[cells.ids][,1]
  ## matrix.indexes.per.pixel is a 2 column dataframe with index of rows in matrix and index of
  ## columns in matrix M
  matrix.indexes.per.pixel <- cbind(vCLC, match(vPredsMacro, sb))
  colnames(matrix.indexes.per.pixel)<- c("rows.clcplus","cols.sb")
  lutValues <- M[matrix.indexes.per.pixel]

  ## NB cells.ids.ambigous.mask are the indexes of the cells.ids! Not the IDs
  # ambigous.ids <-  cells.ids[which(lutValues != vPredsMacro & lutValues!=999)]
  cells.ids.ambigous.mask  <-  lutValues != vPredsMacro & lutValues!=999
  # ambigous.ids <-  cells.ids[cells.ids.ambigous.mask]


  fuel <- terra::rast(rm)
  fuelConf <- terra::rast(rPredConfPre)

  # message(getTileCode(predFile), " - ", round(length(ambigous.ids)/length(cells.ids)*100), "% ambigous ")
  ## ids without match ----

  # rmConf <- rPredConfPre[] #terra::mask(rPredConfPre, studyArea)
  # cells.ids.comf <- getCellsIDS(rmConf)
  vPredsConf <- rPredConfPre[cells.ids][,1]



  rCLCconf <- terra::rast(clcFileConf)
  vCLCconf <-  rCLCconf[cells.ids][,1]

  # gc()

  vCLCconfWeighted <- (vCLCconf/100) * (CLCplus2023userAccuracy$CON[vCLC]/100)
  if(anyNA(vCLCconfWeighted)){
    stop("NA values in weighted conf")
  }

  vPredsConfWeighted <- vPredsConf/100 * as.numeric(SBuserAccuracy)[matrix.indexes.per.pixel[,2]]
  if(anyNA(vPredsConfWeighted)){
    stop("NA values in weighted conf")
  }

  # head(matrix.indexes.per.pixel)
  message(getTileCode(predFile), " - ", round(sum(cells.ids.ambigous.mask)/length(cells.ids)*100), "% ambigous ")

  CLCwins <- vCLCconfWeighted > vPredsConfWeighted

  statsTb<-list(tile=getTileCode(predFile),
                n=length(cells.ids),
                ambigous=sum(cells.ids.ambigous.mask)/length(cells.ids)*100
  )

  cells.ids.ambigous.mask <-  cells.ids.ambigous.mask & CLCwins

  statsTb$ambigousConfHigherCLC <- sum(cells.ids.ambigous.mask)/length(cells.ids)*100

  if(anyNA(cells.ids.ambigous.mask)){
    stop("NA values in weighted conf")
  }

  # hist(vCLCconfWeighted)
  message(getTileCode(predFile), " - ", round(sum(cells.ids.ambigous.mask)/length(cells.ids)*100), "% ambigous with CLC+ confidence > XGBoost ")

  ## attenzione, sono gli cells.ids! che NON corrispondono agli indices
  which.cells.ids.are.ambigous <-  cells.ids[cells.ids.ambigous.mask]
  ## attenzione, gli indices
  which.indices.are.ambigous <-  which(cells.ids.ambigous.mask)


  clcMsk <- vCLC[cells.ids.ambigous.mask]
  sbMsk  <- vPredsMacro[cells.ids.ambigous.mask]

  key <- (clcMsk - 1L) * 1000 + sbMsk

  msks <- split(which.indices.are.ambigous, key)

  ## Fix class 1 -----
  ### Fix class 1 - 98 -----
  for(clcClass in clc){
    statsTb[[sprintf("%02d",clcClass)]]<-list()
    message(getTileCode(predFile), " - CLC Class ",clcClass)
    for(sbClass in sb){
      statsTb[[sprintf("%02d",clcClass)]][[sprintf("%d",sbClass)]]<-NA
      colIndex <- which(sbClass==sb)
      if(M[clcClass,colIndex ]>900){
        # message("CLC Class ",clcClass," and S&B Class ", sbClass, " skipping.")
        next
      }
      if(sbClass== M[clcClass,colIndex ]) {
        # message("CLC Class ",clcClass," and S&B Class ", sbClass, " no change.")
        next
      }

      cname <- sprintf("clc%02d_sb%s",clcClass, sbClass)

      msk <- msks[[as.character((clcClass - 1L) * 1000 + sbClass)]]

      if(length(msk)==0) {
        statsTb[[sprintf("%02d",clcClass)]][[sprintf("%d",sbClass)]] <- 0
        # message(sprintf("None present here"  ))
        next
      }
      ## special case for class 2 and 3 and 4
      if(M[clcClass,colIndex ]<10){
        # message("CLC Class ",clcClass," and S&B Class ", sbClass, " going to S&B ",
        # specialClassCLC23[ M[clcClass,colIndex ] ] )

        ##  M[clcClass,colIndex ]%%2+1 the modulo is to flip 1 becomes 2 and 2 becomes 1
        mskExtra <- vPreds[msk] %in% specialClassCLC23[ M[clcClass,colIndex ]%%2+1 ][[1]]

        if(length(mskExtra)==0) {
          statsTb[[sprintf("%02d",clcClass)]][[sprintf("%d",sbClass)]] <- 0
          # message(sprintf("None present here"  ))
          next
        }
        statsTb[[sprintf("%02d",clcClass)]][[sprintf("%d",sbClass)]] <- sum(mskExtra)/length(which.indices.are.ambigous)*100
        mtc <- match(vPreds[msk][ mskExtra ], specialClassCLC23[ M[clcClass,colIndex ]%%2+1 ][[1]])
        tryCatch(
          {
            vPreds[msk][mskExtra] <-
              specialClassCLC23[M[clcClass, colIndex]][[1]][mtc]
            vPredsConf[msk][mskExtra] <- ((1-vPredsConf[msk][mskExtra]/100) * (vCLCconf[msk][mskExtra]/100))*100
          },
          warning = function(w) {
            browser()
            NULL
          }
        )
        next
      }

      # message("CLC Class ",clcClass," and S&B Class ", sbClass, " going to S&B ", M[clcClass,colIndex ])

      statsTb[[sprintf("%02d",clcClass)]][[sprintf("%d",sbClass)]] <- length(msk)/length(which.indices.are.ambigous)*100

      if(statsTb[[sprintf("%02d",clcClass)]][[sprintf("%d",sbClass)]]==0 ){
        message(getTileCode(predFile), " -  ERRRR - CLC Class ",clcClass," and S&B Class ", sbClass, " going to S&B ", M[clcClass,colIndex ], " Should NOT be here!")
        next
      }
      # if(is.element(153, cells.ids[cells.ids.ambigous.mask][msk] )) {
      #   message("HERE")
      #   browser()
      # }
      # suppressWarnings({
        vPreds[ msk ] <- M[clcClass,colIndex ]
        vPredsConf[msk] <- ((1-vPredsConf[msk]/100) * (vCLCconf[msk]/100))*100
      # })
    }
  }

  fuel[cells.ids]     <- vPreds
  names(fuel)<- varnames(fuel)
  fuelConf[cells.ids] <- vPredsConf
  names(fuelConf)<- varnames(fuelConf)
  coltab(fuel) <- clr[,1:5]

  writeRaster(fuel, sprintf("%s/%s.tif", outdir,
                            terra::varnames(fuel)[[1]]  ),
              datatype="INT1U", overwrite=T)

  writeRaster(fuelConf, sprintf("%sConfidence/%s.tif", outdir,
                                terra::varnames(fuelConf)[[1]] ),
              datatype="INT1U", overwrite=T)


  tb1 <- table(vCLC, vPreds)
  which2keep <- which(rowSums(tb1)/sum(tb1) > 0.0001)
  tbll <- tb1[which2keep, ]
  # getTileCode(predFile)
  plotIt(T,title = sprintf("All classes %s", getTileCode(predFile)),
         getTileCode(predFile), tbll)
  plotIt(F, title = sprintf("All classes %s", getTileCode(predFile)),
         getTileCode(predFile), tbll)

  tb2 <- table(vCLC, vPredsMacro)
  which2keep2 <- which(rowSums(tb2)/sum(tb2) > 0.0001)
  tbl <- tb2[which2keep2, ]
  # getTileCode(predFile)
  plotIt(T,title = sprintf("Macro classes %s", getTileCode(predFile)),
         getTileCode(predFile), tbl)
  plotIt(F, title = sprintf("Macro classes %s", getTileCode(predFile)),
         getTileCode(predFile), tbl)

  statsTb
}
 ,
mc.cores=8
)

id<-153

idx<-which(ambigous.ids==id)

cat("S&B predicted:",vPreds[id],
      "Confid=", vPredsConf[id], "%",
      "\nWeighted Conf=", vPredsConfWeighted[idx],
      " (Weight=", SBuserAccuracy[matrix.indexes.per.pixel[idx,2]],
      ") \n CLC CLass:", vCLC[id],
    "\n CLC conf", vCLCconf[id],
    "\n Weighted Conf=", vCLCconfWeighted[idx]
)

names(stats) <- getTileCode(basename(predFiles))




