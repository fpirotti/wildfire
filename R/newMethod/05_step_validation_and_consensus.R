source(file.path(this.path::this.dir(), "00_functions.R"))

library(terra)

plotIt <- function(rown=T, title=NA, tile=""){
  library(ggplot2)
  library(RColorBrewer)
  library(ggtext)

  if(rown) {
    tb_pct <- sweep(tb, 1, rowSums(tb), "/") * 100
    titadd <- "rowWise"
  } else {
    tb_pct <- sweep(tb, 2, colSums(tb), "/") * 100
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
  # if(grepl("E43N31", predFile)) break
  # next
stats <- pbmclapply(predFiles, function(predFile)
    {
  ## START ----

  message(getTileCode(predFile))
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
  vPredsMacro <- vPreds
  vPredsMacro[vPredsF] <- trunc(vPreds[vPredsF]/10)


  ## all CLC+ values ----
  rCLC <- terra::rast(clcFile)
  vCLC <-  rCLC[cells.ids][,1]
  lutBind <- cbind(vCLC,match(vPredsMacro, sb))
  names(lutBind)<- NULL
  lutValues <- M[lutBind]
  ambigous.ids <-  cells.ids[which(lutValues != vPredsMacro & lutValues!=999)]


  fuel <- terra::rast(rm)
  fuelConf <- terra::rast(rPredConfPre)

  # message(getTileCode(predFile), " - ", round(length(ambigous.ids)/length(cells.ids)*100), "% ambigous ")
  ## ids without match ----

  vPredsConf <- rPredConfPre[cells.ids][,1]



  rCLCconf <- terra::rast(clcFileConf)
  vCLCconf <-  rCLCconf[cells.ids][,1]

  gc()

  vCLCconfWeighted <- vCLCconf[ambigous.ids] * CLCplus2023userAccuracy$CON[vCLC[ambigous.ids]]/10000
  if(anyNA(vCLCconfWeighted)){
    warning("NA values in weighted conf")
  }
  CLCwins <- vCLCconfWeighted > (vPredsConf[ambigous.ids]/100)
  ambigous.ids2 <-  ambigous.ids[which(CLCwins)]
  ambigous.ids2.values <- vCLCconfWeighted[ambigous.ids2]
  # hist(vCLCconfWeighted)
  message(getTileCode(predFile), " - ", round(length(ambigous.ids)/length(cells.ids)*100), "% ambigous ")
  message(getTileCode(predFile), " - ", round(length(ambigous.ids2)/length(cells.ids)*100), "% ambigous with CLC+ confidence > XGBoost ")

  ## make sure CLC+ 2 is conifer-related
  masks.vCLC.ambig2 <- lapply(1:length(clc), function(i){
    vCLC[ambigous.ids2]==i
  })
  masks.vPreds.ambig2 <- lapply(as.character(sb), function(i){
    vPredsMacro[ambigous.ids2]==as.integer(i)
  })


  names(masks.vPreds.ambig2) <- as.character(sb)

  statsTb<-list(n=length(cells.ids),
                    ambigous=length(ambigous.ids)/length(cells.ids)*100,
                    ambigousConf=length(ambigous.ids2)/length(cells.ids)*100
  )

  ## Fix class 1 -----
  ### Fix class 1 - 98 -----
  for(clcClass in clc){
    statsTb[[sprintf("%02d",clcClass)]]<-list()
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

      msk <- which(masks.vCLC.ambig2[[clcClass]] & masks.vPreds.ambig2[[as.character(sbClass)]])

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
        mskExtra <- which(vPreds[ambigous.ids2][msk]%in% specialClassCLC23[ M[clcClass,colIndex ]%%2+1 ][[1]])
        # browser()
        if(length(mskExtra)==0) {
          statsTb[[sprintf("%02d",clcClass)]][[sprintf("%d",sbClass)]] <- 0
          # message(sprintf("None present here"  ))
          next
        }
        statsTb[[sprintf("%02d",clcClass)]][[sprintf("%d",sbClass)]] <- length(mskExtra)/length(ambigous.ids2)*100
        mtc <- match(vPreds[ambigous.ids2][msk][ mskExtra ], specialClassCLC23[ M[clcClass,colIndex ]%%2+1 ][[1]])
        vPreds[ambigous.ids2][msk][ mskExtra ] <- specialClassCLC23[ M[clcClass,colIndex ]  ][[1]][mtc]
        next
      }

      # message("CLC Class ",clcClass," and S&B Class ", sbClass, " going to S&B ", M[clcClass,colIndex ])

      statsTb[[sprintf("%02d",clcClass)]][[sprintf("%d",sbClass)]] <- length(msk)/length(ambigous.ids2)*100

      if(statsTb[[sprintf("%02d",clcClass)]][[sprintf("%d",sbClass)]]==0 ){
        message(getTileCode(predFile), " -  ERRRR - CLC Class ",clcClass," and S&B Class ", sbClass, " going to S&B ", M[clcClass,colIndex ], " Should NOT be here!")
        next
      }
      vPreds[ambigous.ids2][ msk ] <- M[clcClass,colIndex ]
    }
  }

  fuel[]     <- vPreds
  names(fuel)<- varnames(fuel)
  fuelConf[ambigous.ids2] <- ambigous.ids2.values*100
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
  tb <- tb1[which2keep, ]
  # getTileCode(predFile)
  plotIt(T,title = sprintf("All classes %s", getTileCode(predFile)),
         getTileCode(predFile))
  plotIt(F, title = sprintf("All classes %s", getTileCode(predFile)),
         getTileCode(predFile))

  tb2 <- table(vCLC, vPredsMacro)
  which2keep2 <- which(rowSums(tb2)/sum(tb2) > 0.0001)
  tb <- tb2[which2keep2, ]
  # getTileCode(predFile)
  plotIt(T,title = sprintf("Macro classes %s", getTileCode(predFile)),
         getTileCode(predFile))
  plotIt(F, title = sprintf("Macro classes %s", getTileCode(predFile)),
         getTileCode(predFile))

  stats
}
 ,
mc.cores=8
)







