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

#-------------------------------------------------------#
######################## APPLY MODEL  ##################
#-------------------------------------------------------#

CLCconsensusLUT <- c("1"=91,
                     "2" = c(16, 18, 20),
                     "3"=c(16,18,20),
                     "4"=c(16,18,20),
                     "5"=3,
                     "6"=4,
                     "7"=4,
                     "9"=4,
                     "10"=98)

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
  if(grepl("E46N30", predFile)) break
  next

  message(getTileCode(predFile))
  clcFile <- grep(getTileCode(predFile), clcFiles, value=T)
  clcFileConf <- grep(getTileCode(predFile), clcFilesConf, value=T)
  predFileConf <- grep(getTileCode(predFile), predFilesConf, value=T)
  r <- terra::rast(predFile)
  rConf <- terra::rast(predFileConf)
  rm <- terra::mask(r, studyArea)
  cells.ids <- terra::cells(rm)

  vPreds <- rm[cells.ids][,1]
  vPredsConf <- rConf[cells.ids][,1]

  rCLC <- terra::rast(clcFile)
  vCLC <-  rCLC[cells.ids][,1]
  vPredsF <- vPreds > 100
  vPredsMacro <- vPreds
  vPredsMacro[vPredsF] <- trunc(vPreds[vPredsF]/10)

  rCLCconf <- terra::rast(clcFileConf)
  vCLCconf <- rCLCconf[cells.ids][,1]
  vCLC.water <- vCLC==10
  vPredsMacro[vCLC.water] <- 98
  finalFuel <- terra::rast(rm, vals=98)
  finalFuelConf <- terra::rast(rm, vals=0)

  tb <- table(vCLC, vPreds)
  which2keep <- which(rowSums(tb)/sum(tb) > 0.0001)
  tb <- tb[which2keep, ]
  # getTileCode(predFile)
  plotIt(T,title = sprintf("All classes %s", getTileCode(predFile)),
         getTileCode(predFile))
  plotIt(F, title = sprintf("All classes %s", getTileCode(predFile)),
         getTileCode(predFile))

  tb <- table(vCLC, vPredsMacro)
  which2keep <- which(rowSums(tb)/sum(tb) > 0.0001)
  tb <- tb[which2keep, ]
  # getTileCode(predFile)
  plotIt(T,title = sprintf("Macro classes %s", getTileCode(predFile)),
         getTileCode(predFile))
  plotIt(F, title = sprintf("Macro classes %s", getTileCode(predFile)),
         getTileCode(predFile))
}


plotIt <- function(rown=T, title=NA, tile=""){
  library(ggplot2)
  library(RColorBrewer)
  library(ggtext)

  if(rown) {
    tb_pct <- sweep(tb, 1, rowSums(tb), "/") * 100
    titadd <- "rowWise"
  } else {
    tb_pct <- sweep(tb, 1, colSums(tb), "/") * 100
    titadd <- "colWise"
  }
  df <- as.data.frame(as.table(tb_pct))
  names(df) <- c("vCLC", "vPredsMacro", "pct")
  y_labels <- setNames(
    paste0(
      "<span style='color:", clcplus_colors[as.integer(rownames(tb_pct))],
      ";'>",
      as.character(rownames(tb_pct)),
      "</span>"
    ),
    as.character(rownames(tb_pct))
  )
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
    ggsave(
      file.path("plots",
                sprintf("%s_%s.png", gsub(" ", "_", title),
                                 titadd) ), p)
  } else{
    print(p)
  }
}


