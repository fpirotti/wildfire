if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
pacman::p_load(
  pbmcapply, this.path ,
  terra, hdar,
  sf, data.table,
  parallel, openxlsx,
    reticulate,
  dplyr
)
tmpWd <- getwd()
setwd(this.path::this.dir())
on.exit(parallel:::mccollect(wait = FALSE), add = TRUE)


## S&B fuel model color tables ----
clc_to_SB <- data.frame(
  clc_code = c(
    1,2,3,4,5,6,7,8,9,10,11
  ),

  clc_class = c(
    "Sealed",
    "Woody needle leaved trees",
    "Woody broadleaved deciduous trees",
    "Woody broadleaved evergreen trees",
    "Low-growing woody plants",
    "Permanent herbaceous",
    "Periodically herbaceous",
    "Lichens and mosses",
    "Non- and sparsely vegetated",
    "Water",
    "Snow and ice"
  ),

  SB_model = c(
    "NB1",
    "TL3",
    "TL5",
    "TL6",
    "SH5",
    "GR4",
    "GR3",
    "NB9",
    "NB9",
    "NB8",
    "NB2"
  ),

  SB_number = c(
    91,
    183,
    185,
    186,
    145,
    104,
    103,
    99,
    99,
    98,
    92
  ),

  fuel_type = c(
    "Non-burnable",
    "Conifer litter",
    "Broadleaf litter",
    "Evergreen broadleaf litter",
    "Shrub",
    "Grass",
    "Grass",
    "Non-burnable",
    "Non-burnable",
    "Non-burnable",
    "Non-burnable"
  )
)

fuel_models <- data.frame(
  number = c(
    101:109,
    121:124,
    141:149,
    161:165,
    181:189,
    201:204,
    91,92,93,98,99
  ),

  code = c(
    paste0("GR",1:9),
    paste0("GS",1:4),
    paste0("SH",1:9),
    paste0("TU",1:5),
    paste0("TL",1:9),
    paste0("SB",1:4),
    "NB1","NB2","NB3","NB8","NB9"
  ),

  group = c(
    rep("Grass",9),
    rep("Grass-Shrub",4),
    rep("Shrub",9),
    rep("Timber-Understory",5),
    rep("Timber-Litter",9),
    rep("Slash-Blowdown",4),
    rep("Non-burnable",5)
  ),

  vegetation = c(
    rep("Grassland",9),
    rep("Grass/Shrub",4),
    rep("Shrubland",9),
    rep("Forest",5),
    rep("Forest",9),
    rep("Forest",4),
    "Urban","Snow/Ice","Agriculture","Water","Bare Ground"
  ),

  forest_type = c(
    rep(NA,9),
    rep(NA,4),
    rep(NA,9),
    c("Conifer","Mixed","Mixed","Conifer","Mixed"),
    c("Conifer","Conifer","Mixed","Broadleaf","Broadleaf",
      "Conifer","Mixed","Broadleaf","Mixed"),
    c("Conifer","Conifer","Mixed","Mixed"),
    rep(NA,5)
  ),

  R = c(
    # GR
    189,120, 80, 40, 20,110,170,220,255,
    # GS
    140,120,100, 80,
    # SH
    180,160,140,120,100, 90, 80, 70, 60,
    # TU
    30, 40, 50, 60, 70,
    # TL
    20, 30, 40, 70,100, 20, 60,110, 80,
    # SB
    150,130,110, 90,
    # NB
    250,240,240, 60,180
  ),

  G = c(
    # GR
    255,230,210,180,150,170,180,190,220,
    # GS
    180,170,160,150,
    # SH
    150,135,120,110,100, 90, 80, 70, 60,
    # TU
    100,110,120,130,140,
    # TL
    70, 80, 90,120,150,100,130,170,140,
    # SB
    100, 80, 70, 60,
    # NB
    0,240,220,140,180
  ),

  B = c(
    # GR
    70, 50, 40, 30, 20, 40, 60, 80,120,
    # GS
    60, 50, 40, 30,
    # SH
    40, 35, 30, 25, 20, 15, 10, 10, 10,
    # TU
    30, 35, 40, 45, 50,
    # TL
    20, 20, 25, 35, 45, 30, 40, 55, 45,
    # SB
    50, 40, 30, 20,
    # NB
    0,255,120,255,180
  ),

  stringsAsFactors = FALSE
)

fuel_models$hex <- rgb(
  fuel_models$R,
  fuel_models$G,
  fuel_models$B,
  maxColorValue = 255
)

## CLC+ color tables ----
clc_classes <- factor(
  1:11,
  levels = 1:11,
  labels = sprintf("%s (%d)", c(
    "Sealed",
    "Needle leaved trees",
    "Broadleaved deciduous trees",
    "Broadleaved evergreen trees",
    "Low-growing woody plants",
    "Permanent herbaceous",
    "Periodically herbaceous",
    "Lichens and mosses",
    "Non- and sparsely vegetated",
    "Water",
    "Snow and ice"
  ), 1:11)  )
clcplus_colors <- c(
  "Sealed"                         = "#D73027",
  "Needle leaved trees"            = "#1B7837",
  "Broadleaved deciduous trees"    = "#5AAE61",
  "Broadleaved evergreen trees"    = "#00441B",
  "Low-growing woody plants"       = "#A6D96A",
  "Permanent herbaceous"           = "#D9EF8B",
  "Periodically herbaceous"        = "#FFFFBF",
  "Lichens and mosses"             = "#BDB76B",
  "Non- and sparsely vegetated"    = "#BDBDBD",
  "Water"                          = "#2C7BB6",
  "Snow and ice"                   = "#F7FBFF"
)

#------------------------------------------#
# Confusion matrix
#------------------------------------------#
prettyPrint <- function(perf, agg=F){

  cm<-h2o.confusionMatrix(perf)
  df<-as.data.frame(cm)
  # Get confusion matrix
  cm <- as.matrix(df[1:(nrow(df)-1),1:(ncol(df)-2)])

  if(agg){
    # Original class labels
    cls <- as.integer(rownames(cm))

    # Mapping function
    agg_class <- function(x) {
       x %/% 10
    }

    new_cls <- agg_class(cls)

    # Aggregate rows
    cm_rows <- rowsum(cm, group = new_cls)

    # Aggregate columns
    cm_agg <- t(rowsum(t(cm_rows), group = new_cls))

    # Order classes numerically
    ord <- order(as.numeric(rownames(cm_agg)))
    cm_agg <- cm_agg[ord, ord]

    cm <- cm_agg
  }


  classes <- rownames(cm)

  # Totals
  cm_out <- cbind(cm, Total = rowSums(cm))
  cm_out <- rbind(cm_out, Total = c(colSums(cm), sum(cm)))

  cm_df <- data.frame(
    Reference = rownames(cm_out),
    cm_out,
    check.names = FALSE,
    row.names = NULL
  )

  #------------------------------------------#
  # Per-class metrics
  #------------------------------------------#

  TP <- diag(cm)
  FP <- colSums(cm) - TP
  FN <- rowSums(cm) - TP

  precision <- TP / (TP + FP)
  recall    <- TP / (TP + FN)
  f1         <- 2 * precision * recall / (precision + recall)

  precision[is.na(precision)] <- 0
  recall[is.na(recall)] <- 0
  f1[is.na(f1)] <- 0

  metrics <- data.frame(
    Class = classes,
    Support = rowSums(cm),
    Precision = round(precision,4),
    Recall = round(recall,4),
    F1 = round(f1,4)
  )

  #------------------------------------------#
  # Overall metrics
  #------------------------------------------#

  overall_accuracy <- sum(TP)/sum(cm)

  overall <- data.frame(
    Metric = c(
      "Overall Accuracy",
      "Macro Precision",
      "Macro Recall",
      "Macro F1",
      "Weighted F1"
    ),
    Value = round(c(
      overall_accuracy,
      mean(precision),
      mean(recall),
      mean(f1),
      weighted.mean(f1, rowSums(cm))
    ),4)
  )

  #------------------------------------------#
  # Create workbook
  #------------------------------------------#

  wb <- createWorkbook()

  ## Sheet 1
  addWorksheet(wb,"Confusion Matrix")
  writeData(wb,"Confusion Matrix",cm_df)

  headerStyle <- createStyle(
    textDecoration="bold",
    fgFill="#D9EAD3",
    halign="center",
    border="Bottom"
  )

  addStyle(
    wb,"Confusion Matrix",
    headerStyle,
    rows=1,
    cols=1:ncol(cm_df),
    gridExpand=TRUE
  )

  diagStyle <- createStyle(
    fgFill="#C6EFCE",
    textDecoration="bold"
  )

  for(i in seq_len(nrow(cm))){
    addStyle(
      wb,
      "Confusion Matrix",
      diagStyle,
      rows=i+1,
      cols=i+1,
      stack=TRUE
    )
  }

  setColWidths(wb,"Confusion Matrix",
               cols=1:ncol(cm_df),
               widths="auto")

  freezePane(wb,"Confusion Matrix",
             firstRow=TRUE,
             firstCol=TRUE)

  ## Sheet 2
  addWorksheet(wb,"Class Metrics")
  writeData(wb,"Class Metrics",metrics)
  addStyle(
    wb,"Class Metrics",
    headerStyle,
    rows=1,
    cols=1:ncol(metrics),
    gridExpand=TRUE
  )
  setColWidths(wb,"Class Metrics",
               cols=1:ncol(metrics),
               widths="auto")

  ## Sheet 3
  addWorksheet(wb,"Overall")
  writeData(wb,"Overall",overall)
  addStyle(
    wb,"Overall",
    headerStyle,
    rows=1,
    cols=1:2,
    gridExpand=TRUE
  )
  setColWidths(wb,"Overall",
               cols=1:2,
               widths="auto")

  saveWorkbook(wb,
               "classification_report2.xlsx",
               overwrite=TRUE)
}



