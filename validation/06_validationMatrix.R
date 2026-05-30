library(kableExtra)

errorTables <- function(ref,pred, tit, verbose=F){
  k <- list()



  target_ext <- terra::ext(ref)
  src_ext <- terra::project(target_ext, from = ref, to = pred)
  pred_cropped <- terra::crop(pred, src_ext)
  cls<- terra::project(as.points(pred_cropped), ref)
  clsf <-  cls |>  terra::intersect(terra::ext(ref) )

  vals <- data.frame(WildFire = clsf[[1]][,1],
                       Reference = terra::extract(ref, clsf,  ID=F)[,1] )


  vals[vals == 0] <- NA
  vals <- na.omit(vals)

  lvs <- sort(unique(c(vals$WildFire, vals$Reference)))

  vals$WildFire <- factor(vals$WildFire, levels = lvs)
  vals$Reference <-  factor(vals$Reference, levels = lvs)

  conf_mat <- table(vals$Reference, vals$WildFire)
  recall <- diag(conf_mat) / rowSums(conf_mat)    # Equivalent to Producer's Accuracy
  precision <- diag(conf_mat) / colSums(conf_mat) # Equivalent to User's Accuracy
  f1_score <- 2 * (precision * recall) / (precision + recall)

  # 1. Convert your 26x26 table to a standard matrix-dataframe
  df_mat <- as.data.frame.matrix(conf_mat)

  # 2. Automatically loop down the diagonal and inject HTML styling
  for (i in 1:nrow(df_mat)) {
    df_mat[i, i] <- cell_spec(
      df_mat[i, i],
      bold = TRUE,
      background = "#FFFFCC", # Soft yellow highlight
      color = "#000000"       # Keep text black for readability
    )
  }

  k[["fullCm"]] <-  df_mat %>%
    kable(
      format = "html", escape=F,
      caption = "Confusion Matrix: Predicted (columns) vs. Reference (rows)",
      align = "c"
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "bordered"),
      full_width = FALSE,
      position = "center"
    ) %>%
    # Add a top header layer for the Predicted class (WildFire)
    # add_header_above(c("Reference (Actual)" = 1, "Predicted (WildFire)" = 2)) %>%
    # Bold the row names (Reference classes) for clear separation
    column_spec(1, bold = TRUE, background = "#f8f9fa")

######### weighted confusion metrics

  # 1. Convert table to standard numeric matrix
  cm <- as.matrix(conf_mat)
  row_sums <- rowSums(cm)
  col_sums <- colSums(cm)
  mapped_counts <- terra::freq(ref)
  W <- mapped_counts$count / sum(mapped_counts$count)
  names(W)<-as.character(mapped_counts$value)
  # 2. Build the Olofsson Area-Proportion Matrix (p_ij)
  # Formula: p_ij = W_i * (n_ij / n_i.)
  p_mat <- matrix(0, nrow = nrow(cm), ncol = ncol(cm))
  for(j in 1:ncol(cm)) {
    if(col_sums[j] > 0) {
      # Get the column name (Classified class name)
      cn <- colnames(cm)[[j]]

      # Pull the specific weight for this map class
      col_weight <- W[cn]

      # Safety check for naming mismatches
      if(is.na(col_weight)) {
        next
        # stop(paste("Class name", cn, "not found in your weight vector W! Check names(W) vs colnames(cm)"))
      }
      p_mat[, j] <- col_weight * (cm[, j] / col_sums[j])
    }
  }
  rownames(p_mat) <- rownames(cm)
  colnames(p_mat) <- colnames(cm)

  weighted_precision <- diag(p_mat) / colSums(p_mat)

  # Recall (Producer's Accuracy) = p_ii / p_i. (row sums of p)
  weighted_recall <- diag(p_mat) / rowSums(p_mat)
  # F1 Score = 2 * (P * R) / (P + R)
  weighted_f1 <- 2 * (weighted_precision * weighted_recall) / (weighted_precision + weighted_recall)
  # Handle potential NaN if precision + recall == 0
  weighted_f1[is.na(weighted_f1)] <- 0
  class_metrics <- data.frame(
    Class = names(recall),
    NWildfire = colSums(conf_mat),
    NReference = rowSums(conf_mat),
    Recall = round(recall, 3),
    Precision = round(precision, 3),
    F1_Score = round(f1_score, 3),

    # New Area-Weighted columns
    Weighted_Recall = round(weighted_recall, 3),
    Weighted_Precision = round(weighted_precision, 3),
    Weighted_F1 = round(weighted_f1, 3)
  )

#########

  k[["fullC"]] <- class_metrics %>%
    kable(
      digits = 3, align = "r",row.names = F,
      caption = paste0("Classification Performance Metrics per Class (", names(ref)[[1]] ,")"),
      col.names = c("Class", "N Wildfire", "N Reference",
                    "Recall (PA)",
                    "Precision (UA)", "F1-Score",
                    "W.Recall (PA)",
                    "W.Precision (UA)", "W.F1-Score"),
      booktabs = TRUE
    ) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


  vals$Reference2 <-  trunc(as.integer(as.character(vals$Reference))/10)
  vals$WildFire2 <- trunc(as.integer(as.character(vals$WildFire))/10)
  lvs <- sort(unique(c(vals$WildFire2, vals$Reference2)))

  vals$WildFire2 <- factor(vals$WildFire2, levels = lvs)
  vals$Reference2 <-  factor(vals$Reference2, levels = lvs)

  ########################
  conf_mat2 <- table(Reference = vals$Reference2, Classified=vals$WildFire2)

  df_mat2 <- as.data.frame.matrix(conf_mat2)

  # 2. Automatically loop down the diagonal and inject HTML styling
  for (i in 1:nrow(df_mat2)) {
    df_mat2[i, i] <- cell_spec(
      df_mat2[i, i],
      bold = TRUE,
      background = "#FFFFCC", # Soft yellow highlight
      color = "#000000"       # Keep text black for readability
    )
  }

  k[["aggrCm"]] <-  df_mat2 %>%
    kable(
      format = "html", escape=F,
      caption = "Confusion Matrix: Predicted (columns) vs. Reference (rows)",
      align = "c"
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "bordered"),
      full_width = FALSE,
      position = "center"
    ) %>%
    column_spec(1, bold = TRUE, background = "#f8f9fa")


  # Calculate metrics
  recall <- diag(conf_mat2) / rowSums(conf_mat2)    # Equivalent to Producer's Accuracy
  precision <- diag(conf_mat2) / colSums(conf_mat2) # Equivalent to User's Accuracy
  f1_score <- 2 * (precision * recall) / (precision + recall)

  cm <- as.matrix(conf_mat2)
  row_sums <- rowSums(cm)
  col_sums <- colSums(cm)

  mapped_counts$macroClass <- trunc(as.integer(as.character(mapped_counts$value))/10)
  mapped_countsMC <- mapped_counts |> group_by(macroClass) |> summarise(count=sum(count))

  W <- mapped_countsMC$count / sum(mapped_countsMC$count)

  names(W)<-as.character(mapped_countsMC$macroClass)
  # 2. Build the Olofsson Area-Proportion Matrix (p_ij)
  # Formula: p_ij = W_i * (n_ij / n_i.)
  p_mat <- matrix(0, nrow = nrow(cm), ncol = ncol(cm))
  for(j in 1:ncol(cm)) {
    if(col_sums[j] > 0) {
      # Get the column name (Classified class name)
      cn <- colnames(cm)[[j]]

      # Pull the specific weight for this map class
      col_weight <- W[cn]

      # Safety check for naming mismatches
      if(is.na(col_weight)) {
        next
        # stop(paste("Class name", cn, "not found in your weight vector W! Check names(W) vs colnames(cm)"))
      }
      p_mat[, j] <- col_weight * (cm[, j] / col_sums[j])
    }
  }
  rownames(p_mat) <- rownames(cm)
  colnames(p_mat) <- colnames(cm)
  # browser()
  weighted_precision <- diag(p_mat) / colSums(p_mat)

  # Recall (Producer's Accuracy) = p_ii / p_i. (row sums of p)
  weighted_recall <- diag(p_mat) / rowSums(p_mat)
  # F1 Score = 2 * (P * R) / (P + R)
  weighted_f1 <- 2 * (weighted_precision * weighted_recall) / (weighted_precision + weighted_recall)
  # Handle potential NaN if precision + recall == 0
  weighted_f1[is.na(weighted_f1)] <- 0
  class_metrics_aggr <- data.frame(
    Class = names(recall),
    NWildfire = colSums(conf_mat2),
    NReference = rowSums(conf_mat2),
    Recall = round(recall, 3),
    Precision = round(precision, 3),
    F1_Score = round(f1_score, 3),
    # New Area-Weighted columns
    Weighted_Recall = round(weighted_recall, 3),
    Weighted_Precision = round(weighted_precision, 3),
    Weighted_F1 = round(weighted_f1, 3)
  )



  if(exists("risultati") && is.list(risultati)) risultati[[tit]] <<- list(All=class_metrics,
                                                  Aggr=class_metrics_aggr)

  k[["aggrC"]] <- class_metrics_aggr %>%
    kable(
      digits = 3, align = "r",row.names = F,
      caption = paste("Classification Performance Metrics per Class (Ref=", names(ref)[[1]] ,")"),
      col.names = c("Class","N Wildfire",
                    "N Reference",
                    "Recall (PA)",
                    "Precision (UA)", "F1-Score",
                    "W.Recall (PA)",
                    "W.Precision (UA)", "W.F1-Score"),
      booktabs = TRUE
    ) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


  # k[["aggrConf"]] <- class_metrics_aggr %>%
  #   kable(
  #     digits = 3, align = "r",
  #     caption = paste("Classification Performance Metrics per Class (Ref=", names(ref)[[1]] ,")"),
  #     col.names = c("Class","N Wildfire", "N Reference", "Recall (Prod. Acc)", "Precision (User Acc)", "F1-Score"),
  #     booktabs = TRUE
  #   ) |>
  #   kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

  k
}


## test resampling effect------
#
# library(terra)
#
# # 30 m in degrees at Dresden latitude
# deg30 <- 30 / 111230
#
# # 10 km extent in degrees
# extent_deg <- 1000 / 111230
#
# # Dresden center
# cx <- 13.74
# cy <- 51.05
#
# # Build extent
# xmin <- cx - extent_deg/2
# xmax <- cx + extent_deg/2
# ymin <- cy - extent_deg/2
# ymax <- cy + extent_deg/2
#
# # grid size
# ncol <- round((xmax - xmin) / deg30)
# nrow <- ncol
#
# # Raster in lat/long
# r <- rast(ncols=ncol, nrows=nrow,
#           xmin=xmin, xmax=xmax,
#           ymin=ymin, ymax=ymax,
#           crs="EPSG:4326")
#
#
# # assign random classes 1, 2, 3
# set.seed(123)   # reproducible
# values(r) <- sample(1:3, ncell(r), replace = TRUE)
#
# # set categories
# r <- as.factor(r)
# levels(r) <- data.frame(value = 1:3,
#                             class = c("A","B","C"))
#
# # plot
# # plot(r)
# crs32633<-terra::crs("epsg:32632")
# crs3035<-terra::crs("epsg:3035")
# pred <- terra::project(r, crs3035, method="near")
# ref <- terra::project(r, crs32633, method="near")
#
# predv <- terra::project(as.points(r), crs3035 )
# refv <- terra::project(as.points(r), crs32633 )
#
# writeVector(as.points(r), "r4326.gpkg")
# writeVector(predv, "pred3035.gpkg")
# writeVector(refv, "ref32633.gpkg")
# clsv<- terra::project(predv, refv )
#
# writeVector(clsv, "ref3035.gpkg")
#
# writeRaster(r, "r4326.tif")
# writeRaster(pred, "pred3035.tif")
# writeRaster(ref, "ref32633.tif")
# # cls<-terra::resample( terra::project(pred, ref, method="near"), ref, method="near" )
# cls<- terra::project(pred, ref, method="near")
#
# writeRaster(cls, "ref3035.tif")

# cls<- terra::project(predv, ref )
# # cls[cls==0] <- NA
# # ref[ref==0] <- NA
# k <- errorTables(ref, cls, T)
# for(t in k){
#   print(t)
#   cat("\\newpage\n")
# }
