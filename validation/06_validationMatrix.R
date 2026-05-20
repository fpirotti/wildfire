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

  # Create the summary table na.omit
  class_metrics <- ( data.frame(
    Class = names(recall),
    "NWildfire" = colSums(conf_mat),
    "NReference" = rowSums(conf_mat),
    "Recall" = round(recall, 3),
    "Precision" = round(precision, 3),
    F1_Score = round(f1_score, 3)
  ) )

  k[["fullC"]] <- class_metrics %>%
    kable(
      digits = 3, align = "r",
      caption = paste0("Classification Performance Metrics per Class (", names(ref)[[1]] ,")"),
      col.names = c("Class", "N Wildfire", "N Reference",
                    "Recall (Prod. Acc.)",
                    "Precision (User Acc.)", "F1-Score"),
      booktabs = TRUE
    ) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

  vals$Reference2 <-  trunc(as.integer(as.character(vals$Reference))/10)
  vals$WildFire2 <- trunc(as.integer(as.character(vals$WildFire))/10)
  lvs <- sort(unique(c(vals$WildFire2, vals$Reference2)))

  vals$WildFire2 <- factor(vals$WildFire2, levels = lvs)
  vals$Reference2 <-  factor(vals$Reference2, levels = lvs)

  ## discrepancies -----
  # ids <- rownames(vals)[ vals$WildFire2==vals$Reference2 ]
  # errors <- s
  # errors[ as.integer(ids) ]<- NA
  # plot(errors)
  # discrepancies <- na.omit(as.points(errors))
  # writeVector(discrepancies, sprintf("%s_discrepancies.gpkg", area), overwrite=TRUE)

  ########################
  conf_mat2 <- table(Reference = vals$Reference2, Classified=vals$WildFire2)
  # Calculate metrics
  recall <- diag(conf_mat2) / rowSums(conf_mat2)    # Equivalent to Producer's Accuracy
  precision <- diag(conf_mat2) / colSums(conf_mat2) # Equivalent to User's Accuracy
  f1_score <- 2 * (precision * recall) / (precision + recall)

  # Create the summary table
  # Create the summary table na.omit
  class_metrics_aggr <- ( data.frame(
    Class = names(recall),
    "NWildfire" = colSums(conf_mat2),
    "NReference" = rowSums(conf_mat2),
    "Recall" = round(recall, 3),
    "Precision" = round(precision, 3),
    F1_Score = round(f1_score, 3)
  ) )

  if(exists("risultati") && is.list(risultati)) risultati[[tit]] <<- list(All=class_metrics,
                                                  Aggr=class_metrics_aggr)

  k[["aggrC"]] <- class_metrics_aggr %>%
    kable(
      digits = 3, align = "r",
      caption = paste("Classification Performance Metrics per Class (Ref=", names(ref)[[1]] ,")"),
      col.names = c("Class","N Wildfire", "N Reference", "Recall (Prod. Acc)", "Precision (User Acc)", "F1-Score"),
      booktabs = TRUE
    ) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

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
