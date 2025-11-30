library(kableExtra)

errorTables <- function(ref,cls, verbose=F){
  k <- list()
  # browser()
  if(terra::is.points(cls)){

    vals <- data.frame(cls[[1]][,1],
                       terra::extract(ref, cls,  ID=F)[,1] )
  } else {

    s <- c(ref, cls)
    # Extract values as a data.frame
    vals <-  as.data.frame(s, xy=FALSE)
  }

  vals[vals == 0] <- NA
  vals <- na.omit(vals)
  colnames(vals) <- c("CzechGlobe", "WildFire")

  lvs <- sort(unique(c(vals$WildFire, vals$CzechGlobe)))

  vals$WildFire <- factor(vals$WildFire, levels = lvs)
  vals$CzechGlobe <-  factor(vals$CzechGlobe, levels = lvs)

  conf_mat <- table(vals$CzechGlobe, vals$WildFire)
  # Overall accuracy
  overall_acc <- sum(diag(conf_mat)) / sum(conf_mat)
  # Producer's accuracy (per class)
  prod_acc <- diag(conf_mat) / rowSums(conf_mat)
  # User's accuracy (per class)
  user_acc <- diag(conf_mat) / colSums(conf_mat)
  # Kappa
  pe <- sum(rowSums(conf_mat) * colSums(conf_mat)) / (sum(conf_mat)^2)
  kappa <- (overall_acc - pe) / (1 - pe)
  if(verbose) message("Kappa full=", round(kappa,3))
  cm_ext <-  as.matrix(conf_mat)
  cm_ext <- cbind(cm_ext, "PA" = sprintf("%.1f%%", prod_acc*100))
  # Add User’s Accuracy as extra row
  cm_ext <- rbind(cm_ext, "UA" = c(sprintf("%.1f%%", user_acc*100),
                                         sprintf("%.1f%%", overall_acc*100)))


  k[["full"]] <- kable(cm_ext, booktabs = TRUE, caption = paste0("--", area, " - Confusion Matrix ALL rows=CzechGlobe (reference)   colums=Wildfire-V2 (Classified)   kappa=", round(kappa,3)) ) %>%
    kable_styling(full_width = FALSE, latex_options = c("hold_position", "striped", "scale_down")) %>%
    column_spec(1, bold = TRUE)


  vals$CzechGlobe2 <-  trunc(as.integer(as.character(vals$CzechGlobe))/10)
  vals$WildFire2 <- trunc(as.integer(as.character(vals$WildFire))/10)
  lvs <- sort(unique(c(vals$WildFire2, vals$CzechGlobe2)))

  vals$WildFire2 <- factor(vals$WildFire2, levels = lvs)
  vals$CzechGlobe2 <-  factor(vals$CzechGlobe2, levels = lvs)

  # ids <- rownames(vals)[ vals$WildFire2==vals$CzechGlobe2 ]
  # errors <- s
  # errors[ as.integer(ids) ]<- NA
  # plot(errors)
  # discrepancies <- na.omit(as.points(errors))
  # writeVector(discrepancies, sprintf("%s_discrepancies.gpkg", area), overwrite=TRUE)
  conf_mat2 <- table(Reference = vals$CzechGlobe2, Classified=vals$WildFire2)
  # Overall accuracy
  overall_acc2 <- sum(diag(conf_mat2)) / sum(conf_mat2)
  # Producer's accuracy (per class)
  prod_acc2 <- diag(conf_mat2) / rowSums(conf_mat2)
  # User's accuracy (per class)
  user_acc2 <- diag(conf_mat2) / colSums(conf_mat2)
  # Kappa
  pe2 <- sum(rowSums(conf_mat2) * colSums(conf_mat2)) / (sum(conf_mat2)^2)
  kappa2 <- (overall_acc2 - pe2) / (1 - pe2)
  if(verbose) message("Kappa aggr=", round(kappa2,3))

  cm_ext <-  as.matrix(conf_mat2)
  cm_ext <- cbind(cm_ext, "PA" = sprintf("%.1f%%", prod_acc2*100))
  # Add User’s Accuracy as extra row
  cm_ext <- rbind(cm_ext, "UA" = c(sprintf("%.1f%%", user_acc2*100),
                                         sprintf("%.1f%%", overall_acc2*100)))


  k[["aggr"]] <- kable(cm_ext, align = "r", caption = paste0("--", area, " - Confusion Matrix rows=CzechGlobe (reference)   colums=Wildfire-V2 (Classified)   kappa=", round(kappa2,3)) ) %>%
    kable_styling(full_width = FALSE, latex_options = c("hold_position", "striped", "scale_down")) %>%
    column_spec(1, bold = TRUE)

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
