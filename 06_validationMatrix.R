library(kableExtra)

errorTables <- function(ref,cls){
  k <- list()
  s <- c(ref, cls)
  # Extract values as a data.frame
  vals <- na.omit(as.data.frame(s, xy=FALSE))
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
