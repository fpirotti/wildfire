library(rgee)
library(rgeeExtra)
library(googledrive)


# ee_Initialize(quiet = T)
ee_Initialize(user = 'cirgeo'  )
assetRoot = 'projects/progetto-eu-h2020-cirgeo/assets/wildfire';
list = ee$data$listAssets(assetRoot);
tb <- as.data.frame(do.call(rbind, list$assets))
sizes <- lapply(list$assets, function(asset) {
  a <- ee$data$getAsset(asset$id)
  ifelse(is.null(a$sizeBytes) || is.na(as.numeric(a$sizeBytes)), NA, round(as.numeric(a$sizeBytes) / (1024*1024) ))
})
tb$size <- unlist(sizes)
