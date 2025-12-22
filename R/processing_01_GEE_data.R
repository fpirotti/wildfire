library(rgee)
library(rgeeExtra)
# library("inborutils")
# library("parallel")
# library("terra")
library(stars)
library(googledrive)

# 1. Authenticate
drive_auth(email = "cirgeo@unipd.it")


cat(as.character(date()), "\n", file = "processing_01_GEE_data.log" )
# ee_install_upgrade()
version = 3
# ee_Initialize(quiet = T)
ee_Initialize(user = 'cirgeo' )


proj3035_30m = ee$Projection('EPSG:3035')$atScale(30);


## Tree Canopy Density from Copernicus  10 m 2018
tcd = ee$Image("projects/progetto-eu-h2020-cirgeo/assets/copernicus/TCD_2018_010m_eu_03035_V2_4326")$select("b1")
## very high threshold to consider all arid ?
aridityThreshold = 3;

# Time range for NDVI stack
startDate = '2022-01-01';
endDate = '2024-09-30';

# Function to mask clouds and shadows using the SCL band
maskS2clouds <- function(image) {
  scl = image$select('SCL');
  # because we need at least a bit of reflectance, we
  # also mask for red band above 10...
  # e.g. if B4 is 0 then NDVI will always be 1
  red = image$select('B4');
  cloudShadowMask = red$gt(10)$And(scl$neq(3))$And(scl$neq(8))$And(scl$neq(9))$And(scl$neq(10))$And(scl$neq(1));
  return(image$updateMask(cloudShadowMask)$copyProperties(image, list('system:time_start') ))
}

# Function to compute NDVI and add it as a band
addNDVI <-function(image) {
  ndvi = image$normalizedDifference(c('B8', 'B4'))$rename('NDVI');
  return(ndvi);
}
# LAYERS ------
## pilot sites ----
pilotSites = ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/wildfire/wildfire_pilot_sites_v3") ;

# Load and process S2 collection
s2 = ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$filterDate(startDate, endDate)$filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 50))$map(maskS2clouds)$map(addNDVI);

##  NDVI -----
ndviMax = s2$qualityMosaic('NDVI');#$clip(pilotSites);
## Aridity -----
aridityIndex = ee$ImageCollection('projects/progetto-eu-h2020-cirgeo/assets/global/AridityIndex')$mosaic()$divide(10000);
## Canopy height LANG -----
canopy_height = ee$Image('users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1')$unmask()
## Canopy COVER -----
canopy_cover = ee$Image("UMD/hansen/global_forest_change_2023_v1_11")

## Copernicus Global Land Cover 100 m 2019 -----
proba = ee$Image('COPERNICUS/Landcover/100m/Proba-V-C3/Global/2019')
## CLC+ backbone 10 m 2018 ----
clcplus = ee$Image('projects/progetto-eu-h2020-cirgeo/assets/copernicus/CLMS_CLCplus_2021')$select('b1')

figure1_1 = { };
figure1_1_scottBurgan = { };

## CANOPY LOSS MAP ----
onlyNonDisturbedPixels =  canopy_cover$select("lossyear")$unmask()$eq(0);
hansenLossPost2018 =      canopy_cover$select("lossyear")$unmask()$gt(18L);
hansenLossPost2010  =     canopy_cover$select("lossyear")$unmask()$gt(10);
hansenLossPost2010upTo2019 = hansenLossPost2010$And( canopy_cover$select("lossyear")$unmask()$lt(19)  ) ;
hansenLossPost2000   =  canopy_cover$select("lossyear")$unmask()$gt(0)
hansenLossPost2000upTo2009   =  hansenLossPost2000$And( canopy_cover$select("lossyear")$unmask()$lt(11) ) ;

figure1_1$updatedHansen = canopy_cover$select(0)$unmask()$mask(onlyNonDisturbedPixels)


# 91 Urban or suburban development; insufficient wildland fuel ----
figure1_1_scottBurgan$a91 = clcplus$eq(1)$multiply(100)

# 98 water ----
figure1_1_scottBurgan$a98 = clcplus$gt(253)$Or(clcplus$eq(10))$multiply(100)

# 99 barren ----
figure1_1_scottBurgan$a99 = clcplus$gt(253)$Or(clcplus$gt(7)$And(clcplus$lt(10) ))$multiply(100)

# 92 snow ice ----
figure1_1_scottBurgan$a92 = clcplus$eq(11)$multiply(100)


# 93 agriculture ----
# we use eurocrop map from JRC
eucropmap = ee$ImageCollection('JRC/D5/EUCROPMAP/V1')$filterDate(
  '2018-01-01', '2019-01-01')$first();

eucropmapAgric = eucropmap$gt(100)$And(eucropmap$lt(300))

figure1_1_scottBurgan$a93=eucropmapAgric$
  multiply(100)


## GRASS  ----
grassProba=proba$select('discrete_classification')$eq(30)
grassCLCplus=clcplus$eq(6)$Or(tcd$lt(1))

# 101 grasssparse ----
grassSparse= grassCLCplus$And(proba$select('discrete_classification')$eq(60))$multiply(99)

figure1_1_scottBurgan$a101=grassSparse


# GRASS LOW load ----
grassLowLoad=grassCLCplus$Or(grassProba)$And(ndviMax$lt(0.2))$multiply(99)
## 102 dry ----
figure1_1_scottBurgan$a102=grassLowLoad$multiply(aridityIndex$lte(aridityThreshold))
## 105 wet ----
figure1_1_scottBurgan$a105=grassLowLoad$multiply(aridityIndex$gt(aridityThreshold))

# grass MOD load ----
grassModerateLoad=grassCLCplus$Or(grassProba)$And(ndviMax$gte(0.2)$And(ndviMax$lt(0.4)))$multiply(99)
## 104 dry ----
figure1_1_scottBurgan$a104=grassModerateLoad$multiply(aridityIndex$lte(aridityThreshold))
## 106 wet ----
figure1_1_scottBurgan$a106=grassModerateLoad$multiply(aridityIndex$gt(aridityThreshold))

# grass HIGH load ----
grassHighLoad=grassCLCplus$Or(grassProba)$And(ndviMax$gte(0.4))$multiply(99)
## 107 dry ----
figure1_1_scottBurgan$a107=grassHighLoad$multiply(aridityIndex$lte(aridityThreshold))
## 108 wet ----
figure1_1_scottBurgan$a108=grassHighLoad$multiply(aridityIndex$gt(aridityThreshold))

#SHRUB  ----
shrubProba=proba$select('discrete_classification')$eq(20)
shrubCLCplus=clcplus$eq(5)

#SHRUB low load ----
shrubLowLoad=shrubCLCplus$Or(shrubProba)$And(ndviMax$lt(0.2))$multiply(99)
## 141 dry ----
figure1_1_scottBurgan$a141=shrubLowLoad$multiply(aridityIndex$lte(aridityThreshold))
## 146 wet ----
figure1_1_scottBurgan$a146=shrubLowLoad$multiply(aridityIndex$gt(aridityThreshold))


#SHRUB MODERATE load ----
shrubModerateLoad=shrubCLCplus$Or(shrubProba)$And(ndviMax$gte(0.2)$And(ndviMax$lt(0.4)))$multiply(99)
## 142 dry ----
figure1_1_scottBurgan$a142=shrubModerateLoad$multiply(aridityIndex$lte(aridityThreshold))
## 143 wet ----
figure1_1_scottBurgan$a143=shrubModerateLoad$multiply(aridityIndex$gt(aridityThreshold))


#SHRUB high load ----
shrubHighLoad=shrubCLCplus$Or(shrubProba)$And(ndviMax$gte(0.4)$And(ndviMax$lt(0.6)))$multiply(99)
## 145 dry ----
figure1_1_scottBurgan$a145=shrubHighLoad$multiply(aridityIndex$lte(aridityThreshold))
## 148 wet ----
figure1_1_scottBurgan$a148=shrubHighLoad$multiply(aridityIndex$gt(aridityThreshold))

#############
# SHRUB VERY high load ----
shrubVeryHighLoad=shrubCLCplus$And(shrubProba)$And(ndviMax$gte(0.6))$multiply(99)
## 147 dry ----
figure1_1_scottBurgan$a147=shrubVeryHighLoad$multiply(aridityIndex$lte(aridityThreshold))
## 149 wet ----
figure1_1_scottBurgan$a149=shrubVeryHighLoad$multiply(aridityIndex$gt(aridityThreshold))


#############
# TIMBER UNDERSTOREY  ----
proba12x=proba$select('discrete_classification')$divide(10)$floor()$toByte()$eq(12)

# TIMBER UNDERSTOREY LOW+MEDIUM MODERATE load  ----
timberUnderstoreyLowMediumLoad=clcplus$gt(1)$And(clcplus$ lt(6))$And(proba12x)$And(ndviMax$lte(0.6))$multiply(99)
## 161 dry  ----
figure1_1_scottBurgan$a161=timberUnderstoreyLowMediumLoad$multiply(aridityIndex$lte(aridityThreshold))
## 162 wet  ----
figure1_1_scottBurgan$a162=timberUnderstoreyLowMediumLoad$multiply(aridityIndex$gt(aridityThreshold))

#############
#TIMBER UNDERSTOREY high load  ----
timberUnderstoreyHighLoad=clcplus$gt(1)$Or(clcplus$ lt(6))$And(proba12x)$And(ndviMax$gt(0.6))$multiply(99)
## 165 dry ----
figure1_1_scottBurgan$a165=timberUnderstoreyHighLoad$multiply(aridityIndex$lte(aridityThreshold))
## 163 wet ----
figure1_1_scottBurgan$a163=timberUnderstoreyHighLoad$multiply(aridityIndex$gt(aridityThreshold))

#############
#TIMBER LITTER CONIFER  -----
probaClass = proba$select('discrete_classification')
proba11x=probaClass$divide(10)$floor()$toByte()$eq(11)

fuelLoadEstimation = tcd$divide(100)$multiply(ndviMax)$multiply(canopy_height$divide(30))
clcplusTimber = clcplus$gt(1)$And(clcplus$ lt(6))$And(proba12x$Or(proba11x))$And(fuelLoadEstimation$gt(0.1))

clcplusTimberConifer = clcplusTimber$And( probaClass$eq(111)$Or(probaClass$eq(113))$Or(probaClass$eq(115))$Or(probaClass$eq(121))$Or(probaClass$eq(123))$Or(probaClass$eq(125))  )
clcplusTimberBroadleaf = clcplusTimber$And( probaClass$eq(112)$Or(probaClass$eq(114))$Or(probaClass$eq(116))$Or(probaClass$eq(122))$Or(probaClass$eq(124))$Or(probaClass$eq(126))  )

lowLoad = fuelLoadEstimation$gt(0)$And(fuelLoadEstimation$lt(0.33));
medLoad = fuelLoadEstimation$gte(0.33)$And(fuelLoadEstimation$lt(0.66));
highLoad = fuelLoadEstimation$gte(0.66);



##  181 Low Load Compact Conifer Litter -----
  figure1_1_scottBurgan$a181=clcplusTimberConifer$add(lowLoad)$multiply(50)
##  182 Low Load Compact Broadleaves Litter -----
  figure1_1_scottBurgan$a182=clcplusTimberBroadleaf$add(lowLoad)$multiply(50)

##  183 Moderate Load Conifer Litter -----
  figure1_1_scottBurgan$a183=clcplusTimberConifer$add(medLoad)$multiply(50)

##  186 Moderate Load Broadleaf Litter -----
  figure1_1_scottBurgan$a186= clcplusTimberBroadleaf$add(medLoad)$multiply(50)

##  185 High Load Conifer Litter -----
  figure1_1_scottBurgan$a185=clcplusTimberConifer$And(highLoad)$multiply(50)

##  189 very High Load broadleaf Compact Litter -----
  figure1_1_scottBurgan$a189= clcplusTimberBroadleaf$add(highLoad)$multiply(50)



########## SLASH BLOWDOWN USING HANSEN LOSS -------
## areas with 100% canopy cover and trees 25 meters or above
## will have class 204 high load.
## load is lowered depending on density and tree height
sb = clcplusTimber$multiply(tcd)$multiply(canopy_height)$divide(2500)$multiply(4L)$multiply(hansenLossPost2018)$unmask()$toByte()

figure1_1_scottBurgan$a201 = sb$eq(1L)$multiply(100)
figure1_1_scottBurgan$a202 = sb$eq(2L)$multiply(100)
figure1_1_scottBurgan$a203 = sb$eq(3L)$multiply(100)
figure1_1_scottBurgan$a204 = sb$eq(4L)$multiply(100)

for( k in names(figure1_1_scottBurgan) ){
   bv = as.integer(substr(k, 2,6))
   if(is.na(bv)){
     browser()
   }
   nouse =  figure1_1_scottBurgan[[k]]$select(0)$projection()
   newBand = ee$Image$constant( bv )$
                rename('new_band')$
                setDefaultProjection(nouse)$toByte();

   figure1_1_scottBurgan[[k]] = figure1_1_scottBurgan[[k]]$unmask()$toFloat()$addBands(newBand)$rename(c("prob","class") );

}

ScottBurganProbs=ee$ImageCollection( unname(figure1_1_scottBurgan) )
nouse = ScottBurganProbs$first()$projection()

ScottBurgan=ScottBurganProbs$qualityMosaic('prob')$
  setDefaultProjection(nouse)$
  rename(c('scottburgan_cprob', 'scottburgan_class') )




n = pilotSites$size()$getInfo()

task_img_container <- list()
task_img_containerAsset <- list()

for( i in 1:n){

  feature =ee$Feature(pilotSites$toList(12)$get(i-1))
  gg = feature$geometry();
  idf = feature$get("pilot_id")$getInfo() ;


  ScottBurganFiltered = ScottBurgan$select('scottburgan_class')
  id = paste0(idf,'_ScottBurganFuelMapClassV', version)
  # task_img_containerAsset[[id ]] <-"s"
  # next
  ScottBurganProbFiltered = ScottBurgan$select('scottburgan_cprob')
  idp = paste0(idf,'_ScottBurganFuelClassProbV', version)

  if(0==0 && is.null(task_img_container[[id ]])){

    folder <- sprintf("wildfireOutProgettoEU/%s",idf)
    # 2. Get the folder
    folder2rm <- drive_get(folder)

    # 3. List files
    files_in_folder <- drive_ls(path = folder2rm)

    # 4. Remove all files
    drive_rm(files_in_folder)

    task_img_container[[  id ]] <- ee_image_to_drive(
      image= ScottBurganFiltered$toByte()$clip(gg),
      description= id,
      timePrefix = F,
      folder=NULL,
      region= gg ,
      scale= 30,
      crs= 'EPSG:3035',
      maxPixels= 1e13
    )
    task_img_container[[id ]]$start()
#
#     task_img_container[[  idp ]] <- ee_image_to_drive(
#       image= ScottBurganProbFiltered$toByte()$clip(gg),
#       description= idp,
#       folder="wildfireOutProgettoEU",
#       timePrefix = F,
#       region= gg ,
#       scale= 30,
#       crs= 'EPSG:3035',
#       maxPixels= 1e13
#     )
#     task_img_container[[idp ]]$start()

  }


  assetid <- paste0('projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelV2/',  id)
  message(assetid)

  bb <- system( sprintf("earthengine rm %s", assetid), intern = T)
  if(length(bb)) cat(bb, "\n", file = "processing_01_GEE_data.log",append = T) else cat(bb, "SUCCESS\n", file = "processing_01_GEE_data.log",append = T)

  if(is.null(task_img_containerAsset[[ assetid ]])){

    task_img_containerAsset[[id ]] <- ee_image_to_asset(
      image= ScottBurgan$toByte()$clip(gg) ,
      description= paste0(id, 'asset'),
      assetId= assetid,
      region= gg ,
      scale= 30,
      crs= 'EPSG:4326',
      maxPixels= 1e13
    )

   task_img_containerAsset[[id ]]$start()

  }
  # task_img_container[[assetid ]]$status()
  # task_img_container[[assetid ]]$cancel()

}


for( assetid in names(task_img_containerAsset)){
  id <- assetid

  folder <- sprintf("wildfireOutProgettoEU/%s", substr(assetid,1,5))
  message("checking ", assetid)

  cat("checking exports...", assetid, "\n", file = "processing_01_GEE_data.log",append = T)

  task <-  task_img_containerAsset[[assetid]]
  while (task$status()$state %in% c('READY', 'RUNNING')) {
    cat("Task status:", task$status()$state, "\n", file = "processing_01_GEE_data.log",append = T)
    Sys.sleep(10)
  }
  if(task$status()$state=="FAILED"){
    cat("Task status:", task$status()$error_message, "\n", file = "processing_01_GEE_data.log",append = T)
  } else{
    cat("Task status: ", task$status()$state, "\n", file = "processing_01_GEE_data.log",append = T)
  }

  task <-  task_img_container[[assetid]]
  while (task$status()$state %in% c('READY', 'RUNNING')) {
    cat("Task status:", task$status()$state, "\n", file = "processing_01_GEE_data.log",append = T)
    Sys.sleep(10)
  }
  if(task$status()$state=="FAILED"){
    cat("Task status:", task$status()$error_message, "\n", file = "processing_01_GEE_data.log",append = T)
  } else{

    drive_mv(sprintf("%s.tif", id), sprintf("%s/%s.tif", folder, id))


    cat("Task status: ", task$status()$state, "\n", file = "processing_01_GEE_data.log",append = T)
  }

}





bb <- system("earthengine acl set public projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelV2",intern = T)
if(length(bb)) cat(bb, "\n", file = "processing_01_GEE_data.log",append = T) else cat(bb, "SUCCESS\n", file = "processing_01_GEE_data.log",append = T)


classHistogram <- {}
for( assetid in names(task_img_containerAsset)){
  message("histogram ", assetid)

  cat("histogram ", assetid, "\n", file = "processing_01_GEE_data.log",append = T)
  imgname <- paste0('projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelV2/',  assetid)

  fuelModel = ee$Image(imgname)
  nm <- substr(assetid, 1, 5)
  classHistogram[[nm]] = fuelModel$select(1)$reduceRegion(
    reducer= ee$Reducer$frequencyHistogram(),
    geometry= fuelModel$geometry(),
    scale= 30,
    maxPixels= 1e13
  )$getInfo()
}

dt2 <- lapply(classHistogram, function(ll){
  ll <- as.data.frame(ll) / 1e6 * 900 * 100
  ll[ ll< 1 ]  <- NA

  message("tot ha ", sum(ll))

  names(ll) <- sprintf("ha_class%03d",
                       as.integer(gsub('[^(-?(\\d*\\.)?\\d+)]', "",
                                       gsub('\\.','',names(ll) ) ) ) )

  ll
} )

dt3 <- data.table::rbindlist(dt2, idcol = "site", fill = T)
dt4<- reshape2::melt(dt3)

dt3.dt <- as.data.frame(t(dt3[,-1]))
names(dt3.dt)<-dt3[,1][[1]]
dt3.dt$class<-  as.integer(gsub('[^(-?(\\d*\\.)?\\d+)]', "", rownames(dt3.dt) ) )

writexl::write_xlsx(dt3.dt, "fuelmodelPilotSite.xlsx")

featuresHist <- {}
for( i in 1:n){
  feature =ee$Feature(pilotSites$toList(12)$get(i-1))
  gg = feature$geometry();
  idf = feature$get("pilot_id")$getInfo() ;
  message(idf)
  llfeat <- as.list(dt2[[idf]])
  names(llfeat)<- sprintf("class%03d", as.integer(gsub('[^(-?(\\d*\\.)?\\d+)]', "", names(llfeat))) )
  llfeat[["site"]] <- idf
  featuresHist[[idf]] <- ee$Feature(gg,  )
}

amk_fc <-ee$FeatureCollection( unname(featuresHist)  )

## HISTOGRAM -----
assetIdh= sprintf('projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelV%dHFD', version)
task <- ee_table_to_asset(
  collection= amk_fc ,
  description="Histogram",
  assetId= assetIdh
  )

bb <- system( sprintf("earthengine rm %s", assetIdh), intern = T)
if(length(bb)) cat(bb, "\n", file = "processing_01_GEE_data.log",append = T) else cat(bb, "SUCCESS\n", file = "processing_01_GEE_data.log",append = T)

task$start()

cat("Task status HISTOGRAM:", task$status()$state, "\n", file = "processing_01_GEE_data.log",append = T)
while (task$status()$state %in% c('READY', 'RUNNING')) {
  cat("Task status:", task$status()$state, "\n", file = "processing_01_GEE_data.log",append = T)
  Sys.sleep(10)
}
if(task$status()$state=="FAILED"){
  cat("Task status:", task$status()$error_message, "\n", file = "processing_01_GEE_data.log",append = T)
} else{
  cat("Task status: ", task$status()$state, "\n", file = "processing_01_GEE_data.log",append = T)
}
bb <- system( sprintf("earthengine acl set public %s", assetIdh), intern = T)
if(length(bb)) cat(bb, "\n", file = "processing_01_GEE_data.log",append = T) else cat(bb, "SUCCESS\n", file = "processing_01_GEE_data.log",append = T)


