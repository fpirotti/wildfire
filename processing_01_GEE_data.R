library(rgee)
library(rgeeExtra)
library("inborutils")
library("parallel")
library("terra")
library(stars)

version = 2
ee_Initialize(quiet = T)
ee_Initialize(user = 'cirgeo' )

proj3035_30m = ee$Projection('EPSG:3035')$atScale(30);


## Tree Canopy Density from Copernicus  10 m 2018
tcd = ee$Image("projects/progetto-eu-h2020-cirgeo/assets/copernicus/TCD_2018_010m_eu_03035_V2_4326")$select("b1")
## very high threshold to consider all arid
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

# Load and process S2 collection
s2 = ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$filterDate(startDate, endDate)$filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 50))$map(maskS2clouds)$map(addNDVI);

##  NDVI -----
ndviMax = s2$qualityMosaic('NDVI');#$clip(pilotSites);
## aridity -----
aridityIndex = ee$ImageCollection('projects/progetto-eu-h2020-cirgeo/assets/global/AridityIndex')$mosaic();

## canopy height LANG -----
canopy_height = ee$Image('users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1')$unmask()

pilotSites = ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/wildfire/wildfire_pilot_sites_v3") ;
## Copernicus Global Land Cover 100 m 2019 -----
proba = ee$Image('COPERNICUS/Landcover/100m/Proba-V-C3/Global/2019')
## corine land cover plus backbone 10 m 2018 ----
clcplus = ee$Image('projects/progetto-eu-h2020-cirgeo/assets/copernicus/CLMS_CLCplus_2021')$select('b1')

figure1_1 = { };
figure1_1_scottBurgan = { };

##HANSEN COVER -----
canopy_cover = ee$Image("UMD/hansen/global_forest_change_2023_v1_11")
onlyNonDisturbedPixels =  canopy_cover$select("lossyear")$unmask()$eq(0);
hansenLossPost2019 =      canopy_cover$select("lossyear")$unmask()$gt(19);
hansenLossPost2010  =     canopy_cover$select("lossyear")$unmask()$gt(10);
hansenLossPost2010upTo2019 = hansenLossPost2010$And( canopy_cover$select("lossyear")$unmask()$lt(20)  ) ;
hansenLossPost2000   =  canopy_cover$select("lossyear")$unmask()$gt(0)
hansenLossPost2000upTo2009   =  hansenLossPost2000$And( canopy_cover$select("lossyear")$unmask()$lt(11) ) ;

figure1_1$updatedHansen = canopy_cover$select(0)$unmask()$mask(onlyNonDisturbedPixels)


# 91 Urban or suburban development; insufficient wildland fuel ----
figure1_1_scottBurgan$a91 = clcplus$eq(1)$Or(proba$select('discrete_classification')$eq(50))$multiply(100)$reduceResolution(reducer = ee$Reducer$mean())$reproject(proj)$rename("prob")

# 98 water ----
figure1_1_scottBurgan$a98 = clcplus$gt(253)$Or(clcplus$eq(10))$multiply(100)$reduceResolution(reducer = ee$Reducer$mean())$reproject(proj)$rename("prob")

# 99 barren ----
figure1_1_scottBurgan$a99 = clcplus$gt(253)$Or(clcplus$gt(7)$And(clcplus$lt(10) ))$multiply(100)$reduceResolution(reducer = ee$Reducer$mean())$reproject(proj)$rename("prob")

# 92 snow ice ----
figure1_1_scottBurgan$a92 = clcplus$eq(11)$multiply(100)$reduceResolution(reducer = ee$Reducer$mean())$reproject(proj)$rename("prob")

# agriculture in CLC+ is  5 or 7, but also check consensus with 100 m PROBA land cover
# conservative approach as if permanent herbaceaus it is better it goes in GR code of
# scott and burgan

figure1_1_scottBurgan$a93=clcplus$eq(5)$Or(clcplus$ eq(6))$Or(clcplus$ eq(7))$
  And(proba$select('discrete_classification')$eq(40))$
  multiply(100)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")

# 101 grasssparse ----
grassSparse=proba$select('discrete_classification')$eq(60)$And(clcplus$eq(6))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")

figure1_1_scottBurgan$a101=grassSparse

# GRASS  ----
grassProba=proba$select('discrete_classification')$eq(30)
grassCLCplus=clcplus$eq(6)

# GRASS LOW load ----
grassLowLoad=grassCLCplus$And(grassProba)$And(ndviMax$lt(0.2))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 102 dry ----
figure1_1_scottBurgan$a102=grassLowLoad$multiply(aridityIndex$lte(aridityThreshold))$rename("prob")
## 105 wet ----
figure1_1_scottBurgan$a105=grassLowLoad$multiply(aridityIndex$gt(aridityThreshold))$rename("prob")

# grass MOD load ----
grassModerateLoad=grassCLCplus$And(grassProba)$And(ndviMax$gte(0.2)$Or(ndviMax$lt(0.4)))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 104 dry ----
figure1_1_scottBurgan$a104=grassModerateLoad$multiply(aridityIndex$lte(aridityThreshold))$rename("prob")
## 106 wet ----
figure1_1_scottBurgan$a106=grassModerateLoad$multiply(aridityIndex$gt(aridityThreshold))$rename("prob")

# grass HIGH load ----
grassHighLoad=grassCLCplus$And(grassProba)$And(ndviMax$gte(0.4))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 107 dry ----
figure1_1_scottBurgan$a107=grassHighLoad$multiply(aridityIndex$lte(aridityThreshold))$rename("prob")
## 108 wet ----
figure1_1_scottBurgan$a108=grassHighLoad$multiply(aridityIndex$gt(aridityThreshold))$rename("prob")

#SHRUB  ----
shrubProba=proba$select('discrete_classification')$eq(20)
shrubCLCplus=clcplus$eq(5)

#SHRUB low load ----
shrubLowLoad=shrubCLCplus$And(shrubProba)$And(ndviMax$lt(0.2))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 141 dry ----
figure1_1_scottBurgan$a141=shrubLowLoad$multiply(aridityIndex$lte(aridityThreshold))$rename("prob")
## 146 wet ----
figure1_1_scottBurgan$a146=shrubLowLoad$multiply(aridityIndex$gt(aridityThreshold))$rename("prob")


#SHRUB MODERATE load ----
shrubModerateLoad=shrubCLCplus$And(shrubProba)$And(ndviMax$gte(0.2)$Or(ndviMax$lt(0.4)))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 142 dry ----
figure1_1_scottBurgan$a142=shrubModerateLoad$multiply(aridityIndex$lte(aridityThreshold))$rename("prob")
## 143 wet ----
figure1_1_scottBurgan$a143=shrubModerateLoad$multiply(aridityIndex$gt(aridityThreshold))$rename("prob")


#SHRUB high load ----
shrubHighLoad=shrubCLCplus$And(shrubProba)$And(ndviMax$gte(0.4)$Or(ndviMax$lt(0.6)))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 145 dry ----
figure1_1_scottBurgan$a145=shrubHighLoad$multiply(aridityIndex$lte(aridityThreshold))$rename("prob")
## 148 wet ----
figure1_1_scottBurgan$a148=shrubHighLoad$multiply(aridityIndex$gt(aridityThreshold))$rename("prob")

#############
# SHRUB VERY high load ----
shrubVeryHighLoad=shrubCLCplus$And(shrubProba)$And(ndviMax$gte(0.6))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 147 dry ----
figure1_1_scottBurgan$a147=shrubVeryHighLoad$multiply(aridityIndex$lte(aridityThreshold))$rename("prob")
## 149 wet ----
figure1_1_scottBurgan$a149=shrubVeryHighLoad$multiply(aridityIndex$gt(aridityThreshold))$rename("prob")


#############
# TIMBER UNDERSTOREY  ----
proba12x=proba$select('discrete_classification')$divide(10)$floor()$toByte()$eq(12)

# TIMBER UNDERSTOREY LOW+MEDIUM MODERATE load  ----
timberUnderstoreyLowMediumLoad=clcplus$gt(1)$Or(clcplus$ lt(7))$And(proba12x)$And(ndviMax$lte(0.6))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 161 dry  ----
figure1_1_scottBurgan$a161=timberUnderstoreyLowMediumLoad$multiply(aridityIndex$lte(aridityThreshold))$rename("prob")
## 162 wet  ----
figure1_1_scottBurgan$a162=timberUnderstoreyLowMediumLoad$multiply(aridityIndex$gt(aridityThreshold))$rename("prob")

#############
#TIMBER UNDERSTOREY high load  ----
timberUnderstoreyHighLoad=clcplus$gt(1)$Or(clcplus$ lt(7))$And(proba12x)$And(ndviMax$gt(0.6))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 165 dry ----
figure1_1_scottBurgan$a165=timberUnderstoreyHighLoad$multiply(aridityIndex$lte(aridityThreshold))$rename("prob")
## 163 wet ----
figure1_1_scottBurgan$a163=timberUnderstoreyHighLoad$multiply(aridityIndex$gt(aridityThreshold))$rename("prob")

#############
#TIMBER LITTER CONIFER  -----
proba11x=proba$select('discrete_classification')$divide(10)$floor()$toByte()$eq(11)

clcplusTimber = clcplus$gt(1)$Or(clcplus$ lt(7))$And(proba12x$Or(proba11x))
## 183 Low-mediumLoadCompactLitter -----
  #weassumethatifcanopycoversallpixelandcanopyheightis20orabove,
  #wehaveamatureforestwithlowloadoflitter
figure1_1_scottBurgan$a183=clcplus$eq(2)$And(proba11x)$And(tcd$gt(80))$And(canopy_height$gt(20))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")

## 185 HighLoadCompactLitter -----
#weassumethatifcanopycoversallpixelandcanopyheightis20orabove,
#wehaveamatureforestwithlowloadoflitter
figure1_1_scottBurgan$a185=clcplus$eq(2)$And(proba11x)$And(tcd$lte(80)$Or(canopy_height$lte(20)))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")


#TIMBER LITTER BROADLEAVES -----
## 184 Low-mediumLoadCompactLitter -----
#weassumethatifcanopycoversallpixelandcanopyheightis20orabove,
#wehaveamatureforestwithlowloadoflitter
figure1_1_scottBurgan$a184=clcplus$eq(3)$Or(clcplus$ eq(4))$And(proba11x)$And(tcd$gt(80))$And(canopy_height$gt(20))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")

## 186 High Load Compact Litter -----
#weassumethatifcanopycoversallpixelandcanopyheightis20orabove,
#wehaveamatureforestwithlowloadoflitter
figure1_1_scottBurgan$a186=clcplus$eq(3)$Or(clcplus$eq(4))$And(proba11x)$And(tcd$lte(80)$Or(canopy_height$lte(20)))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")


########## SLASH BLOWDOWN USING HANSEN LOSS -------
## areas with 100% canopy cover and trees 25 meters or above
## will have class 204 high load.
## load is lowered depending on density and tree height
sb = clcplusTimber$multiply(tcd)$multiply(canopy_height)$divide(2500)$multiply(4L)$multiply(hansenLossPost2019)

figure1_1_scottBurgan$a201 = sb$eq(1L)$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")
figure1_1_scottBurgan$a202 = sb$eq(2L)$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")
figure1_1_scottBurgan$a203 = sb$eq(3L)$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")
figure1_1_scottBurgan$a204 = sb$eq(4L)$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")

for( imgn in names(figure1_1_scottBurgan) ){
  figure1_1_scottBurgan[[imgn]] = figure1_1_scottBurgan[[imgn]]$select("prob")$addBands(ee$Image(as.numeric(gsub("a", "", imgn) ) ))$toByte()$reproject(proj)
}

ScottBurganProbs=ee$ImageCollection( unname(figure1_1_scottBurgan) )

ScottBurgan=ScottBurganProbs$qualityMosaic('prob')$rename(c('scottburgan_cprob',
                                                            'scottburgan_class') )





ss=ScottBurgan$getInfo()
n = pilotSites$size()$getInfo()

task_img_container <- list()
task_img_containerAsset <- list()
for( i in 1:n){

  feature =ee$Feature(pilotSites$toList(12)$get(i-1))
  gg = feature$geometry();
  idf = feature$get("pilot_id")$getInfo() ;

  ScottBurganFiltered = ScottBurgan$select('scottburgan_class')
  id = paste0(idf,'_ScottBurganFuelMapClassV', version)
  ScottBurganProbFiltered = ScottBurgan$select('scottburgan_cprob')
  idp = paste0(idf,'_ScottBurganFuelClassProbV', version)

  if(0==0 && is.null(task_img_container[[id ]])){

    task_img_container[[  id ]] <- ee_image_to_drive(
      image= ScottBurganFiltered$toByte()$clip(gg),
      description= id,
      timePrefix = F,
      folder= "wildfireOut",
      region= gg ,
      scale= 30,
      crs= 'EPSG:3035',
      maxPixels= 1e13
    )
    task_img_container[[id ]]$start()

    task_img_container[[  idp ]] <- ee_image_to_drive(
      image= ScottBurganProbFiltered$toByte()$clip(gg),
      description= idp,
      folder= "wildfireOut",
      timePrefix = F,
      region= gg ,
      scale= 30,
      crs= 'EPSG:3035',
      maxPixels= 1e13
    )
    task_img_container[[idp ]]$start()

  }
  task_img_container[[id ]]$status()
#   # task_img_container[[id ]]$cancel()

  assetid <- paste0('projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelV2/',  id)
  message(assetid)

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
  message("checking ", assetid)
  task <-  task_img_containerAsset[[assetid]]
  while (task$status()$state %in% c('READY', 'RUNNING')) {
    cat("Task status:", task$status()$state, "\n", file = "procssing_01_GEE_data.log",append = T)
    Sys.sleep(10)
  }

bb <- system(sprintf("earthengine acl set public %s", assetid),intern = T)
cat(bb, "\n", file = "procssing_01_GEE_data.log",append = T)

}

classHistogram = fuelModel$first()$select(1)$reduceRegion(
  reducer= ee.Reducer$frequencyHistogram(),
  geometry= fuelModel$first()$geometry(),
  scale= 30,
  maxPixels= 1e13
)
