library(rgee)
library(rgeeExtra)
library("inborutils")
library("parallel")
library("terra")

ee_Initialize(quiet = T)
ee_Initialize(user = 'cirgeo' )




proj3035_30m = ee$Projection('EPSG:3035')$atScale(30);

dir.exists(outputdir) dir.create(outputdir)

library(stars)
# Main fuel types ESA World  Cover:
  # tree, shrub, grass, crop,  wet/peat,
# urban, nonfuel,
dsm = ee$ImageCollection('JAXA/ALOS/AW3D30/V3_2');
tcd = ee$Image("projects/progetto-eu-h2020-cirgeo/assets/copernicus/TCD_2018_010m_eu_03035_V2_4326")


#Map.addLayer(tileEEA, {}, 'tiles EEA' )
aridityThreshold = 3;

# Time range
startDate = '2022-01-01';
endDate = '2024-09-30';





# Function to mask clouds and shadows using the SCL band
maskS2clouds <- function(image) {
  scl = image$select('SCL');
  cloudShadowMask = scl$neq(3)$And(scl$neq(8))$And(scl$neq(9))$And(scl$neq(10))$And(scl$neq(1));

  return(image$updateMask(cloudShadowMask)$copyProperties(image, list('system:time_start') ))
}

# Function to compute NDVI and add it as a band
addNDVI <-function(image) {
  ndvi = image$normalizedDifference(c('B8', 'B4'))$rename('NDVI');
  return(ndvi);
}

# Load and process collection
s2 = ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$filterDate(startDate, endDate)$filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 50))$map(maskS2clouds)$map(addNDVI);

# Create composite using pixel with highest NDVI
ndviMax = s2$qualityMosaic('NDVI');#$clip(pilotSites);
# biomass in t/ac
biomassGrass = ndviMax$multiply(1270)$subtract(200)$multiply(0.008924)$mask(ndviMax$gt(0.1)$And(ndviMax$lt(0.8) ) )

aridityIndex = ee$ImageCollection('projects/progetto-eu-h2020-cirgeo/assets/global/AridityIndex')$mosaic();

canopy_height = ee$Image('users/nlang/ETH_GlobalCanopyHeight_2020_10m_v1')
pilotSites = ee$FeatureCollection("projects/progetto-eu-h2020-cirgeo/assets/wildfire/wildfire_pilot_sites_v3") ;
corine = ee$Image("COPERNICUS/CORINE/V20/100m/2018");
proba = ee$Image('COPERNICUS/Landcover/100m/Proba-V-C3/Global/2019')
esaWorldCover10m = ee$ImageCollection('ESA/WorldCover/v200')$first();


proj3035_30m = ee$Projection('EPSG:3035')$atScale(30);
proj = ee$Projection('EPSG:3035')$atScale(30);

#corine.filterDate('2017-12-01', '2018-12-01').first()
clcplus = ee$Image('projects/progetto-eu-h2020-cirgeo/assets/copernicus/CLMS_CLCplus_2021')$select('b1')

figure1_1 = { };
figure1_1_scottBurgan = { };

canopy_cover = ee$Image("UMD/hansen/global_forest_change_2023_v1_11")
onlyNonDisturbedPixels =  canopy_cover$select("lossyear")$unmask()$eq(0);
hansenLossPost2019 =      canopy_cover$select("lossyear")$unmask()$gt(19);
hansenLossPost2010  =     canopy_cover$select("lossyear")$unmask()$gt(10);
hansenLossPost2010upTo2019 = hansenLossPost2010$And( canopy_cover$select("lossyear")$unmask()$lt(20)  ) ;
hansenLossPost2000   =  canopy_cover$select("lossyear")$unmask()$gt(0)
hansenLossPost2000upTo2009   =  hansenLossPost2000$And( canopy_cover$select("lossyear")$unmask()$lt(11) ) ;

figure1_1$updatedHansen = canopy_cover$select(0)$unmask()$mask(onlyNonDisturbedPixels)


# 91 Urban or suburban development; insufficient wildland fuel ----
figure1_1_scottBurgan$a91 = clcplus$eq(1)$Or(proba$select('discrete_classification')$eq(50))$multiply(100)$reduceResolution(reducer = ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(91));

# 98 water ----
figure1_1_scottBurgan$a98 = clcplus$gt(253)$Or(clcplus$eq(10))$multiply(100)$reduceResolution(reducer = ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(98));

# 99 barren ----
figure1_1_scottBurgan$a99 = clcplus$gt(253)$Or(clcplus$gt(7)$And(clcplus$lt(10) ))$multiply(100)$reduceResolution(reducer = ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(99));

# 92 snow ice ----
figure1_1_scottBurgan$a92 = clcplus$eq(11)$multiply(100)$reduceResolution(reducer = ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(92));

# agriculture in CLC+ is  5 or 7, but also check consensus with 100 m PROBA land cover
# conservative approach as if permanent herbaceaus it is better it goes in GR code of
# scott and burgan

figure1_1_scottBurgan$a93=shrubCLCplus$Or(clcplus$ eq(6))$Or(clcplus$ eq(7))$
  And(proba$select('discrete_classification')$eq(40))$
  multiply(100)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(93))

# 101 grasssparse ----
grassSparse=proba$select('discrete_classification')$eq(60)$And(clcplus$eq(6))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(101))

figure1_1_scottBurgan$a101=grassSparse

# GRASS  ----
grassProba=proba$select('discrete_classification')$eq(30)
grassCLCplus=clcplus$eq(6)

# GRASS LOW load ----
grassLowLoad=grassCLCplus$And(grassProba)$And(ndviMax$lt(0.2))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 102 dry ----
figure1_1_scottBurgan$a102=grassLowLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(102));
## 105 wet ----
figure1_1_scottBurgan$a105=grassLowLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(105))

# grass MOD load ----
grassModerateLoad=grassCLCplus$And(grassProba)$And(ndviMax$gte(0.2)$Or(ndviMax$lt(0.4)))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 104 dry ----
figure1_1_scottBurgan$a104=grassModerateLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(104));
## 106 wet ----
figure1_1_scottBurgan$a106=grassModerateLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(106))

# grass HIGH load ----
grassHighLoad=grassCLCplus$And(grassProba)$And(ndviMax$gte(0.4))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 107 dry ----
figure1_1_scottBurgan$a107=grassHighLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(107));
## 108 wet ----
figure1_1_scottBurgan$a108=grassHighLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(108))

#SHRUB  ----
shrubProba=proba$select('discrete_classification')$eq(20)
shrubCLCplus=clcplus$eq(5)

#SHRUB low load ----
shrubLowLoad=shrubCLCplus$And(shrubProba)$And(ndviMax$lt(0.2))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 141 dry ----
figure1_1_scottBurgan$a141=shrubLowLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(141));
## 146 wet ----
figure1_1_scottBurgan$a146=shrubLowLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(146))


#SHRUB MODERATE load ----
shrubModerateLoad=shrubCLCplus$And(shrubProba)$And(ndviMax$gte(0.2)$Or(ndviMax$lt(0.4)))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 142 dry ----
figure1_1_scottBurgan$a142=shrubModerateLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(142));
## 143 wet ----
figure1_1_scottBurgan$a143=shrubModerateLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(143))


#SHRUB high load ----
shrubHighLoad=shrubCLCplus$And(shrubProba)$And(ndviMax$gte(0.4)$Or(ndviMax$lt(0.6)))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 145 dry ----
figure1_1_scottBurgan$a145=shrubHighLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(145));
## 148 wet ----
figure1_1_scottBurgan$a148=shrubHighLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(148))

#############
# SHRUB VERY high load ----
shrubVeryHighLoad=shrubCLCplus$And(shrubProba)$And(ndviMax$gte(0.6))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 147 dry ----
figure1_1_scottBurgan$a147=shrubVeryHighLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(147));
## 149 wet ----
figure1_1_scottBurgan$a149=shrubVeryHighLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(149))


#############
# TIMBER UNDERSTOREY  ----
proba12x=proba$select('discrete_classification')$divide(10)$floor()$toByte()$eq(12)

# TIMBER UNDERSTOREY LOW+MEDIUM MODERATE load  ----
timberUnderstoreyLowMediumLoad=clcplus$gt(1)$Or(clcplus$ lt(7))$And(proba12x)$And(ndviMax$lte(0.6))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 161 dry  ----
  figure1_1_scottBurgan$a161=timberUnderstoreyLowMediumLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(161));
## 162 wet  ----
figure1_1_scottBurgan$a162=timberUnderstoreyLowMediumLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(162))

#############
#TIMBER UNDERSTOREY high load  ----
timberUnderstoreyHighLoad=clcplus$gt(1)$Or(clcplus$ lt(7))$And(proba12x)$And(ndviMax$gt(0.6))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 165 dry ----
figure1_1_scottBurgan$a165=timberUnderstoreyHighLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(165));
## 163 wet ----
figure1_1_scottBurgan$a163=timberUnderstoreyHighLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(163))

#############
#TIMBER LITTER CONIFER  -----
proba11x=proba$select('discrete_classification')$divide(10)$floor()$toByte()$eq(11)
clcplusTimber = clcplus$neq(3)$And( tcd$ )
## 183 Low-mediumLoadCompactLitter -----
  #weassumethatifcanopycoversallpixelandcanopyheightis20orabove,
  #wehaveamatureforestwithlowloadoflitter
figure1_1_scottBurgan$a183=clcplus$eq(2)$And(proba11x)$And(figure1_1$updatedHansen$gt(80))$And(canopy_height$gt(20))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(183))

## 185 HighLoadCompactLitter -----
#weassumethatifcanopycoversallpixelandcanopyheightis20orabove,
#wehaveamatureforestwithlowloadoflitter
figure1_1_scottBurgan$a185=clcplus$eq(2)$And(proba11x)$And(figure1_1$updatedHansen$lte(80)$Or(canopy_height$lte(20)))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(185))


#TIMBER LITTER BROADLEAVES -----
## 184 Low-mediumLoadCompactLitter -----
#weassumethatifcanopycoversallpixelandcanopyheightis20orabove,
#wehaveamatureforestwithlowloadoflitter
figure1_1_scottBurgan$a184=clcplus$eq(3)$Or(clcplus$ eq(4))$And(proba11x)$And(figure1_1$updatedHansen$gt(80))$And(canopy_height$gt(20))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(184))

## 186 High Load Compact Litter -----
#weassumethatifcanopycoversallpixelandcanopyheightis20orabove,
#wehaveamatureforestwithlowloadoflitter
figure1_1_scottBurgan$a186=clcplus$eq(3)$Or(clcplus$eq(4))$And(proba11x)$And(figure1_1$updatedHansen$lte(80)$Or(canopy_height$lte(20)))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(186))


figure1_1_scottBurgan$a254=grassCLCplus$And(proba11x$Or(proba12x))$And(figure1_1$updatedHansen$gt(20))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(254))


########## SLASH BLOWDOWN USING HANSEN LOSS
########## +++ USE COPERNICUS FOREST 10 m

for( imgn in names(figure1_1_scottBurgan) ){
  figure1_1_scottBurgan[[imgn]] = figure1_1_scottBurgan[[imgn]]$toByte()$reproject(proj)
}









ScottBurganProbs=ee$ImageCollection( unname(figure1_1_scottBurgan) )

ScottBurgan=ScottBurganProbs$qualityMosaic('prob')$rename(c('scottburgan_cprob',
                                                            'scottburgan_class') )

ss=ScottBurgan$getInfo()
n = pilotSites$size()$getInfo()

print(n)


task_img_container <- list()
task_img_containerAsset <- list()
for( i in 1:n){

  feature =ee$Feature(pilotSites$toList(12)$get(i-1))
  gg = feature$geometry();
  idf = feature$get("pilot_id")$getInfo() ;

  ScottBurganFiltered = ScottBurgan$select('scottburgan_class')
  id = paste0(idf,'_ScottBurganFuelMapClass')
  ScottBurganProbFiltered = ScottBurgan$select('scottburgan_cprob')
  idp = paste0(idf,'_ScottBurganFuelClassProb')



  if(0==1 && is.null(task_img_container[[id ]])){

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
  # task_img_container[[id ]]$status()
  # task_img_container[[id ]]$cancel()

  assetid <- paste0('projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelV2/',  id)
  message(assetid)

  if(is.null(task_img_containerAsset[[id ]])){

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
ee_imagecollection_to_local( ee$ImageCollection('projects/progetto-eu-h2020-cirgeo/assets/wildfire/fuelModelV1'),region= gg )
