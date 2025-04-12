library(rgee)
library(rgeeExtra)
library("inborutils")
library("parallel")
library("terra")


rgee::ee_install_upgrade()
HOME <- Sys.getenv("HOME")
# Sys.setenv("RETICULATE_PYTHON" = sprintf("%s/.virtualenvs/rgee/bin/python3", HOME))
Sys.setenv("RETICULATE_PYTHON" = sprintf("%s/.local/share/r-miniconda/bin/python3", HOME))
Sys.setenv("EARTHENGINE_GCLOUD" = sprintf("%s/google-cloud-sdk/bin/", HOME))

ee_install() 


ee_Initialize(quiet = T)
ee_Initialize(user = 'cirgeo', project = "progetto-eu-h2020-cirgeo" )




proj3035_30m = ee$Projection('EPSG:3035')$atScale(30);

dir.exists(outputdir) dir.create(outputdir)

library(stars)
# Main fuel types ESA World  Cover:
  # tree, shrub, grass, crop,  wet/peat,
# urban, nonfuel,
dsm = ee$ImageCollection('JAXA/ALOS/AW3D30/V3_2');
#Map.addLayer(tileEEA, {}, 'tiles EEA' )
aridityThreshold = 1;

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


lut = list(
  "a0000"=c("#cccccc",  "NoData",0,                                   "NB",  "NB",  0,  0    ),
  "a1111"=c("#ace188",  "Open broadleaf evergreen forest",1,          "SH7", "SH8", 147, 148 ),
  "a1112"=c("#ace100",  "Closed broadleaf evergreen forest",2,        "TU1", "TU2", 161, 162 ),
  "a1121"=c("#48cf88",  "Open broadleaf deciduous forest",3,          "SH5", "SH9", 145, 149 ),
  "a1122"=c("#48cf00",  "Closed broadleaf deciduous forest",4,        "TU5", "TU3", 165, 163 ),
  "a1211"=c("#625906",  "Open needleleaf evergreen forest",5,         "SH7", "SH8", 147, 148 ),
  "a1212"=c("#514e23",  "Closed needleleaf evergreen forest",6,       "TU1", "TU2", 161, 162 ),
  "a1221"=c("#514e89",  "Open needleleaf deciduous forest",7,         "SH5", "SH9", 145, 149 ),
  "a1222"=c("#514e23",  "Closed needleleaf deciduous forest",8,       "TU5", "TL3", 165, 183 ),
  "a1301"=c("#959900",  "Open mixed forest",9,                        "SH7", "SH8", 147, 148 ),
  "a1302"=c("#4f7321",  "Closed mixed forest",10,                     "TU5", "TL3", 165, 183 ),
  "a21"=c("#f0a978",  "Low shrubland [0–0.5 m)",11,                 "SH2", "SH3", 142, 143 ),
  "a22"=c("#eccb0e",  "Medium shrubland [0.5–1.5 m)",12,            "SH7", "SH8", 147, 148 ),
  "a23"=c("#d84200",  "High shrubland [≥1.5 m)",13,                 "SH5", "SH9", 145, 149 ),
  "a31"=c("#f8fda6",  "Low grassland [0–0.3 m)",14,                 "GR2", "GR6", 102, 103 ), # changed from Aragonese 106 to 103!
  "a32"=c("#f9ff3c",  "Medium grassland [0.3–0.7 m)",15,            "GR4", "GR8", 104, 108 ),
  "a33"=c("#e7ec00",  "High grassland [≥0.7 m)",16,                 "GR7", "GR9", 107, 109 ),
  "a41"=c("#e894ff",  "Herbaceous cropland",17,                     "NB3", "NB3", 93, 93   ),
  "a42"=c("#b600d7",  "Woody cropland",18,                          "NB3", "NB3", 93, 93   ),
  "a51"=c("#266566",  "Wet and peat/semi-peat land – tree",19,      "SH7", "SH8", 147, 148 ),
  "a52"=c("#3e999a",  "Wet and peat/semi-peat land – shrubland",20, "SH5", "SH9", 145, 149 ),
  "a53"=c("#5fe3ee",  "Wet and peat/semi-peat land – grassland",21, "GR7", "GR9", 107, 109 ),
  "a61"=c("#e90000",  "Urban continuous fabric",22,                  "NB",  "NB",   91, 91 ),
  "a62"=c("#ed3840",  "Urban discontinuous fabric",23,              "NB", "NB",  91,    91 ), #142, 143 ],
  "a7"=c("#ffffff",  "Nonfuel",24,                                  "NB",  "NB", 99,   99 )

)


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
onlyNonDisturbedPixels =  canopy_cover$select("lossyear")$unmask()$eq(0)$Or(canopy_cover$select("lossyear")$unmask()$gt(21) );
figure1_1$updatedHansen = canopy_cover$select(0)$unmask()$mask(onlyNonDisturbedPixels)
# then we draw consensus from WorldCover map and tree height map
# if world map and tree height map says that there are trees
# in loss pixel, then we bring back hansen's tree cover
forestPercCover = esaWorldCover10m$eq(10)$multiply(100)

# 91 Urban or suburban development; insufficient wildland fuel -- --
figure1_1_scottBurgan$a91 = clcplus$eq(1)$Or(proba$select('discrete_classification')$eq(50))$multiply(100)$reduceResolution(reducer = ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(91));

# 98 water ----
figure1_1_scottBurgan$a98 = clcplus$gt(253)$Or(clcplus$eq(10))$multiply(100)$reduceResolution(reducer = ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(98));

# 99 barren ---
figure1_1_scottBurgan$a99 = clcplus$gt(253)$Or(clcplus$gt(7)$And(clcplus$lt(10) ))$multiply(100)$reduceResolution(reducer = ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(99));

# snow ice
figure1_1_scottBurgan$a92 = clcplus$eq(11)$multiply(100)$reduceResolution(reducer = ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(92));

# agriculture in CLC+ is  5 or 7, but also check consensus with 100 m PROBA land cover
# conservative approach as if permanent herbaceaus it is better it goes in GR code of
# scott and burgan

figure1_1_scottBurgan$a93=clcplus$eq(5)$Or(clcplus$ eq(6))$Or(clcplus$ eq(7))$
  And(proba$select('discrete_classification')$eq(40))$
  multiply(100)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(93))

#grasssparse 101 ----
grassSparse=proba$select('discrete_classification')$eq(60)$And(clcplus$eq(6))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(101))

figure1_1_scottBurgan$a101=grassSparse

  #GRASSlowload ----
grass=proba$select('discrete_classification')$eq(30)
grassLowLoad=clcplus$eq(6)$And(grass)$And(ndviMax$lt(0.2))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 102 dry ----
figure1_1_scottBurgan$a102=grassLowLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(102));
## 105 wet ----
figure1_1_scottBurgan$a105=grassLowLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(105))

#############
#grassmoderateload ----
grassModerateLoad=clcplus$eq(6)$And(grass)$And(ndviMax$gte(0.2)$Or(ndviMax$lt(0.4)))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 104 dry ----
figure1_1_scottBurgan$a104=grassModerateLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(104));
## 106 wet ----
figure1_1_scottBurgan$a106=grassModerateLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(106))

#grasshighload ----
grassHighLoad=clcplus$eq(6)$And(grass)$And(ndviMax$gte(0.4))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 107 dry ----
figure1_1_scottBurgan$a107=grassHighLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(107));
## 108 wet ----
figure1_1_scottBurgan$a108=grassHighLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(108))

  #SHRUBlowload ----
  shrub=proba$select('discrete_classification')$eq(20)
shrubLowLoad=clcplus$eq(5)$And(shrub)$And(ndviMax$lt(0.2))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 141 dry ----
figure1_1_scottBurgan$a141=shrubLowLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(141));
## 146 wet ----
figure1_1_scottBurgan$a146=shrubLowLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(146))


#SHRUBmoderateload ----
shrubModerateLoad=clcplus$eq(5)$And(shrub)$And(ndviMax$gte(0.2)$Or(ndviMax$lt(.4)))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 142 dry ----
figure1_1_scottBurgan$a142=shrubModerateLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(142));
## 143 wet ----
figure1_1_scottBurgan$a143=shrubModerateLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(143))


#SHRUB high load ----
shrubHighLoad=clcplus$eq(5)$And(shrub)$And(ndviMax$gte(0.4)$Or(ndviMax$lt(0.6)))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 145 dry ----
figure1_1_scottBurgan$a145=shrubHighLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(145));
## 148 wet ----
figure1_1_scottBurgan$a148=shrubHighLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(148))

#############
#SHRUBVERYhighload ----
shrubVeryHighLoad=clcplus$eq(5)$And(shrub)$And(ndviMax$gte(0.6))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 147 dry ----
figure1_1_scottBurgan$a147=shrubVeryHighLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(147));
## 149 wet ----
figure1_1_scottBurgan$a149=shrubVeryHighLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(149))


#############
# TIMBER UNDERSTOREY LOW+MEDIUM MODERATE load  ----
proba12x = proba$select('discrete_classification')$divide(10)$floor()$toByte()$eq(12)

timberUnderstoreyLowMediumLoad = clcplus$gt(1)$Or(clcplus$ lt(7))$And(ndviMax$lte(0.6))$And(proba12x)$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)

## 161 dry  ----
figure1_1_scottBurgan$a161=timberUnderstoreyLowMediumLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(161));
## 162 wet  ----
figure1_1_scottBurgan$a162=timberUnderstoreyLowMediumLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(162))

#############
#TIMBER UNDERSTOREY highload  ----
timberUnderstoreyHighLoad=clcplus$gt(1)$Or(clcplus$ lt(7))$And(ndviMax$gt(0.6))$And(proba12x)$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)
## 165 dry ----
figure1_1_scottBurgan$a165=timberUnderstoreyHighLoad$mask(aridityIndex$lte(aridityThreshold))$rename("prob")$addBands(ee$Image(165));
## 163 wet ----
figure1_1_scottBurgan$a163=timberUnderstoreyHighLoad$mask(aridityIndex$gt(aridityThreshold))$rename("prob")$addBands(ee$Image(163))


#############
#TIMBER LITTER CONIFER  -----
proba11x=proba$select('discrete_classification')$divide(10)$floor()$toByte()$eq(11)

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


figure1_1_scottBurgan$a254=clcplus$eq(6)$And(proba11x$Or(proba12x))$And(figure1_1$updatedHansen$gt(20))$multiply(99)$reduceResolution(reducer=ee$Reducer$mean())$reproject(proj)$rename("prob")$addBands(ee$Image(254))

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
