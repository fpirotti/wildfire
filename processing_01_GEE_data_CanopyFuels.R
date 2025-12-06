library(rgee)
library(rgeeExtra)
library(stars)
library(googledrive)

### setting version ----
versionFuelModel  = 3

########### THIS REQUIRES FIRST THAT THE processing_01_GEE_tileMeta.R!
# 1. Authenticate ----
drive_auth(email = "cirgeo@unipd.it")
ee_Initialize(user = 'cirgeo'  )


# FeatureCollections and Images
pilotRegions <- ee$FeatureCollection(
  "projects/progetto-eu-h2020-cirgeo/assets/wildfire/pilotRegions"
)

pilotSites <- ee$FeatureCollection(
  "projects/progetto-eu-h2020-cirgeo/assets/wildfire/wildfire_pilot_sites_v3"
)

agbcoll <- ee$ImageCollection("projects/sat-io/open-datasets/ESA/ESA_CCI_AGB")
agb <- agbcoll$
  filterDate("2021-01-01", "2023-01-01")$
  first()$
  select("AGB")

agb_sd <- agbcoll$
  filterDate("2021-01-01", "2023-01-01")$
  first()$
  select("SD")

biomass <- agb
biomass_sd <- agb_sd

hansen <- ee$Image("UMD/hansen/global_forest_change_2024_v1_12")

clc <- ee$Image(
  "projects/progetto-eu-h2020-cirgeo/assets/copernicus/CLMS_CLCplus_RASTER_2023"
)

tcd <- ee$ImageCollection(
  "projects/progetto-eu-h2020-cirgeo/assets/copernicus/CLMS_TCF_TreeDensity_RASTER_2021"
)

canopy_height <- ee$ImageCollection(
  "projects/progetto-eu-h2020-cirgeo/assets/wildfire/canopyHeightFromMeta10m"
)$mosaic()

nuts <- ee$FeatureCollection(
  "projects/progetto-eu-h2020-cirgeo/assets/NUTS_RG_01M_2024_4326"
)

codiciNutsInProject <- c(
  "DED2F", "CZ042", "CZ064", "AT124",
  "AT125", "ITH43", "AT211", "AT212",
  "SI043", "ITH42", "SI042"
)

nutsAll <- nuts$filter(ee$Filter$eq("LEVL_CODE", 3))

nuts2use <- nuts$filter(ee$Filter$inList("NUTS_ID", codiciNutsInProject))

pilotRegions <- nuts2use$map(
  ee_utils_pyfunc(function(f) f$geometry()$buffer(1000)$dissolve())
)$geometry()$dissolve()

# ---- parametersCanopyBaseHeight dictionary ----
parametersCanopyBaseHeight <- ee$Dictionary(list(
  veg_abies_alba_anv_v3 = c(0.0, 0.253, 2.039462437),
  veg_castanea_sativa_anv_v3 = c(0.0, 0.550, 2.944425115),
  veg_corylus_avellana_anv_v3 = c(0.0, 0.637, 2.143781294),
  veg_fagus_sylvatica_anv_v3 = c(0.0, 0.510, 4.518709433),
  veg_olea_europaea_anv_v3 = c(0.0, 0.629, 1.119391027),
  veg_picea_abies_anv_v3 = c(0.0, 0.167, 3.135843265),
  veg_pinus_halepensis_anv_v3 = c(0.0, 0.523, 1.567688906),
  veg_pinus_nigra_anv_v3 = c(0.0, 0.503, 1.745724687),
  veg_pinus_pinea_anv_v3 = c(0.0, 0.446, 1.544889678),
  veg_pinus_sylvestris_anv_v3 = c(0.0, 0.472, 2.254288877),
  veg_prunus_avium_anv_v3 = c(0.0, 0.325, 2.939292084),
  veg_quercus_cerris_anv_v3 = c(0.0, 0.410, 1.359665539),
  veg_quercus_ilex_anv_v3 = c(0.0, 0.394, 0.993522344),
  veg_quercus_robur_anv_v3 = c(0.0, 0.671, 2.843008899),
  veg_quercus_suber_anv_v3 = c(0.0, 0.369, 1.653155781),
  veg_salix_caprea_anv_v3 = c(0.0, 0.389, 2.26567518)
))

dbh2canopyBiomassFraction2 <- list(
  veg_abies_alba_anv_v3 =
    '(exp(-2.3958  + 2.4497  *log(DBH)) - exp(-3.2683 + 2.5768 * log(DBH))) /
     exp(-2.3958 + 2.4497 * log(DBH))',

  veg_castanea_sativa_anv_v3 =
    '(exp(-6.2950  + 2.3956  *log(DBH)) + 0.111 * exp(-3.6834 + 2.3812 * log(DBH))) /
     (exp(-6.295 + 2.3956 * log(DBH)) +
      exp(-3.6834 + 2.3812 * log(DBH)) +
      exp(-2.23 + 2.3129 * log(DBH)))',

  veg_corylus_avellana_anv_v3 =
    '(exp(0.0180 + 2.051 * log(DBH)) + 0.111 * exp(0.007 + 3.547 * log(DBH))) /
     (exp(0.018 + 2.051 * log(DBH)) +
      exp(0.007 + 3.547 * log(DBH)) +
      exp(0.029 + 2.353 * log(DBH)))',

  veg_fagus_sylvatica_anv_v3 =
    '(exp(-4.4813 + 1.9073 * log(DBH)) + 0.111 * exp(-4.322 + 2.7599 * log(DBH))) /
     (exp(-4.4813 + 1.9073 * log(DBH)) +
      exp(-4.322 + 2.7599 * log(DBH)) +
      exp(-1.4487 + 2.1661 * log(DBH)))',

  veg_olea_europaea_anv_v3 =
    '(exp(-3.52781 + 1.53326 * log(DBH)) + 0.111 * exp(-4.65302 + 2.68734 * log(DBH))) /
     (exp(-3.52781 + 1.53326 * log(DBH)) +
      exp(-4.65302 + 2.68734 * log(DBH)) +
      exp(-1.0421 + 1.60779 * log(DBH)))',

  veg_picea_abies_anv_v3 =
    '(exp(-2.7957 + 1.8688 * log(DBH)) + 0.111 * exp(-3.3163 + 2.1983 * log(DBH))) /
     (exp(-2.7957 + 1.8688 * log(DBH)) +
      exp(-3.3163 + 2.1983 * log(DBH)) +
      exp(-2.5027 + 2.3404 * log(DBH)))',

  veg_pinus_halepensis_anv_v3 =
    'exp(-3.7090 + 2.2890 * log(DBH)) /
     (exp(-3.709 + 2.289 * log(DBH)) +
      exp(-2.51839 + 2.13609 * log(DBH)))',

  veg_pinus_nigra_anv_v3 =
    '(exp(-0.6105 + 0.8705 * log(DBH)) + 0.111 * exp(-1.1351 + 1.468 * log(DBH))) /
     (exp(-0.6105 + 0.8705 * log(DBH)) +
      exp(-1.1351 + 1.468 * log(DBH)) +
      exp(-2.5551 + 2.3691 * log(DBH)))',

  veg_pinus_pinea_anv_v3 =
    '((DBH^1.6788) * 0.6184 + ((DBH/100)^4.248054) * 2061.7740) /
     (((DBH^1.6788) * 0.6184) +
      ((DBH/100)^4.248054) * 2061.7740 +
      ((DBH^3.2469) * 0.0024))',

  veg_pinus_sylvestris_anv_v3 =
    '(exp(-3.5276 + 1.7471 * log(DBH)) + 0.111 * exp(-3.6641 + 2.1601 * log(DBH))) /
     (exp(-3.5276 + 1.7471 * log(DBH)) +
      exp(-3.6641 + 2.1601 * log(DBH)) +
      exp(-2.3583 + 2.308 * log(DBH)))',

  veg_prunus_avium_anv_v3 =
    '(exp(-4.1058 + 1.3212 * log(DBH)) + 0.111 * exp(-2.6762 + 2.2061 * log(DBH))) /
     (exp(-4.1058 + 1.3212 * log(DBH)) +
      exp(-2.6762 + 2.2061 * log(DBH)) +
      exp(-1.0948 + 1.9656 * log(DBH)))',

  veg_quercus_cerris_anv_v3 =
    '(exp(-0.959 + 1.895 * log(DBH)) + 0.111 * exp(-0.599 + 1.918 * log(DBH))) /
     (exp(-0.959 + 1.895 * log(DBH)) +
      exp(-0.599 + 1.918 * log(DBH)) +
      exp(-0.303 + 1.901 * log(DBH)))',

  veg_quercus_ilex_anv_v3 =
    '(exp(-4.4998 + 2.1018 * log(DBH)) + 0.111 * exp(-2.985 + 2.3093 * log(DBH))) /
     (exp(-4.4998 + 2.1018 * log(DBH)) +
      exp(-2.985 + 2.3093 * log(DBH)) +
      exp(-2.1809 + 2.2686 * log(DBH)))',

  veg_quercus_robur_anv_v3 =
    '(exp(-4.46630 + 2.1375 * log(DBH)) + 0.111 * exp(-4.4339 + 2.9526 * log(DBH))) /
     (exp(-4.4663 + 2.1375 * log(DBH)) +
      exp(-4.4339 + 2.9526 * log(DBH)) +
      exp(-2.7054 + 2.5279 * log(DBH)))',

  veg_quercus_suber_anv_v3 =
    '(exp(0.600169 + 1.355957 * log(DBH)) + 0.111 * (DBH^0.5831 * 5.918)) /
     (exp(0.600169 + 1.355957 * log(DBH)) +
      (DBH^0.5831 * 5.918) +
      exp(0.164185 + 2.011002 * log(DBH)))',

  veg_salix_caprea_anv_v3 =
    '(exp(1.4718 + 2.3117 * log(DBH)) + 0.111 * exp(4.5086 + 1.9234 * log(DBH))) /
     (exp(1.4718 + 2.3117 * log(DBH)) +
      exp(4.4721 + 2.4987 * log(DBH)) +
      exp(4.5086 + 1.9234 * log(DBH)))'
)

dbh2canopyBiomassFraction_errorPropagation2 <- list(
  veg_abies_alba_anv_v3 =
    '(2.4497 * exp(2.4497 * log(DBH) - 2.3958) -
      (2.4497 * (exp(2.4497 * log(DBH) - 2.3958) - exp(2.5768 * log(DBH) - 3.2683)) +
       2.5768 * exp(2.5768 * log(DBH) - 3.2683))) /
     (DBH * exp(2.4497 * log(DBH) - 2.3958))',

  veg_castanea_sativa_anv_v3 =
    '(0.2643132 * exp(2.3812 * log(DBH) - 3.6834) + 2.3956 * exp(2.3956 * log(DBH) - 6.295) -
      (0.111 * exp(2.3812 * log(DBH) - 3.6834) + exp(2.3956 * log(DBH) - 6.295)) *
      (2.3129 * exp(2.3129 * log(DBH) - 2.23) + 2.3812 * exp(2.3812 * log(DBH) - 3.6834) + 2.3956 * exp(2.3956 * log(DBH) - 6.295)) /
      (exp(2.3129 * log(DBH) - 2.23) + exp(2.3812 * log(DBH) - 3.6834) + exp(2.3956 * log(DBH) - 6.295))) /
     (DBH * (exp(2.3129 * log(DBH) - 2.23) + exp(2.3812 * log(DBH) - 3.6834) + exp(2.3956 * log(DBH) - 6.295)))',

  veg_corylus_avellana_anv_v3 =
    '(0.393717 * exp(0.007 + 3.547 * log(DBH)) + 2.051 * exp(0.018 + 2.051 * log(DBH)) -
      (0.111 * exp(0.007 + 3.547 * log(DBH)) + exp(0.018 + 2.051 * log(DBH))) *
      (2.051 * exp(0.018 + 2.051 * log(DBH)) + 2.353 * exp(0.029 + 2.353 * log(DBH)) + 3.547 * exp(0.007 + 3.547 * log(DBH))) /
      (exp(0.007 + 3.547 * log(DBH)) + exp(0.018 + 2.051 * log(DBH)) + exp(0.029 + 2.353 * log(DBH)))) /
     (DBH * (exp(0.007 + 3.547 * log(DBH)) + exp(0.018 + 2.051 * log(DBH)) + exp(0.029 + 2.353 * log(DBH))))',

  veg_fagus_sylvatica_anv_v3 =
    '(0.3063489 * exp(2.7599 * log(DBH) - 4.322) + 1.9073 * exp(1.9073 * log(DBH) - 4.4813) -
      (0.111 * exp(2.7599 * log(DBH) - 4.322) + exp(1.9073 * log(DBH) - 4.4813)) *
      (1.9073 * exp(1.9073 * log(DBH) - 4.4813) + 2.1661 * exp(2.1661 * log(DBH) - 1.4487) + 2.7599 * exp(2.7599 * log(DBH) - 4.322)) /
      (exp(1.9073 * log(DBH) - 4.4813) + exp(2.1661 * log(DBH) - 1.4487) + exp(2.7599 * log(DBH) - 4.322))) /
     (DBH * (exp(1.9073 * log(DBH) - 4.4813) + exp(2.1661 * log(DBH) - 1.4487) + exp(2.7599 * log(DBH) - 4.322)))',

  veg_olea_europaea_anv_v3 =
    '(0.29829474 * exp(2.68734 * log(DBH) - 4.65302) + 1.53326 * exp(1.53326 * log(DBH) - 3.52781) -
      (0.111 * exp(2.68734 * log(DBH) - 4.65302) + exp(1.53326 * log(DBH) - 3.52781)) *
      (1.53326 * exp(1.53326 * log(DBH) - 3.52781) + 1.60779 * exp(1.60779 * log(DBH) - 1.0421) + 2.68734 * exp(2.68734 * log(DBH) - 4.65302)) /
      (exp(1.53326 * log(DBH) - 3.52781) + exp(1.60779 * log(DBH) - 1.0421) + exp(2.68734 * log(DBH) - 4.65302))) /
     (DBH * (exp(1.53326 * log(DBH) - 3.52781) + exp(1.60779 * log(DBH) - 1.0421) + exp(2.68734 * log(DBH) - 4.65302)))'

  # …and so on for the other species, following the same structure
)

