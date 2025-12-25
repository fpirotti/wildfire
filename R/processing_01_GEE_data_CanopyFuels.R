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
heightWeightscoll <- ee$ImageCollection("users/cirgeo/wildfire/biomassCanopyHeightsUpsampleWeights10m")
heightWeights <- ee$Image(heightWeightscoll$mosaic()$setDefaultProjection(heightWeightscoll$first()$projection()))
agb <- heightWeights$multiply( agbcoll$
    filterDate("2021-01-01", "2023-01-01")$
    first()$
    select("AGB")
  )

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

canopy_height_coll <- ee$ImageCollection(
  'users/cirgeo/wildfire/canopyHeightFromMeta10m'
);

canopy_height <- canopy_height_coll$select(0)$mosaic()$setDefaultProjection(canopy_height_coll$first()$projection());

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

# EQUATION PARAMTERS -----
## treeH2treeCBH -----
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
## treeDBH2thinBiomassFraction -----
dbh2canopyBiomassFraction <- ee$Dictionary(  list(
  veg_abies_alba_anv_v3 = '1 - (exp(-0.8725) * DBH^0.1271)',

  veg_castanea_sativa_anv_v3 =
    '(0.001845 * DBH^2.3956 + 0.002796 * DBH^2.3812) /
     (0.001845 * DBH^2.3956 + 0.025137 * DBH^2.3812 + 0.107524 * DBH^2.3129)',

  veg_corylus_avellana_anv_v3 =
    '(1.01816 * DBH^2.051 + 0.11177 * DBH^3.547) /
     (1.01816 * DBH^2.051 + 1.00702 * DBH^3.547 + 1.02942 * DBH^2.353)',

  veg_fagus_sylvatica_anv_v3 =
    '(0.011318 * DBH^1.9073 + 0.001469 * DBH^2.7599) /
     (0.011318 * DBH^1.9073 + 0.013273 * DBH^2.7599 + 0.234878 * DBH^2.1661)',

  veg_olea_europaea_anv_v3 =
    '(0.029369 * DBH^1.53326 + 0.001058 * DBH^2.68734) /
     (0.029369 * DBH^1.53326 + 0.009533 * DBH^2.68734 + 0.352709 * DBH^1.60779)',

  veg_picea_abies_anv_v3 =
    '(0.061072 * DBH^1.8688 + 0.004031 * DBH^2.1983) /
     (0.061072 * DBH^1.8688 + 0.036314 * DBH^2.1983 + 0.081864 * DBH^2.3404)',

  veg_pinus_halepensis_anv_v3 =
    '(0.024497 * DBH^2.289) /
     (0.024497 * DBH^2.289 + 0.08059 * DBH^2.13609)',

  veg_pinus_nigra_anv_v3 =
    '(0.543077 * DBH^0.8705 + 0.035665 * DBH^1.468) /
     (0.543077 * DBH^0.8705 + 0.32139 * DBH^1.468 + 0.077684 * DBH^2.3691)',

  veg_pinus_pinea_anv_v3 =
  '(0.6184 * DBH^1.6788 + 0.00000657859 * DBH^4.248054) /
     (0.6184 * DBH^1.6788 + 0.00000657859 * DBH^4.248054 + 0.0024 * DBH^3.2469)',

  veg_pinus_sylvestris_anv_v3 =
    '(0.029375 * DBH^1.7471 + 0.002844 * DBH^2.1601) /
     (0.029375 * DBH^1.7471 + 0.025627 * DBH^2.1601 + 0.09458 * DBH^2.308)',

  veg_prunus_avium_anv_v3 =
    '(0.016477 * DBH^1.3212 + 0.007638 * DBH^2.2061) /
     (0.016477 * DBH^1.3212 + 0.068812 * DBH^2.2061 + 0.334604 * DBH^1.9656)',

  veg_quercus_cerris_anv_v3 =
    '(0.38327 * DBH^1.895 + 0.06098 * DBH^1.918) /
     (0.38327 * DBH^1.895 + 0.54936 * DBH^1.918 + 0.73860 * DBH^1.901)',

  veg_quercus_ilex_anv_v3 =
    '(0.011111 * DBH^2.1018 + 0.005615 * DBH^2.3093) /
     (0.011111 * DBH^2.1018 + 0.050539 * DBH^2.3093 + 0.11294 * DBH^2.2686)',

  veg_quercus_robur_anv_v3 =
    '(0.011489 * DBH^2.1375 + 0.001314 * DBH^2.9526) /
     (0.011489 * DBH^2.1375 + 0.011868 * DBH^2.9526 + 0.066843 * DBH^2.5279)',

  veg_quercus_suber_anv_v3 =
    '(1.82242 * DBH^1.3559 + 0.65689 * DBH^0.5831) /
     (1.82242 * DBH^1.3559 + 5.918 * DBH^0.5831 + 1.17843 * DBH^2.011)',

  veg_salix_caprea_anv_v3 =
    '(4.3571 * DBH^2.3117 + 10.076 * DBH^1.9234) /
     (4.3571 * DBH^2.3117 + 87.540 * DBH^2.4987 + 90.795 * DBH^1.9234)'
)
)
#ee$Dictionary(
# dbh2canopyBiomassFraction2 <-  list(
#   veg_abies_alba_anv_v3 =
#     '(exp(-2.3958  + 2.4497  *log(DBH)) - exp(-3.2683 + 2.5768 * log(DBH))) /
#      exp(-2.3958 + 2.4497 * log(DBH))',
#   veg_castanea_sativa_anv_v3 =
#     '(exp(-6.2950  + 2.3956  *log(DBH)) + 0.111 * exp(-3.6834 + 2.3812 * log(DBH))) /
#      (exp(-6.295 + 2.3956 * log(DBH)) +
#       exp(-3.6834 + 2.3812 * log(DBH)) +
#       exp(-2.23 + 2.3129 * log(DBH)))',
#
#   veg_corylus_avellana_anv_v3 =
#     '(exp(0.0180 + 2.051 * log(DBH)) + 0.111 * exp(0.007 + 3.547 * log(DBH))) /
#      (exp(0.018 + 2.051 * log(DBH)) +
#       exp(0.007 + 3.547 * log(DBH)) +
#       exp(0.029 + 2.353 * log(DBH)))',
#
#   veg_fagus_sylvatica_anv_v3 =
#     '(exp(-4.4813 + 1.9073 * log(DBH)) + 0.111 * exp(-4.322 + 2.7599 * log(DBH))) /
#      (exp(-4.4813 + 1.9073 * log(DBH)) +
#       exp(-4.322 + 2.7599 * log(DBH)) +
#       exp(-1.4487 + 2.1661 * log(DBH)))',
#
#   veg_olea_europaea_anv_v3 =
#     '(exp(-3.52781 + 1.53326 * log(DBH)) + 0.111 * exp(-4.65302 + 2.68734 * log(DBH))) /
#      (exp(-3.52781 + 1.53326 * log(DBH)) +
#       exp(-4.65302 + 2.68734 * log(DBH)) +
#       exp(-1.0421 + 1.60779 * log(DBH)))',
#
#   veg_picea_abies_anv_v3 =
#     '(exp(-2.7957 + 1.8688 * log(DBH)) + 0.111 * exp(-3.3163 + 2.1983 * log(DBH))) /
#      (exp(-2.7957 + 1.8688 * log(DBH)) +
#       exp(-3.3163 + 2.1983 * log(DBH)) +
#       exp(-2.5027 + 2.3404 * log(DBH)))',
#
#   veg_pinus_halepensis_anv_v3 =
#     'exp(-3.7090 + 2.2890 * log(DBH)) /
#      (exp(-3.709 + 2.289 * log(DBH)) +
#       exp(-2.51839 + 2.13609 * log(DBH)))',
#
#   veg_pinus_nigra_anv_v3 =
#     '(exp(-0.6105 + 0.8705 * log(DBH)) + 0.111 * exp(-1.1351 + 1.468 * log(DBH))) /
#      (exp(-0.6105 + 0.8705 * log(DBH)) +
#       exp(-1.1351 + 1.468 * log(DBH)) +
#       exp(-2.5551 + 2.3691 * log(DBH)))',
#
#   veg_pinus_pinea_anv_v3 =
#     '((DBH^1.6788) * 0.6184 + ((DBH/100)^4.248054) * 2061.7740) /
#      (((DBH^1.6788) * 0.6184) +
#       ((DBH/100)^4.248054) * 2061.7740 +
#       ((DBH^3.2469) * 0.0024))',
#
#   veg_pinus_sylvestris_anv_v3 =
#     '(exp(-3.5276 + 1.7471 * log(DBH)) + 0.111 * exp(-3.6641 + 2.1601 * log(DBH))) /
#      (exp(-3.5276 + 1.7471 * log(DBH)) +
#       exp(-3.6641 + 2.1601 * log(DBH)) +
#       exp(-2.3583 + 2.308 * log(DBH)))',
#
#   veg_prunus_avium_anv_v3 =
#     '(exp(-4.1058 + 1.3212 * log(DBH)) + 0.111 * exp(-2.6762 + 2.2061 * log(DBH))) /
#      (exp(-4.1058 + 1.3212 * log(DBH)) +
#       exp(-2.6762 + 2.2061 * log(DBH)) +
#       exp(-1.0948 + 1.9656 * log(DBH)))',
#
#   veg_quercus_cerris_anv_v3 =
#     '(exp(-0.959 + 1.895 * log(DBH)) + 0.111 * exp(-0.599 + 1.918 * log(DBH))) /
#      (exp(-0.959 + 1.895 * log(DBH)) +
#       exp(-0.599 + 1.918 * log(DBH)) +
#       exp(-0.303 + 1.901 * log(DBH)))',
#
#   veg_quercus_ilex_anv_v3 =
#     '(exp(-4.4998 + 2.1018 * log(DBH)) + 0.111 * exp(-2.985 + 2.3093 * log(DBH))) /
#      (exp(-4.4998 + 2.1018 * log(DBH)) +
#       exp(-2.985 + 2.3093 * log(DBH)) +
#       exp(-2.1809 + 2.2686 * log(DBH)))',
#
#   veg_quercus_robur_anv_v3 =
#     '(exp(-4.46630 + 2.1375 * log(DBH)) + 0.111 * exp(-4.4339 + 2.9526 * log(DBH))) /
#      (exp(-4.4663 + 2.1375 * log(DBH)) +
#       exp(-4.4339 + 2.9526 * log(DBH)) +
#       exp(-2.7054 + 2.5279 * log(DBH)))',
#
#   veg_quercus_suber_anv_v3 =
#     '(exp(0.600169 + 1.355957 * log(DBH)) + 0.111 * (DBH^0.5831 * 5.918)) /
#      (exp(0.600169 + 1.355957 * log(DBH)) +
#       (DBH^0.5831 * 5.918) +
#       exp(0.164185 + 2.011002 * log(DBH)))',
#
#   veg_salix_caprea_anv_v3 =
#     '(exp(1.4718 + 2.3117 * log(DBH)) + 0.111 * exp(4.5086 + 1.9234 * log(DBH))) /
#      (exp(1.4718 + 2.3117 * log(DBH)) +
#       exp(4.4721 + 2.4987 * log(DBH)) +
#       exp(4.5086 + 1.9234 * log(DBH)))'
# )
# )

## dbh2canopyBiomassFraction error propagation ------

dbh2canopyBiomassFraction_errorPropagation <-  ee$Dictionary(
  list(
    # Abies is a special case (subtraction model): Error = |a * b * DBH^(b-1)|
    veg_abies_alba_anv_v3 =
      'sqrt( pow(0.0531 * pow(DBH, -0.8729) * errDBH, 2)  )',

    # Castanea sativa: Scaling diff ~0.05
    veg_castanea_sativa_anv_v3 =
      'sqrt( pow(Ratio * (1 - Ratio) * 0.053 * (errDBH / DBH), 2)  )',

    # Corylus avellana: High divergence in exponents (~1.2)
    veg_corylus_avellana_anv_v3 =
      'sqrt( pow(Ratio * (1 - Ratio) * 1.194 * (errDBH / DBH), 2)  )',

    # Fagus sylvatica: Scaling diff ~0.61
    veg_fagus_sylvatica_anv_v3 =
      'sqrt( pow(Ratio * (1 - Ratio) * 0.608 * (errDBH / DBH), 2)  )',

    # Olea europaea: Scaling diff ~0.15
    veg_olea_europaea_anv_v3 =
      'sqrt( pow(Ratio * (1 - Ratio) * 0.149 * (errDBH / DBH), 2)  )',

    # Picea abies: Scaling diff ~0.35
    veg_picea_abies_anv_v3 =
      'sqrt( pow(Ratio * (1 - Ratio) * 0.347 * (errDBH / DBH), 2)  )',

    # Pinus halepensis: Scaling diff ~0.15
    veg_pinus_halepensis_anv_v3 =
      'sqrt( pow(Ratio * (1 - Ratio) * 0.153 * (errDBH / DBH), 2)  )',

    # Pinus nigra: Scaling diff ~0.95
    veg_pinus_nigra_anv_v3 =
      'sqrt( pow(Ratio * (1 - Ratio) * 0.941 * (errDBH / DBH), 2)  )',

    # Pinus pinea: Scaling diff ~1.57 (High sensitivity at large DBH)
    veg_pinus_pinea_anv_v3 =
      'sqrt( pow(Ratio * (1 - Ratio) * 1.569 * (errDBH / DBH), 2)  )',

    # Pinus sylvestris: Scaling diff ~0.45
    veg_pinus_sylvestris_anv_v3 =
      'sqrt( pow(Ratio * (1 - Ratio) * 0.448 * (errDBH / DBH), 2)  )',

    # Prunus avium: Scaling diff ~0.40
    veg_prunus_avium_anv_v3 =
      'sqrt( pow(Ratio * (1 - Ratio) * 0.395 * (errDBH / DBH), 2)  )',

    # Quercus cerris: Very low scaling diff (~0.01) - very stable ratio
    veg_quercus_cerris_anv_v3 =
      'sqrt( pow(Ratio * (1 - Ratio) * 0.012 * (errDBH / DBH), 2)  )',

    # Quercus ilex: Scaling diff ~0.15
    veg_quercus_ilex_anv_v3 =
      'sqrt( pow(Ratio * (1 - Ratio) * 0.146 * (errDBH / DBH), 2)  )',

    # Quercus robur: Scaling diff ~0.61
    veg_quercus_robur_anv_v3 =
      'sqrt( pow(Ratio * (1 - Ratio) * 0.614 * (errDBH / DBH), 2)  )',

    # Quercus suber: Scaling diff ~0.65
    veg_quercus_suber_anv_v3 =
      'sqrt( pow(Ratio * (1 - Ratio) * 0.655 * (errDBH / DBH), 2)  )',

    # Salix caprea: Scaling diff ~0.37
    veg_salix_caprea_anv_v3 =
      'sqrt( pow(Ratio * (1 - Ratio) * 0.369 * (errDBH / DBH), 2)  )'
  )
)

dbh2canopyBiomassFraction_errorPropagation3 <- list(
  # Abies is a special case (subtraction model): Error = |a * b * DBH^(b-1)|
  veg_abies_alba_anv_v3 =
    '((0.0531 * DBH**-0.8729 * errDBH)**2)**0.5',

  # Castanea sativa: Scaling diff ~0.05
  veg_castanea_sativa_anv_v3 =
    '((Ratio * (1 - Ratio) * 0.053 * (errDBH / DBH))**2)**0.5',

  # Corylus avellana: High divergence in exponents (~1.2)
  veg_corylus_avellana_anv_v3 =
    '((Ratio * (1 - Ratio) * 1.194 * (errDBH / DBH))**2)**0.5',

  # Fagus sylvatica: Scaling diff ~0.61
  veg_fagus_sylvatica_anv_v3 =
    '((Ratio * (1 - Ratio) * 0.608 * (errDBH / DBH))**2)**0.5',

  # Olea europaea: Scaling diff ~0.15
  veg_olea_europaea_anv_v3 =
    '((Ratio * (1 - Ratio) * 0.149 * (errDBH / DBH))**2)**0.5',

  # Picea abies: Scaling diff ~0.35
  veg_picea_abies_anv_v3 =
    '((Ratio * (1 - Ratio) * 0.347 * (errDBH / DBH))**2)**0.5',

  # Pinus halepensis: Scaling diff ~0.15
  veg_pinus_halepensis_anv_v3 =
    '((Ratio * (1 - Ratio) * 0.153 * (errDBH / DBH))**2)**0.5',

  # Pinus nigra: Scaling diff ~0.95
  veg_pinus_nigra_anv_v3 =
    '((Ratio * (1 - Ratio) * 0.941 * (errDBH / DBH))**2)**0.5',

  # Pinus pinea: Scaling diff ~1.57 (High sensitivity at large DBH)
  veg_pinus_pinea_anv_v3 =
    '((Ratio * (1 - Ratio) * 1.569 * (errDBH / DBH))**2)**0.5',

  # Pinus sylvestris: Scaling diff ~0.45
  veg_pinus_sylvestris_anv_v3 =
    '((Ratio * (1 - Ratio) * 0.448 * (errDBH / DBH))**2)**0.5',

  # Prunus avium: Scaling diff ~0.40
  veg_prunus_avium_anv_v3 =
    '((Ratio * (1 - Ratio) * 0.395 * (errDBH / DBH))**2)**0.5',

  # Quercus cerris: Very low scaling diff (~0.01) - very stable ratio
  veg_quercus_cerris_anv_v3 =
    '((Ratio * (1 - Ratio) * 0.012 * (errDBH / DBH))**2)**0.5',

  # Quercus ilex: Scaling diff ~0.15
  veg_quercus_ilex_anv_v3 =
    '((Ratio * (1 - Ratio) * 0.146 * (errDBH / DBH))**2)**0.5',

  # Quercus robur: Scaling diff ~0.61
  veg_quercus_robur_anv_v3 =
    '((Ratio * (1 - Ratio) * 0.614 * (errDBH / DBH))**2)**0.5',

  # Quercus suber: Scaling diff ~0.65
  veg_quercus_suber_anv_v3 =
    '((Ratio * (1 - Ratio) * 0.655 * (errDBH / DBH))**2)**0.5',

  # Salix caprea: Scaling diff ~0.37
  veg_salix_caprea_anv_v3 =
    '((Ratio * (1 - Ratio) * 0.369 * (errDBH / DBH))**2)**0.5'
)
# dbh2canopyBiomassFraction_errorPropagation2 <-  ee$Dictionary(list(
#   veg_abies_alba_anv_v3 =
#     '(2.4497 * exp(2.4497 * log(DBH) - 2.3958) -
#       (2.4497 * (exp(2.4497 * log(DBH) - 2.3958) - exp(2.5768 * log(DBH) - 3.2683)) +
#        2.5768 * exp(2.5768 * log(DBH) - 3.2683))) /
#      (DBH * exp(2.4497 * log(DBH) - 2.3958))',
#
#   veg_castanea_sativa_anv_v3 =
#     '(0.2643132 * exp(2.3812 * log(DBH) - 3.6834) + 2.3956 * exp(2.3956 * log(DBH) - 6.295) -
#       (0.111 * exp(2.3812 * log(DBH) - 3.6834) + exp(2.3956 * log(DBH) - 6.295)) *
#       (2.3129 * exp(2.3129 * log(DBH) - 2.23) + 2.3812 * exp(2.3812 * log(DBH) - 3.6834) + 2.3956 * exp(2.3956 * log(DBH) - 6.295)) /
#       (exp(2.3129 * log(DBH) - 2.23) + exp(2.3812 * log(DBH) - 3.6834) + exp(2.3956 * log(DBH) - 6.295))) /
#      (DBH * (exp(2.3129 * log(DBH) - 2.23) + exp(2.3812 * log(DBH) - 3.6834) + exp(2.3956 * log(DBH) - 6.295)))',
#
#   veg_corylus_avellana_anv_v3 =
#     '(0.393717 * exp(0.007 + 3.547 * log(DBH)) + 2.051 * exp(0.018 + 2.051 * log(DBH)) -
#       (0.111 * exp(0.007 + 3.547 * log(DBH)) + exp(0.018 + 2.051 * log(DBH))) *
#       (2.051 * exp(0.018 + 2.051 * log(DBH)) + 2.353 * exp(0.029 + 2.353 * log(DBH)) + 3.547 * exp(0.007 + 3.547 * log(DBH))) /
#       (exp(0.007 + 3.547 * log(DBH)) + exp(0.018 + 2.051 * log(DBH)) + exp(0.029 + 2.353 * log(DBH)))) /
#      (DBH * (exp(0.007 + 3.547 * log(DBH)) + exp(0.018 + 2.051 * log(DBH)) + exp(0.029 + 2.353 * log(DBH))))',
#
#   veg_fagus_sylvatica_anv_v3 =
#     '(0.3063489 * exp(2.7599 * log(DBH) - 4.322) + 1.9073 * exp(1.9073 * log(DBH) - 4.4813) -
#       (0.111 * exp(2.7599 * log(DBH) - 4.322) + exp(1.9073 * log(DBH) - 4.4813)) *
#       (1.9073 * exp(1.9073 * log(DBH) - 4.4813) + 2.1661 * exp(2.1661 * log(DBH) - 1.4487) + 2.7599 * exp(2.7599 * log(DBH) - 4.322)) /
#       (exp(1.9073 * log(DBH) - 4.4813) + exp(2.1661 * log(DBH) - 1.4487) + exp(2.7599 * log(DBH) - 4.322))) /
#      (DBH * (exp(1.9073 * log(DBH) - 4.4813) + exp(2.1661 * log(DBH) - 1.4487) + exp(2.7599 * log(DBH) - 4.322)))',
#
#   veg_olea_europaea_anv_v3 =
#     '(0.29829474 * exp(2.68734 * log(DBH) - 4.65302) + 1.53326 * exp(1.53326 * log(DBH) - 3.52781) -
#       (0.111 * exp(2.68734 * log(DBH) - 4.65302) + exp(1.53326 * log(DBH) - 3.52781)) *
#       (1.53326 * exp(1.53326 * log(DBH) - 3.52781) + 1.60779 * exp(1.60779 * log(DBH) - 1.0421) + 2.68734 * exp(2.68734 * log(DBH) - 4.65302)) /
#       (exp(1.53326 * log(DBH) - 3.52781) + exp(1.60779 * log(DBH) - 1.0421) + exp(2.68734 * log(DBH) - 4.65302))) /
#      (DBH * (exp(1.53326 * log(DBH) - 3.52781) + exp(1.60779 * log(DBH) - 1.0421) + exp(2.68734 * log(DBH) - 4.65302)))',
#
#  veg_picea_abies_anv_v3 = "(0.2440113 * exp(2.1983 * log(DBH) - 3.3163) + 1.8688 * exp(1.8688 * log(DBH) - 2.7957) - (0.111 * exp(2.1983 * log(DBH) - 3.3163) + exp(1.8688 * log(DBH) - 2.7957)) * (1.8688 * exp(1.8688 * log(DBH) - 2.7957) + 2.1983 *          exp(2.1983 * log(DBH) - 3.3163) + 2.3404 * exp(2.3404 * log(DBH) - 2.5027))/exp(1.8688 * log(DBH) - 2.7957) + exp(2.1983 * log(DBH) - 3.3163) + exp(2.3404 * log(DBH) - 2.5027))/(DBH * exp(1.8688 * log(DBH) - 2.7957) + exp(2.1983 * log(DBH) - 3.3163) + exp(2.3404 * log(DBH) - 2.5027))",
#  veg_pinus_halepensis_anv_v3 = "(2.289 - (2.13609 * exp(2.13609 * log(DBH) - 2.51839) + 2.289 * exp(2.289 * log(DBH) - 3.709))/exp(2.13609 * log(DBH) - 2.51839) + exp(2.289 * log(DBH) - 3.709)) * exp(2.289 * log(DBH) - 3.709)/(DBH *          exp(2.13609 * log(DBH) - 2.51839) + exp(2.289 * log(DBH) - 3.709))",
#  veg_pinus_nigra_anv_v3 = "(0.162948 * exp(1.468 * log(DBH) - 1.1351) + 0.8705 * exp(0.8705 * log(DBH) - 0.6105) - (0.111 * exp(1.468 * log(DBH) - 1.1351) + exp(0.8705 * log(DBH) - 0.6105)) * (0.8705 * exp(0.8705 * log(DBH) - 0.6105) + 1.468 *          exp(1.468 * log(DBH) - 1.1351) + 2.3691 * exp(2.3691 * log(DBH) - 2.5551))/exp(0.8705 * log(DBH) - 0.6105) + exp(1.468 * log(DBH) - 1.1351) + exp(2.3691 * log(DBH) - 2.5551))/(DBH * exp(0.8705 * log(DBH) - 0.6105) + exp(1.468 * log(DBH) - 1.1351) + exp(2.3691 * log(DBH) - 2.5551))",
#  veg_pinus_pinea_anv_v3 = "(1.03816992 * DBH**0.6788 + 87.58527287796 * DBH/100**3.248054 - (0.6184 * DBH**1.6788 + 2061.774 * DBH/100**4.248054) * (87.58527287796 * DBH/100**3.248054 +          DBH**0.6788 * (0.00779256 * DBH**1.5681 + 1.03816992))/2061.774 * DBH/100**4.248054 + DBH**1.6788 * (0.0024 * DBH**1.5681 + 0.6184))/2061.774 * DBH/100**4.248054 + DBH**1.6788 * (0.0024 * DBH**1.5681 + 0.6184)",
#  veg_pinus_sylvestris_anv_v3 = "(0.2397711 * exp(2.1601 * log(DBH) - 3.6641) + 1.7471 * exp(1.7471 * log(DBH) - 3.5276) - (0.111 * exp(2.1601 * log(DBH) - 3.6641) + exp(1.7471 * log(DBH) - 3.5276)) * (1.7471 * exp(1.7471 * log(DBH) - 3.5276) + 2.1601 *          exp(2.1601 * log(DBH) - 3.6641) + 2.308 * exp(2.308 * log(DBH) - 2.3583))/exp(1.7471 * log(DBH) - 3.5276) + exp(2.1601 * log(DBH) - 3.6641) + exp(2.308 * log(DBH) - 2.3583))/(DBH * exp(1.7471 * log(DBH) - 3.5276) + exp(2.1601 * log(DBH) - 3.6641) + exp(2.308 * log(DBH) - 2.3583))",
#  veg_prunus_avium_anv_v3 = "(0.2448771 * exp(2.2061 * log(DBH) - 2.6762) + 1.3212 * exp(1.3212 * log(DBH) - 4.1058) - (0.111 * exp(2.2061 * log(DBH) - 2.6762) + exp(1.3212 * log(DBH) - 4.1058)) * (1.3212 * exp(1.3212 * log(DBH) - 4.1058) + 1.9656 *          exp(1.9656 * log(DBH) - 1.0948) + 2.2061 * exp(2.2061 * log(DBH) - 2.6762))/exp(1.3212 * log(DBH) - 4.1058) + exp(1.9656 * log(DBH) - 1.0948) + exp(2.2061 * log(DBH) - 2.6762))/(DBH * exp(1.3212 * log(DBH) - 4.1058) + exp(1.9656 * log(DBH) - 1.0948) + exp(2.2061 * log(DBH) - 2.6762))",
#  veg_quercus_cerris_anv_v3 = "(0.212898 * exp(1.918 * log(DBH) - 0.599) + 1.895 * exp(1.895 * log(DBH) - 0.959) - (0.111 * exp(1.918 * log(DBH) - 0.599) + exp(1.895 * log(DBH) - 0.959)) * (1.895 * exp(1.895 * log(DBH) - 0.959) + 1.901 *          exp(1.901 * log(DBH) - 0.303) + 1.918 * exp(1.918 * log(DBH) - 0.599))/exp(1.895 * log(DBH) - 0.959) + exp(1.901 * log(DBH) - 0.303) + exp(1.918 * log(DBH) - 0.599))/(DBH * exp(1.895 * log(DBH) - 0.959) + exp(1.901 * log(DBH) - 0.303) + exp(1.918 * log(DBH) - 0.599))",
#  veg_quercus_ilex_anv_v3 = "(0.2563323 * exp(2.3093 * log(DBH) - 2.985) + 2.1018 * exp(2.1018 * log(DBH) - 4.4998) - (0.111 * exp(2.3093 * log(DBH) - 2.985) + exp(2.1018 * log(DBH) - 4.4998)) * (2.1018 * exp(2.1018 * log(DBH) - 4.4998) + 2.2686 *          exp(2.2686 * log(DBH) - 2.1809) + 2.3093 * exp(2.3093 * log(DBH) - 2.985))/exp(2.1018 * log(DBH) - 4.4998) + exp(2.2686 * log(DBH) - 2.1809) + exp(2.3093 * log(DBH) - 2.985))/(DBH * exp(2.1018 * log(DBH) - 4.4998) + exp(2.2686 * log(DBH) - 2.1809) + exp(2.3093 * log(DBH) - 2.985))",
#  veg_quercus_robur_anv_v3 = "(0.3277386 * exp(2.9526 * log(DBH) - 4.4339) + 2.1375 * exp(2.1375 * log(DBH) - 4.4663) - (0.111 * exp(2.9526 * log(DBH) - 4.4339) + exp(2.1375 * log(DBH) - 4.4663)) * (2.1375 * exp(2.1375 * log(DBH) - 4.4663) + 2.5279 *          exp(2.5279 * log(DBH) - 2.7054) + 2.9526 * exp(2.9526 * log(DBH) - 4.4339))/exp(2.1375 * log(DBH) - 4.4663) + exp(2.5279 * log(DBH) - 2.7054) + exp(2.9526 * log(DBH) - 4.4339))/(DBH * exp(2.1375 * log(DBH) - 4.4663) + exp(2.5279 * log(DBH) - 2.7054) + exp(2.9526 * log(DBH) - 4.4339))",
#  veg_quercus_suber_anv_v3 = "(0.3830372238 + 1.355957 * (exp(0.600169 + 1.355957 * log(DBH))/DBH**0.5831) - ((1.355957 * exp(0.600169 + 1.355957 * log(DBH)) +          2.011002 * exp(0.164185 + 2.011002 * log(DBH)))/DBH**0.5831 + 3.4507858) * (0.656898 * DBH**0.5831 +          exp(0.600169 + 1.355957 * log(DBH)))/5.918 * DBH**0.5831 + exp(0.164185 + 2.011002 * log(DBH)) + exp(0.600169 + 1.355957 * log(DBH)))/(DBH**0.4169 * 5.918 * DBH**0.5831 + exp(0.164185 + 2.011002 * log(DBH)) + exp(0.600169 + 1.355957 * log(DBH)))",
#  veg_salix_caprea_anv_v3 = "(0.2134974 * exp(1.9234 * log(DBH) + 4.5086) + 2.3117 * exp(1.4718 + 2.3117 * log(DBH)) - (0.111 * exp(1.9234 * log(DBH) + 4.5086) + exp(1.4718 + 2.3117 * log(DBH))) * (1.9234 *          exp(1.9234 * log(DBH) + 4.5086) + 2.3117 * exp(1.4718 + 2.3117 * log(DBH)) + 2.4987 * exp(2.4987 * log(DBH) + 4.4721))/exp(1.4718 + 2.3117 * log(DBH)) + exp(1.9234 * log(DBH) + 4.5086) + exp(2.4987 * log(DBH) + 4.4721))/(DBH * exp(1.4718 + 2.3117 * log(DBH)) + exp(1.9234 * log(DBH) + 4.5086) + exp(2.4987 * log(DBH) + 4.4721))"
#
#
#
# ) )

## treeH2treeDBH -----
# careful,
## parameters height to dbh for polynomial quadratic function -
## b0(X)**2 + b1(X) + b2 + E ;  each parameter is c(b2, b1, b0, E)
## E = Error (RMSE) of linear model
parametersPolynomialQuadratic_treeH2treeDBH <-  ee$Dictionary( list(
  veg_abies_alba_anv_v3       = c( 1.343649043,  0.687352953,  0.037239577,   4.91),
  veg_castanea_sativa_anv_v3  = c( 4.09797249,  -0.239361253,  0.0868789,     7.12),
  veg_corylus_avellana_anv_v3 = c( 0.904585844,  0.533727752,  0.036534637,   0.48),
  veg_fagus_sylvatica_anv_v3  = c( 0.864821818,  0.380177125,  0.04702699,    4.48),
  veg_olea_europaea_anv_v3    = c(-4.310682232,  3.19918107,  -0.052826581,   4.87),
  veg_picea_abies_anv_v3      = c( 1.380584301,  0.678991739,  0.035128331,   3.87),
  veg_pinus_halepensis_anv_v3 = c(-9.290716186,  3.357434352, -0.046325054,   6.83),
  veg_pinus_nigra_anv_v3      = c( 5.45609197,   1.105017037,  0.011863807,   5.94),
  veg_pinus_pinea_anv_v3      = c(-2.363414601,  2.979136129, -0.029655175,   8.74),
  veg_pinus_sylvestris_anv_v3 = c( 5.129869306, -0.055376739,  0.039545636,   4.02),
  veg_prunus_avium_anv_v3     = c(-1.074143494,  0.517291151,  0.057392221,   5.97),
  veg_quercus_cerris_anv_v3   = c( 0.673365032,  0.995395474,  0.019629626,   5.48),
  veg_quercus_ilex_anv_v3     = c( 4.782623276,  0.449189731,  0.067941183,   4.64),
  veg_quercus_robur_anv_v3    = c( 3.561031278,  0.167072658,  0.046124222,   5.68),
  veg_quercus_suber_anv_v3    = c(-2.508958732,  3.617397079,  0.012845111,   7.83),
  veg_salix_caprea_anv_v3     = c(-7.855460266,  1.451894959,  0.031738413,   6.24)
))

## namesAndDesc: Output rasters -----
namesAndDesc <- list(
  canopyHeight               = "Average tree canopy heights from ground in pixel, (m)",
  # canopyHeightSD           = "Estimation of error (sigma) (m)",  # commented out like in JS
  materassoHeight            = "Average height of canopy-only in pixel, (m)",
  materassoVolume            = "Average volume of canopy from the forest stand's canopy bottom to tree tops. In cubic meters (m3)",
  materassoVolumeRMSE        = "Estimation of error (sigma) of average volume of canopy from the forest stand's canopy bottom to tree tops. In cubic meters (m3)",
  canopyBaseHeight           = "The forest Canopy Base Height (CBH) describes the average height from the ground to a forest stand's canopy bottom. Specifically, it is the lowest height in a stand at which there is a sufficient amount of forest canopy fuel to propagate fire vertically into the canopy.",
  canopyBaseHeightRMSE       = "Estimation of error (sigma) from propagation of canopy height model errors and CBH model error",
  averageDbh                 = "Average Diameter at Base Height",
  averageDbhRMSE             = "Estimation of error (sigma) from propagation of canopy height model errors and H=>DBH model error",
  biomassOfCanopy            = "Biomas (Mg/ha) of the canopy part of AGB",
  biomassOfCanopyRMSE        = "Estimation of error (sigma) from propagation of canopy height model errors and H=>DBH model error",
  canopyBulkDensity          = "Canopy Bulk Density (CBD) is the amount of canopy biomass over canopy volume (kg/m3)",
  canopyBulkDensityRMSE      = "Estimation of error (sigma) from propagation of errors coming from the models and input variables (i.e. canopy heights, canopy base height, diameter and canopy biomass).",
  canopyBiomassFraction      = "Fraction (from 0 to 1) of canopy i.e. leaves with respect to total AGB (leaves+branches+stem). It was calculated with a species-specific model over 16 species.",
  canopyBiomassFractionRMSE  = "Propagated error to the canopy biomass fraction value from the model used for estimation."
)


## R code start -------
# List assets
assets <- ee$data$listAssets("users/cirgeo/FIRE-RES/open")

# Extract asset IDs
filt <- lapply(assets$assets, function(el) el$id)
# Filter asset IDs containing "veg"
filt2 <- Filter(function(el) grepl("veg", el), filt)

# Sum of all probability layers
sumProbs <- ee$ImageCollection(filt2)$sum()

## here I simulate the error propagation with first order taylor expansion
plotErroPropagation <- function(){
  output <- list()
  dbh2canopyBiomassFraction2 <- as.list(dbh2canopyBiomassFraction$getInfo())
  dbh2canopyBiomassFraction_errorPropagation2 <- as.list(dbh2canopyBiomassFraction_errorPropagation$getInfo())
  parametersPolynomialQuadratic_treeH2treeDBH2 <- as.list(parametersPolynomialQuadratic_treeH2treeDBH$getInfo())
  pow<-function(x,y){x^y}
  for(j in names(dbh2canopyBiomassFraction2)){
    for(height in seq(5,50, 2)){
      ch=height
      errCH=4.25

      DBH = parametersPolynomialQuadratic_treeH2treeDBH2[[j]][[3]]*ch^2 +
           ch*parametersPolynomialQuadratic_treeH2treeDBH2[[j]][[2]] +
            parametersPolynomialQuadratic_treeH2treeDBH2[[j]][[1]]

      errDBH = sqrt( errCH*(2*ch*parametersPolynomialQuadratic_treeH2treeDBH2[[j]][[3]]  +
          parametersPolynomialQuadratic_treeH2treeDBH2[[j]][[2]])^2 +
          parametersPolynomialQuadratic_treeH2treeDBH2[[j]][[4]] ^2 )

      Ratio = eval( parse(  text=dbh2canopyBiomassFraction2[[j]]  ) )
      err_Ratio = eval( parse(  text=dbh2canopyBiomassFraction_errorPropagation2[[j]]  ) )
      err_Ratio2 = eval( parse(  text=dbh2canopyBiomassFraction_errorPropagation3[[j]]  ) )

      output[[paste0(j,height)]] = list(CH=ch,
                                    CH_sigma=errCH,
                                    DBH=DBH,
                                    DBH_sigma=errDBH,
                                    ThinBiomassFraction = Ratio,
                                    ThinBiomassFraction_sigma  = err_Ratio,
                                    Species=stringr::str_to_sentence(paste( stringr::str_split(j, "_")[[1]][2:3], collapse =" "))
      )
    }
  }

  df <- data.table::rbindlist(output)

  a <- diff(range(df$ThinBiomassFraction)) / diff(range(df$DBH))
  b <- min(df$ThinBiomassFraction) - a * min(df$DBH)

  pdf(file="errorPropPlot.pdf", height=12, width=12)
   ggplot(df, aes(x=CH)) +
     geom_line(aes(y = ThinBiomassFraction, color = "Biomass fraction")) +

    geom_ribbon(
      aes(ymin = ThinBiomassFraction - ThinBiomassFraction_sigma,
          ymax = ThinBiomassFraction + ThinBiomassFraction_sigma ),
      alpha = 0.15
    ) +

     geom_line(aes(y = a * DBH + b, color = "DBH"), linetype = "dashed") +

     geom_ribbon(
       aes(ymin = a * DBH + b - a * DBH_sigma,
           ymax = a * DBH + b + a * DBH_sigma ),
       alpha = 0.15
     ) +
    facet_wrap(~ Species, scales = "free_y") +
     scale_y_continuous(
       name = "Thin biomass fraction",
       sec.axis = sec_axis(~ (. - b) / a, name = "DBH (cm)")
     ) +
    labs(
      x = "Canopy Height (m)",
      y = "Thin biomass fraction",
      title = "Thin biomass fraction and DBH estimated from Canopy Height with error propagation\nDBH is linearly rescaled for visualization"
    ) +
    theme_minimal(base_size = 13)
    dev.off()
}

# The function translated
canopyBulkDensfunction <- function(element) {

  element <- ee$Image(element)
  # element <- ee$Image(filt2[[6]])

  spname <- ee$String(element$get("system:id"))$slice(27, 73)
  # spname$getInfo()
  # ------- CBH parameters ----------
  paramsCBH <- ee$List(parametersCanopyBaseHeight$get(spname))
  slopev    <- ee$Number(paramsCBH$get(1))
  intercept <- ee$Number(paramsCBH$get(0))
  err       <- ee$Number(paramsCBH$get(2))

  # spname$getInfo()

  cbht <- canopy_height$select(0)$multiply(slopev)$add(intercept)
  cbh <- cbht$multiply(cbht$gt(0))
  ## 4.25 m is the uncertainty of the META canopy heights!
  ## it thus propagates linearly 4.1.4. We use RMSE for sigma assuming no bias
  ## Correlation with field data in https://doi.org/10.1016/j.rse.2023.113888
  cbhsd_chain <- slopev$multiply(4.25)
  cbhsd_chain <- cbhsd_chain$pow(2)$add(err$pow(2))$sqrt()

  area30mPixel3035 <- canopy_height$pixelArea()

  # --------- DBH parameters ----------
  paramsH2treeDBH <- ee$List(parametersPolynomialQuadratic_treeH2treeDBH$get(spname))
  # paramsH2treeDBH$getInfo()
  expressionFF <- ee$String(dbh2canopyBiomassFraction$get(spname))
  expressionFFerrPropag <- ee$String(dbh2canopyBiomassFraction_errorPropagation$get(spname))

  # ---------- Canopy layer thickness ----------
  materassoZ <- canopy_height$select(0)$subtract(cbh)
  materassoZ <- materassoZ$subtract(materassoZ$multiply(materassoZ$lt(0)))$float()

  materasso3d <- materassoZ$multiply(area30mPixel3035)
  materasso3d_sd <-  area30mPixel3035$multiply(cbhsd_chain)

  # ---------- Average DBH ----------
  averageDBH <- canopy_height$select(0)$pow(2)$multiply(
     ee$Number(paramsH2treeDBH$get(2))
  )$add(
    canopy_height$select(0)$multiply(
       ee$Number(paramsH2treeDBH$get(1))
    )
  )$add(
      ee$Number(paramsH2treeDBH$get(0))
  )

  averageDBH <- averageDBH$multiply(averageDBH$gt(5))

  # canopy_height <- ee$Number(30)
  averageDBH_sd <- canopy_height$select(0)$
    multiply(ee$Number(paramsH2treeDBH$get(2))$multiply(2))$
    add(ee$Number(paramsH2treeDBH$get(1)))

  averageDBH_sd <- averageDBH_sd$pow(2)$add(
    ee$Number(paramsH2treeDBH$get(3))$pow(2)
  )$sqrt()

  # averageDBH_sd$getInfo()

  averageDBH <- averageDBH$addBands(averageDBH_sd)$rename("DBH", "DBHsd")

  # Fraction functions
  Ratio <- averageDBH$expression(expressionFF, list(DBH = averageDBH$select("DBH")))


  ff_sd <- averageDBH$expression(
    expressionFFerrPropag,
    list(
      DBH = averageDBH$select("DBH"),
      sigma_DBH = averageDBH$select("DBHsd")
    )
  )

  # Biomass fractions
  biomassOfCanopyFraction <- biomass$multiply(ff)
  biomassOfCanopyFraction_sd <- biomass$multiply(ff_sd)

  # Canopy bulk density
  canopyBulkDensity <- biomassOfCanopyFraction$multiply(1000)$divide(materasso3d)

  canopyBulkDensity_sd <- (
    biomassOfCanopyFraction_sd$multiply(1000)$pow(2)$multiply(materasso3d$pow(2))
    $add(
      biomassOfCanopyFraction$multiply(1000)$pow(2)$multiply(materasso3d_sd$pow(2))
    )
  )$divide(materasso3d$pow(4))$sqrt()

  # Final band stack
  final <- canopy_height$
    addBands(list(
      materassoZ$float(),
      materasso3d$float(),
      ee$Image(materasso3d_sd$float()),
      cbh$float(),
      ee$Image(cbhsd_chain)$float(),
      averageDBH$float(),
      biomassOfCanopyFraction$float(),
      biomassOfCanopyFraction_sd$float(),
      canopyBulkDensity$float(),
      canopyBulkDensity_sd$float(),
      ff$float(),
      ff_sd$float()
    ))$
    rename(names(namesAndDesc))

  # Return weighted final image
  element$unmask()$divide(sumProbs)$multiply(final)
}


## process ----
# Map function over collection ----

# namesAndDesc()

mapped <- ee$ImageCollection(filt2)$map(canopyBulkDensfunction)

proj <- clc$projection()
projection <- proj$getInfo()

# Build CBD image
CBD <- mapped$
  sum()$
  setDefaultProjection(proj)$
  reduceResolution(
    reducer   = ee$Reducer$mean(),
    maxPixels = 256
  )

### Pilot sites list -----------
ps_list <- pilotSites$toList(pilotSites$size())
n <- pilotSites$size()$getInfo()

# Band names
bbands <- names(namesAndDesc)

# --- Loop over sites & bands ---
for (i2 in seq_len(n) - 1) {

  feat <- ee$Feature(ps_list$get(i2))
  nm <- feat$get("pilot_id")$getInfo()
  geom <- feat$geometry()

  for (b in bbands) {

    img_export <- CBD$
      unmask()$
      clip(geom)$
      select(b)$
      toFloat()

    task <- ee_image_to_drive(
      image       = img_export,
      description = paste0(nm, "_", b),
      folder      = "GEE_export",
      region      = geom,
      scale       = 30,
      crs         = "EPSG:3035",
      maxPixels   = 1e13
    )

    task$start()

  }
}
