
################################################################################
################################################################################
################################################################################

path.CLC <- list.files(rootPathCLC, full.names = T)
path.CLCconf <- list.files(rootPathCLCconf, full.names = T)

if(length(path.CLC)!=length(path.CLCconf)){
  message("N. of TIFFS for CLC and confidence layer do not match; ",
  sprintf("%s%s",
          substr(basename(path.CLCconf),0,30)[[1]],
          setdiff(substr(basename(path.CLC), 28,405),substr(basename(path.CLCconf),30,407))
          )
  )
}


#########################################################
######################## LOAD TRAINING ##################
#########################################################
## training data -----
### from CzechGlobe ----

train.czg <- terra::rast("validation/WildfireCE_Fuel_map_validation_DE-CZ_CzechGlobe_model.tif")
train.czg.values <- sf::st_as_sf(as.data.frame(train.czg,  xy=TRUE, na.rm=T, wide=T ), coords=c("x","y"),
                                 crs=terra::crs(train.czg))

valid.czg <- terra::rast("validation/WildfireCE_Fuel_map_validation_AT-CZ_CzechGlobe_model.tif")
valid.czg.values <- sf::st_as_sf(as.data.frame(valid.czg,  xy=TRUE, na.rm=T, wide=T ), coords=c("x","y"),
                                 crs=terra::crs(valid.czg))

### from BOKU ----
train.boku <- terra::rast("validation/carinthia_forest_fuel_3416.tif")
train.boku2 <- terra::aggregate(train.boku, fact=3, fun="mean")
train.boku2[] <- ifel(train.boku2 %% 1 == 0, train.boku2, NA)
train.boku.values.sf <- sf::st_as_sf(as.data.frame(train.boku2,
                                                   xy=TRUE, na.rm=T,
                                                   wide=T ),
                                     coords=c("x","y"),
                                 crs=terra::crs(train.boku2)
                                 )

train.bokuClean <- terra::extract(train.boku, train.boku.values.sf, ID=F, mat=F)
train.boku.values.sf$match <- train.boku.values.sf[,1][[1]] == train.bokuClean[,1]
train.boku.values.sf <- train.boku.values.sf |> dplyr::filter(match)

# table(sf::st_drop_geometry(train.boku.values)[,1])


# table(sf::st_drop_geometry(valid.czg.values)[,1])

#########################################################
#########################################################
file.remove(sprintf("%s/000_tileindex.gpkg",rootPathCLC))
system( sprintf("gdaltindex %s/000_tileindex.gpkg   %s/*tif",
                rootPathCLC, rootPathCLC) )
file.remove(sprintf("%s/000_tileindex.gpkg",rootPathCLCconf))
system( sprintf("gdaltindex %s/000_tileindex.gpkg   %s/*tif",
                rootPathCLCconf, rootPathCLCconf) )
#########################################################
#########################################################
bounds.CLC <- sf::read_sf(sprintf("%s/000_tileindex.gpkg",
                                  rootPathCLC))
filtered.CLC <- st_filter(bounds.CLC, geometry |>
                            st_transform(st_crs(bounds.CLC)), .predicate = st_intersects)
bounds.CLCconf <- sf::read_sf(sprintf("%s/000_tileindex.gpkg",
                                  rootPathCLCconf))
filtered.CLCconf <- st_filter(bounds.CLCconf, geometry |>
                            st_transform(st_crs(bounds.CLC)), .predicate = st_intersects)


clcTilePath <- filtered.CLC[1,"location"]$location
clcConfTilePath <- filtered.CLCconf[1,"location"]$location

clcTile <- rast(clcTilePath)
tbl <- terra::coltab(clcTile)

#
#
# processTile<-function(clcTilePath, clcConfTilePath){
#
#
  clcTile <- rast(clcTilePath)
  clcTileConf <- rast(clcConfTilePath)
  fuelModelTile <- rast(clcTile)
  fuelModelConfTile <- rast(clcTileConf)
  plot(clcTile)
  fuelModelTile <- setValues(fuelModelTile, 98L)
#
  clc_v <- terra::values(clcTile, mat=FALSE)
  # table(clc_v)
  clc_v_conf <- terra::values(clcTileConf, mat=FALSE)
#
  fuel <- integer(length(clc_v))
#   fuel[] <- 98L
#
  fuel[clc_v == 1]  <- 91     # Urban
  fuel[clc_v == 10] <- 98     # Water
  fuel[clc_v > 250] <- 98     # Water
  fuel[clc_v == 11] <- 92     # Snow
  fuel[clc_v %in% c(8,9)] <- 99   # Bare

  ## includes agriculture
  mask_grass_or_grassshrub <- which(clc_v %in% c(6,7))
  mask_shrub <- which(clc_v == 5)
  mask_forest <- which(clc_v %in% c(2,3,4))
#
#   ## grab CH
#   e <- terra::ext(clcTile)
#   args <- c(
#     "-multi",
#     "-wo", "NUM_THREADS=ALL_CPUS",
#     "-t_srs", "EPSG:3035",
#     "-tr", "10", "10",
#     "-tap",
#     "-te",
#     e$xmin, e$ymin, e$xmax, e$ymax,
#     "source.tif",
#     "out.tif"
#   )
#
#
#   fuelModelTile[] <- fuel
#
# }
#
# # initialize output
#
