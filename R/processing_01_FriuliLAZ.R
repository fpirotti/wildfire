library(lidR)
library(sf)
library(dplyr)
pilotSiteFriuli <- sf::read_sf("data/pilotSitesWildfire.gpkg") |> dplyr::filter( grepl("IT", pilot_id))
# ctg <- lidR::readLAScatalog("/archivio/shared/geodati/LAS/FVG/FVGlaz/")
# areas <-sf::st_area( ctg@data$geometry )
# lidR::crs(ctg) <- 3045
#
pilotSiteFriuli <- pilotSiteFriuli|> sf::st_transform(3045)
#
# md <- median(areas)
# iq <- IQR(areas)
# outliers <- which(as.numeric(areas) > as.numeric(md)+2*iq)

ctg.sf <- sf::read_sf("data/overlapFVGtiles.gpkg")
tiles <- sf::st_intersection(ctg.sf, pilotSiteFriuli  )
num <- list.files(dirname(tiles$filename[[1]]))
cc=0
for(file in tiles$filename){
  cc = cc +1
  message(cc, " /", nrow(tiles) )
  if(file.exists(file)){
    message(file, " exists,,,")
    next
  }
  download.file( paste0("https://www.cirgeo.unipd.it", file), file )
}

<<<<<<< HEAD
ctg.sf <- sf::st_as_sf(ctg)
tiles <- sf::st_intersection(ctg.sf, pilotSiteFriuli$geometry )
=======

  tiles.spl <- split(tiles$filename, as.factor(tiles$pilot_id))
>>>>>>> 395da61 (c)

write_sf(tiles |> select(filename), "overlapFVGtiles.gpkg")
