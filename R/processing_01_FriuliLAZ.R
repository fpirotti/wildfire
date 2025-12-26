library(lidR)
library(sf)
library(dplyr)
pilotSiteFriuli <- sf::read_sf("data/pilotSites_v3.shp") |> dplyr::filter( grepl("IT", pilot_id))
ctg <- lidR::readLAScatalog("/archivio/shared/geodati/LAS/FVG/FVGlaz/")
areas <-sf::st_area( ctg@data$geometry )
lidR::crs(ctg) <- 3045

pilotSiteFriuli <- pilotSiteFriuli|> sf::st_transform(3045)

md <- median(areas)
iq <- IQR(areas)
outliers <- which(as.numeric(areas) > as.numeric(md)+2*iq)

ctg.sf <- sf::st_as_sf(ctg)
tiles <- sf::st_intersection(ctg.sf, pilotSiteFriuli$geometry[1,] )

