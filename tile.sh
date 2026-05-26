IN=/archivio/shared/R/wildfire/CLMS_HRLVLCC_TCD_S2023.tif
OUT=/archivio/shared/R/wildfire/tcd

gdal_translate "$IN" "$OUT/CLMS_HRLVLCC_TCD_S2023_tile1.tif" -srcwin 0 0 325000 230000 -ot Byte -co COMPRESS=DEFLATE -co NUM_THREADS=4 -co BIGTIFF=YES &
gdal_translate "$IN" "$OUT/CLMS_HRLVLCC_TCD_S2023_tile2.tif" -srcwin 325000 0 325000 230000 -ot Byte -co COMPRESS=DEFLATE -co NUM_THREADS=4 -co BIGTIFF=YES &
gdal_translate "$IN" "$OUT/CLMS_HRLVLCC_TCD_S2023_tile3.tif" -srcwin 0 230000 325000 230000 -ot Byte -co COMPRESS=DEFLATE -co NUM_THREADS=4 -co BIGTIFF=YES &
gdal_translate "$IN" "$OUT/CLMS_HRLVLCC_TCD_S2023_tile4.tif" -srcwin 325000 230000 325000 230000 -ot Byte -co COMPRESS=DEFLATE -co NUM_THREADS=4 -co BIGTIFF=YES &

wait
echo "All tiles completed."
