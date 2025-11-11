#!/bin/bash

input_raster="/archivio/shared/geodati/raster/CLMS_TCF_TreeDensity_RASTER_2021/CLMS_TCF_TreeDensity_RASTER_2021.tif"
output_dir="/archivio/shared/geodati/raster/CLMS_TCF_TreeDensity_RASTER_2021/tiled/"
mkdir -p "$output_dir"

# Number of tiles
nx=2   # columns
ny=2   # rows

# Get raster size
size_x=$(gdalinfo "$input_raster" | grep "Size is" | awk '{print $3}' | sed 's/,//')
size_y=$(gdalinfo "$input_raster" | grep "Size is" | awk '{print $4}')

# Compute tile size
tile_x=$((size_x / nx))
tile_y=$((size_y / ny))

echo "Raster size: $size_x x $size_y"
echo "Tile size: $tile_x x $tile_y"

for i in $(seq 0 $((nx-1))); do
    for j in $(seq 0 $((ny-1))); do
        # Compute pixel offsets
        xoff=$((i * tile_x))
        yoff=$((j * tile_y))

        # Compute width/height for edge tiles
        w=$tile_x
        h=$tile_y
        if [ $i -eq $((nx-1)) ]; then
            w=$((size_x - xoff))
        fi
        if [ $j -eq $((ny-1)) ]; then
            h=$((size_y - yoff))
        fi

        # Output filename
        out_file="$output_dir/tile_${j}_${i}.tif"

       echo "gdal_translate -srcwin $xoff $yoff $w $h \
        -co COMPRESS=DEFLATE -co PREDICTOR=2 -co TILED=YES -co BIGTIFF=YES \
          $input_raster $out_file"
    done
done | parallel -j 8

echo "DONE!"
