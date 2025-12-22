import os
import subprocess

# Path to your TIFF tiles
tif_folder = "/archivio/shared/geodati/raster/CLMS_CLCplus_RASTER_2023/TIFFs"
asset_root = "projects/progetto-eu-h2020-cirgeo/assets/copernicus/CLCplus_2023"  # GEE asset folder

# Create the asset folder first if needed
subprocess.run([
    "earthengine", "create", "folder", asset_root
])

# Loop through TIFF files and upload each
for tif in os.listdir(tif_folder):
    if tif.endswith(".tif"):
        tif_path = os.path.join(tif_folder, tif)
        asset_id = f"{asset_root}/{os.path.splitext(tif)[0]}"
        cmd = [
            "earthengine", "upload", "image",
            "--asset_id", asset_id,
            "--pyramiding_policy", "mode",
            "--crs", "EPSG:3035",
            tif_path
        ]
        print("Uploading:", tif)
        subprocess.run(cmd)
        break
