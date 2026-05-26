library(hdar)
library(jsonlite) # Used to convert your JSON text into an R list

# 1. Initialize the client and authenticate
# (Replace with your actual WEkEO username and password)
user <- "fpirotti"
password <- "*******************************"

client <- Client$new(user = user, password = password, save_credentials = TRUE)

# 2. Check your connection status
print(client$token()) # If successful, this will output your active session token

# 3. Accept Copernicus Terms and Conditions (Required for downloads)
# This command automatically accepts all relevant T&Cs for the datasets
client$terms_and_conditions(accept_all = TRUE)


# 4. Paste your WEkEO JSON query structure here
json_query_string <- '{
  "dataset_id": "EO:EEA:DAT:HRL:TCF",
  "productType": "Tree Cover Density",
  "resolution": "10m",
  "year": "2023",
  "itemsPerPage": 200,
  "startIndex": 0
}'

# Convert the raw JSON text string into a list structure that R understands
query_list <- fromJSON(json_query_string, simplifyVector = FALSE)


# 5. Search the catalog for matches
# print("Searching WEkEO catalog...")
# matches <- client$search(json_query_string)
# match <- matches
# matches <- list(TCD=match)
# save(matches, file="matches.rda")
load("matches.rda")
# Print out data metrics found
cat("Total files found: ", matches[[1]]$total_count, "\n")
cat("Total download size: ", matches[[1]]$total_size, " bytes\n")


# 6. Execute the download
# Define your local output directory
output_dir <- "./wekeo_downloads"

print("Starting download...")
existing_files <- list.files(output_dir)
df <- sapply(matches[[1]]$results, function(x){
  print(x$id)
  nchar(x[["id"]])<48||sprintf("%s.zip",x$id)%in%existing_files })

matches[[1]]$results <- matches[[1]]$results[!df]
length(matches[[1]]$results)
matches[[1]]$download(output_dir = output_dir, force = FALSE)
# Note: force = FALSE skips files you have already downloaded if the script restarts

print("All downloads completed successfully!")








download_dir <- "./wekeo_downloads"

# 1. Find all zip files in your directory
zip_files <- list.files(download_dir, pattern = "\\.zip$", full.names = TRUE)

# 2. Loop through and unzip them
for (zip_file in zip_files) {
  # Create a target folder name based on the zip file name (minus the .zip extension)
  output_folder <- "./wekeo_downloads"#
  fn <- tools::file_path_sans_ext(zip_file)

  if (!file.exists(fn)) {
    message(paste("📦 Unzipping:", basename(zip_file)))
    unzip(zip_file, exdir = output_folder)
  } else {
    message(paste("⏩ Already unzipped:", basename(zip_file)))
  }
}
