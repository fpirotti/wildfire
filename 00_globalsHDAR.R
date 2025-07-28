library(httr2)

source("00_globals.R")


hdar.download <- function(access_token,
                          download_url,
                          output_file ){

  # Build request
  req <- request(download_url) |>
    req_method("GET") |>
    req_headers(
      "Authorization" = paste("Bearer", access_token),
      "Accept" = "application/json"
    )

  # Perform request with error handling and file download
  tryCatch({
    # Download directly to file

    req_perform(req, path = output_file) |>
      resp_check_status()  # throws error if not 200-level

    message_log("✅ Download successful: ", output_file)
  }, error = function(e) {
    warning_log("❌ Download failed: ", e$message)
  })
}

hdar.getId <- function(token,
                  dataset_id,
                  product_id,
                  location){
  # Replace with your actual token
  access_token <- token
  message_log("Getting id of item: ",product_id)
  # Build the request
  req <- request("https://gateway.prod.wekeo2.eu/hda-broker/api/v1/dataaccess/download") |>
    req_method("POST") |>
    req_headers(
      "Authorization" = paste("Bearer", access_token),
      "Accept" = "application/json",
      "Content-Type" = "application/json"
    ) |>
    req_body_json(list(
      cacheable = TRUE,
      searchMetadata = "string",
      dataset_id = dataset_id,
      product_id = product_id,
      location = location
    ))

  # Perform the request and handle errors
  resp <- tryCatch({
    resp <- req_perform(req)
    status <- resp_status(resp)
    if (status != 200 && status != 201) {
      warning_log("Request failed with status: ", status, " - ", resp_status_desc(resp))
      warning_log("Body:\n", resp_body_string(resp))
      stop("Non-200 HTTP response")
    }
    resp
  }, error = function(e) {
    warning_log("Error performing request: ", e$message)
    NULL
  })

  # Optional: parse body if successful
  if (!is.null(resp)) {
    json_resp <- resp_body_json(resp)
    hdar.download
    return(json_resp)
  }

}


