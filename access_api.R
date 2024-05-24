library(tidyverse)
library(httr2)

# Assuming we have the necessary details for authentication
auth_url <- "weR_AUTHENTICATION_SERVER_URL"
client_id <- "weR_CLIENT_ID"
client_secret <- "weR_CLIENT_SECRET"
grant_type <- "client_credentials" # Common for client credentials flow

# Function to obtain access token
get_access_token <- function(auth_url, client_id, client_secret, grant_type) {
  response <- request(auth_url) %>%
    req_body_form(
      client_id = client_id,
      client_secret = client_secret,
      grant_type = grant_type
    ) %>%
    req_perform()

  # Parse the response to extract the access token
  parsed_response <- response %>% resp_body_json()
  access_token <- parsed_response$access_token
  return(access_token)
}

# Function to make an authenticated request to the Arealdata API
get_arealdata <- function(base_url, endpoint, access_token) {
  response <- request(base_url) %>%
    req_url_path(endpoint) %>%
    req_headers(Authorization = paste("Bearer", access_token)) %>%
    req_perform()

  # Check response status and content type
  if (resp_status(response) == 200) {
    content_type <- resp_header(response, "content-type")
    if (str_detect(content_type, "html")) {
      cat("Response is HTML. Manual inspection may be required.\n")
    } else if (str_detect(content_type, "csv")) {
      csv_content <- response %>% resp_body_string()
      data <- read_csv(csv_content)
      glimpse(data) # Get a quick overview of the dataset structure
    } else {
      cat("Unexpected content type. Further investigation needed.\n")
    }
  } else {
    cat("Failed to retrieve data. Status code:", resp_status(response), "\n")
  }
}

# Set the base URL and endpoint for the Arealdata API request
arealdata_base_url <- "https://arealdata.miljoeportal.dk"
endpoint <- "datasets/urn:dmp:ds:arter-resultat"

# Obtain access token
access_token <- get_access_token(auth_url, client_id, client_secret, grant_type)

# Make authenticated request to the Arealdata API
get_arealdata(arealdata_base_url, endpoint, access_token)
