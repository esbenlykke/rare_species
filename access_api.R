library(tidyverse)
library(httr2)

# Load environment variables if needed
# dotenv::load_dot_env()

# Set the base URL and endpoint for the Arealdata API request
arealdata_base_url <- "https://arealdata.miljoeportal.dk" 
endpoint <- "datasets/urn:dmp:ds:arter-resultat" 

# Make a request to the Arealdata API to access the "arter-resultat" dataset
response <- 
  request(arealdata_base_url) %>% 
  req_url_path(endpoint) %>%
  req_headers(Accept = "application/json") %>%
  req_perform()

# Inspect the response status and content type
response

# Depending on the content type, handle the response accordingly
# If the content type is HTML, you might need to inspect the HTML or manually download the data
# If it's CSV, you can read the CSV data directly
if (str_detect(resp_header(response, "content-type"), "html")) {
  cat("Response is HTML. Manual inspection may be required.\n")
} else if (str_detect(resp_header(response, "content-type"), "csv")) {
  csv_content <- response %>% resp_body_string()
  data <- read_csv(csv_content)
  glimpse(data)  # Get a quick overview of the dataset structure
} else {
  cat("Unexpected content type. Further investigation needed.\n")
}
