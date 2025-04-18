# Submitting a query -----------------------------------------------------------
library(jsonlite)
library(httr)
library(R.utils)
library(tidyverse)

# Setting working directory
setwd("/Users/alessiarainone/Desktop/Populist-Narratives-on-Environmental-Issues/00-Planning/API/Swissdox")

# API Key
api_key <- readLines("api_key.txt")
api_secret <- readLines("api_secret.txt") 

headers <- add_headers(
  "X-API-Key" = api_key,
  "X-API-Secret" = api_secret
)

# Query
API_URL_QUERY <- "https://swissdox.linguistik.uzh.ch/api/query"

yaml_query_3 <- "
query:
    sources:
         - ZWAO
         - SRF
         - NZZO
         - NNTA
         - BLIO
    dates:
        - from: 2009-01-01
          to: 2024-12-31
    languages:
        - de
    content:
        AND:
            - OR:
              - Umwelt
              - Umweltschutz
              - Umweltpolitik
              - Nachhaltigkeit
              - Ökologie
              - Umweltkatastrophe
              - Ökosystem
              - Naturzerstörung

              - Klimawandel
              - Klimakrise
              - Klimaschutz
              - Klimapolitik
              - Globale Erwärmung
              - CO2-Ausstoß
              - Energiewende

              - Luftverschmutzung
              - Wasserverschmutzung
              - Plastikmüll
              - Mikroplastik
              - Abholzung
              - Bodenverschmutzung
              - Müllproblematik
              - Industrieverschmutzung

              - Artensterben
              - Biodiversität
              - Naturschutz
              - Regenwaldzerstörung
              - Artenschutz

              - Ökodiktatur
              - Klimahysterie
              - Umweltlüge
              - Verbotspartei
              - Grüne Bevormundung
              - Umweltterrorismus
              - Öko-Marxismus

result:
    format: TSV
    maxResults: 500000
    columns:
        - id
        - pubtime
        - medium_code
        - medium_name
        - rubric
        - regional
        - doctype
        - doctype_description
        - language
        - char_count
        - dateline
        - head
        - subhead
        - content_id
        - content
version: 1.2
"

response <- POST(
  url = API_URL_QUERY,
  headers,
  body = list(
    query = yaml_query_3,
    name = "Query 3 Environmental Problems", 
    comment = "Second data collection",
    expirationDate = "2025-12-31"
  ),
  encode = "form"
)

print(content(response, "parsed"))

# Checking the status of submitted queries--------------------------------------
API_URL_STATUS <- "https://swissdox.linguistik.uzh.ch/api/status"

status_response <- GET(
  url = API_URL_STATUS,
  headers
)

status_content <- content(status_response, "text", encoding = "UTF-8")
status_json <- fromJSON(status_content)
print(status_json)


# Download of the retrieved dataset----------------------------------------------
download_url <- "https://swissdox.linguistik.uzh.ch/api/download/83d1c6b0-fccf-4ec4-ba13-6fb2a514e66b__2025_04_01T13_29_37.tsv.xz"
download_response <- GET(download_url, headers)

if (status_code(download_response) == 200) {
  writeBin(content(download_response, "raw"), "dataset_environmentalpolitics.tsv.xz")
  cat("Download complete. File saved as dataset_environmentalpolitics.tsv.xz\n")
} else {
  cat("Download failed:\n")
  print(content(download_response, "text"))
}



# Unzip & load data -------------------------------------------------------------
gunzip("dataset_environmentalpolitics.tsv.xz", destname = "dataset_environmentalpolitics.tsv", remove = FALSE)
data <- read.delim("dataset_environmentalpolitics.tsv", sep = "\t", encoding = "UTF-8")
head(data)

