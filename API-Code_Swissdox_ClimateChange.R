# Submitting a query -----------------------------------------------------------
library(jsonlite)
library(httr)
library(R.utils)
library(tidyverse)

api_key <- readLines("api_key.txt")
api_secret <- readLines("api_secret.txt") 

headers <- add_headers(
  "X-API-Key" = api_key,
  "X-API-Secret" = api_secret
)

API_URL_QUERY <- "https://swissdox.linguistik.uzh.ch/api/query"

yaml_query_2 <- "
query:
    sources:
         - ZWAO
         - SRF
         - NZZO
         - NNTA
         - BLIO
         - TLM
         - HEU
         - NNBE
         - BAZ
    dates:
        - from: 2009-01-01
          to: 2024-12-31
    languages:
        - de
        - fr
        - it
    content:
        AND:
            - OR:
                - Klima
                - Klimawandel
                - Klimakrise
                - Klimaschutz
                - Klimaerwärmung
                - Erderwärmung
                - Klimapolitik
                - Erderwärmung
                - changement climatique
                - réchauffement climatique
                - protection du climat
                - transition énergétique
                - crise climatique
                - émissions de gaz à effet de serre
                - riscaldamento globale
                - protezione del clima
                - transizione energetica
                - ondata di caldo
                - siccità
                - inondazioni
                - fonte dei ghiacciai
                - eventi meteorologici estremi
                - Hitzewelle
                - Dürren
                - Überschwemmungen
                - Gletscherschmelze
                - Wetterextreme
                - CO₂-Ausstoß
                - CO₂-Steuer
                - Emissionsreduktion
                - neutralité carbone
                - taxe carbone
                - riduzione delle emissioni
                - Swiss Climate Policy
                - Grüne Partei Schweiz
                - Klimastreik Schweiz
                - BAFU
                - OFEV
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
    query = yaml_query_2,
    name = "Query 2 Climate Change News", 
    comment = "First data collection",
    expirationDate = "2025-12-31"
  ),
  encode = "form"
)

print(content(response, "parsed"))

#Checking the status of submitted queries--------------------------------------
API_URL_STATUS <- "https://swissdox.linguistik.uzh.ch/api/status"

status_response <- GET(
  url = API_URL_STATUS,
  headers
)

status_content <- content(status_response, "text", encoding = "UTF-8")
status_json <- fromJSON(status_content)
print(status_json)


#Download of the retrieved dataset----------------------------------------------
download_url <- "https://swissdox.linguistik.uzh.ch/api/download/20079960-da17-41ee-b196-f6e6e4e4b620__2025_03_26T10_53_49.tsv.xz"
download_response <- GET(download_url, headers)

if (status_code(download_response) == 200) {
  writeBin(content(download_response, "raw"), "dataset_climate.tsv.xz")
  cat("Download complete. File saved as dataset_climate.tsv.xz\n")
} else {
  cat("Download failed:\n")
  print(content(download_response, "text"))
}


#Unzip & load data -------------------------------------------------------------
gunzip("dataset_climate.tsv.xz", destname = "dataset_climate.tsv", remove = FALSE)
data <- read.delim("dataset_climate.tsv", sep = "\t", encoding = "UTF-8")
head(data)

