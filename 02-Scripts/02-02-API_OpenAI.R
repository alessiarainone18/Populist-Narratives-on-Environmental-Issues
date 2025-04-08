library(tidyverse)
library(httr)
library(readr)

# Set up WD
setwd("/Users/alessiarainone/Desktop/Populist-Narratives-on-Environmental-Issues")

analysis_prompt <- read_lines("02-03-Analysis_Prompt.txt")
 
# Authentication
APIkey <- readLines("openai_key.txt")   # place your API key in a .txt file
bearer <- stringr::str_c("Authorization: Bearer ", APIkey)
articles <- 

# Text generation endpoints
r <- httr::POST(
  url = "https://api.openai.com/v1/chat/completions", 
  content_type("application/json"), 
  add_headers(Authorization = paste("Bearer", APIkey, sep = " ")), 
  body = list(
    model = "gpt-4o", 
    # messages is a list of lists
    messages = list(
      list(role = "system", 
           content = analysis_prompt))
  ), 
  encode = "json"
)
content(r)

cat(content(r)$choices[[1]]$message$content)


