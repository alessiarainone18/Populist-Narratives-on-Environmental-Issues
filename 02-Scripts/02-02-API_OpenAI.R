library(tidyverse)
library(httr)
library(readr)

# Set up WD
setwd("/Users/alessiarainone/Desktop/Data-Mining-Project_Climate-Change-Media-Attention/00-Planning/API/OpenAI")

analysis_prompt <- read_lines("analysis_prompt.txt")
 
# Authentication
APIkey <- readLines("openai_key.txt")   # place your API key in a .txt file
bearer <- stringr::str_c("Authorization: Bearer ", APIkey)

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


