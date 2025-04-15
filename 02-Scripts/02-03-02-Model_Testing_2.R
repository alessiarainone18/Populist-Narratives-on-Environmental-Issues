# Set working directory
setwd("/Users/alessiarainone/Desktop/Populist-Narratives-on-Environmental-Issues")

# Load necessary libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(readr)

# Step 1: Load prompt and API key
analysis_prompt <- read_lines("02-Scripts/02-03-Analysis_Prompt.txt") %>% paste(collapse = " ")
api_key <- trimws(readLines("00-Planning/00_02_API/OpenAI/openai_key.txt"))

# Step 2: Load and filter articles (selected articles that are relevant and in populistic rhetoric)
data <- read.csv("01-Data/random_sample_2500.csv", sep = ",", header = TRUE)
test_articles <- data %>%
  mutate(id = as.character(id)) %>%
  filter(id %in% c("36931585", "47543364", "46901385", "413604")) %>%
  mutate(article_nr = 1:n(),
         content = as.character(content))

# Step 3: Initialize vector to store responses
chatgpt_responses <- vector("character", length = nrow(test_articles))

# Step 4: Loop through articles and get ChatGPT responses
for (i in seq_len(nrow(test_articles))) {
  article_text <- test_articles$content[i]
  
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    content_type("application/json"),
    add_headers(Authorization = paste("Bearer", api_key)),
    body = list(
      model = "gpt-4o",
      temperature = 0,
      messages = list(
        list(role = "system", 
             content = paste(analysis_prompt, "\nHere is the article for you to code:", article_text))
      )
    ),
    encode = "json"
  )
  
  chatgpt_responses[i] <- tryCatch({
    content(response, as = "parsed")$choices[[1]]$message$content
  }, error = function(e) {
    paste("Error in case", i)
  })
  
  cat("Finished case", i, "\n")
}

# Step 5: Combine responses with original data
results_df <- test_articles %>%
  mutate(chatgpt_response = chatgpt_responses) %>%
  select(id, chatgpt_response)


