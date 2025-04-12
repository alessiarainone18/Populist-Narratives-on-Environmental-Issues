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

# Step 2: Load and filter articles
data <- read.csv("01-Data/random_sample_2500.csv", sep = ",", header = TRUE)
articles <- data %>%
  mutate(article_nr = 1:n(),
         content = as.character(content))

# Step 6: Initialize vector to store responses
chatgpt_responses <- vector("character", length = nrow(articles))

for (i in seq_len(nrow(articles))) {
  article_text <- articles$content[i]
  
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
    response_content <- content(response, as = "parsed")
    if (!is.null(response_content$choices) && length(response_content$choices) > 0) {
      response_content$choices[[1]]$message$content
    } else {
      paste("Empty response in case", i)
    }
  }, error = function(e) {
    paste("Error in case", i, "-", conditionMessage(e))
  })
  
  cat("Finished case", i, "\n")
}

# Step 8: Combine responses with original data
results_df <- articles %>%
  mutate(chatgpt_response = chatgpt_responses) %>%
  select(id, chatgpt_response)

# Entferne Markdown-Formatierung und überflüssige Whitespaces
results_clean <- results_df %>%
  mutate(
    cleaned_response = chatgpt_response %>%
      str_remove_all("```.*?\\n?|```") %>%
      str_squish()
  )

# Teile in einzelne Spalten (angenommen 6 Codes pro Artikel)
results_split <- results_clean %>%
  separate(cleaned_response, into = paste0("code_", 1:6), sep = " ")

results_split %>% count(code_1)