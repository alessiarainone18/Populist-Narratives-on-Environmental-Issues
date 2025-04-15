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

# Step 2: Load and prepare articles
data <- read.csv("01-Data/random_sample_2500.csv", sep = ",", header = TRUE)
articles <- data %>%
  mutate(article_nr = 1:n(),
         content = as.character(content))

# Test sample----        
# selected_4 <- data %>%
#   mutate(id = as.character(id)) %>%
#   filter(id %in% c("36931585", "47543364", "46901385", "413604" ))
# 
# set.seed(1234)
# random_100 <- sample_n(data,100) %>%
#   mutate(id = as.character(id))
# 
# articles <- bind_rows(random_100, selected_4) %>%
#   mutate(article_nr = 1:n(),
#          content = as.character(content))


# Real sample ----


# Step 3: Initialize response vector (slow)
chatgpt_responses <- vector("character", length = nrow(articles))

exponential_backoff <- function(retries) {

  Sys.sleep(2^retries)
}

# Loop over articles
for (i in seq_len(nrow(articles))) {
  article_text <- articles$content[i]
  
  retries <- 0
  success <- FALSE
  
  while (!success && retries < 5) {  # maximum of 5 tries
    response <- tryCatch({
      httr::POST(
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
    }, error = function(e) {
      message(paste("Error in case", i, "-", conditionMessage(e)))
      NULL
    })
    
    if (!is.null(response)) {
      # Überprüfe, ob die Antwort gültig ist
      response_content <- content(response, as = "parsed")
      if (!is.null(response_content$choices) && length(response_content$choices) > 0) {
        chatgpt_responses[i] <- response_content$choices[[1]]$message$content
        success <- TRUE
      } else {
        message("Empty response in case", i)
        success <- TRUE  
      }
    }
    
    if (!success) {
      retries <- retries + 1
      message(paste("Retrying case", i, "attempt", retries))
      exponential_backoff(retries)  
    }
  }
  
  cat("Finished case", i, "\n")
  Sys.sleep(2)  # to slower analysis
}

results_df <- articles %>%
  mutate(chatgpt_response = chatgpt_responses) %>%
  select(id, chatgpt_response)

# Clean dataset
results_clean <- results_df %>%
  mutate(
    cleaned_response = chatgpt_response %>%
      str_remove_all("```.*?\\n?|```") %>%
      str_squish()
  )

results_split <- results_clean %>%
  separate(cleaned_response, into = paste0("code_", 1:6), sep = " ")

results_split %>% count(code_1)
results_split %>% count(code_2)
results_split %>% count(code_3)
results_split %>% count(code_4)
results_split %>% count(code_5)
results_split %>% count(code_6)

# Save as CSV
write_csv(results_split, "03-Output/article_analysis_results2500.csv")


