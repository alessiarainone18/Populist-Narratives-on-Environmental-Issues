library(stringr)
library(jsonlite)
library(dplyr)
library(httr)


setwd("/Users/alessiarainone/Desktop/Populist-Narratives-on-Environmental-Issues")


### Prompt & API Key
analysis_prompt <- read_lines("02-Scripts/02-03-Analysis_Prompt.txt") %>% paste(collapse = "\n")
articles <- read.csv("01-Data/random_sample_2000.csv", sep = ",", header = TRUE)
api_key <- trimws(readLines("00-Planning/00_02_API/OpenAI/openai_key.txt"))

openai_url <- "https://api.openai.com/v1/chat/completions"

classify_article <- function(article_text, analysis_prompt) {
  # Sicherstellen, dass analysis_prompt als Zeichenkette vorliegt
  if (!is.character(analysis_prompt)) {
    stop("analysis_prompt muss eine Zeichenkette sein!")
  }
  
  messages <- list(
    list(
      role = "system", 
      content = paste(analysis_prompt, article_text, sep = "\nHere is the article for you to code: ")
    )
  )
  
  response <- POST(
    url = openai_url,
    add_headers(
      Authorization = paste("Bearer", api_key),
      `Content-Type` = "application/json"
    ),
    body = toJSON(list(
      model = "gpt-4o",
      messages = messages,
      temperature = 0
    ), auto_unbox = TRUE)
  )
  
  result <- content(response, as = "parsed", simplifyVector = TRUE)
  return(result$choices$message$content)
}


# Run analysis ----------------------------------------------------------
classified_results <- vector("list", length = 50)

# Schleife von 1 bis 50 (nicht 0 bis 50)
for (i in 1:50) {
  article_text <- articles$content[i]
  
  classified_results[[i]] <- classify_article(article_text = article_text, analysis_prompt = analysis_prompt)
  
}


classified_results <- unlist(classified_results)
write.csv(classified_results, "classified_articles_test_50.csv", row.names = FALSE)






## New try
setwd("/Users/alessiarainone/Desktop/Populist-Narratives-on-Environmental-Issues")

# Step 1

### Prompt & API Key
analysis_prompt <- read_lines("02-Scripts/02-03-Analysis_Prompt.txt", collapse = " ")
articles <- read.csv("01-Data/random_sample_2000.csv", sep = ",", header = TRUE)
api_key <- trimws(readLines("00-Planning/00_02_API/OpenAI/openai_key.txt"))

n_cases <- 10       # Change to 2000 for actual use
round_num <- 1      # Set to 1 for the first round; 2 (or higher) for subsequent rounds


# Step 2: Select relevant variables
data <- read.csv("01-Data/random_sample_2500.csv", sep = ",", header = TRUE)
test_articles <- data %>%
  mutate(id = as.character(id)) %>%
  filter(id %in% c("36931585", "47543364", "46901385", "413604" )) %>%
  mutate(article_nr = 1:n())

test_articles <- test_articles %>%
  mutate(content=as.character(content))

test_articles_text <- test_articles$content
test_article_nr <- test_articles$article_nr


# Step 6: Initialize a vector to store ChatGPT responses
chatgpt_responses <- vector("character", length = nrow(test_articles))


  # Build the final prompt with the combined text
 
  # API call to get ChatGPT's response
  r <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    content_type("application/json"),
    add_headers(Authorization = paste("Bearer", APIkey)),
    body = list(
      model = "gpt-4o",
      temperature = 0,
      messages = list(
        list(role = "system", 
             content = paste(analysis_prompt, article_text, sep = "\nHere is the article for you to code: "))
      )
    ),
    encode = "json"
  )
  
  # Save the response or an error message
  chatgpt_responses[i] <- tryCatch({
    content(r)$choices[[1]]$message$content
  }, error = function(e) {
    paste("Error in case", i)
  })
  
  cat("Finished case", i, "\n")
}

# Step 8: Combine everything into the final results dataframe
results_df <- test_articles %>%
  mutate(
    chatgpt_response = chatgpt_responses
  ) %>%
  select(id, chatgpt_response)


# improved by ai
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
test_articles <- data %>%
  mutate(id = as.character(id)) %>%
  filter(id %in% c("36931585", "47543364", "46901385", "413604")) %>%
  mutate(article_nr = 1:n(),
         content = as.character(content))

# Step 6: Initialize vector to store responses
chatgpt_responses <- vector("character", length = nrow(test_articles))

# Step 7: Loop through articles and get ChatGPT responses
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

# Step 8: Combine responses with original data
results_df <- test_articles %>%
  mutate(chatgpt_response = chatgpt_responses) %>%
  select(id, chatgpt_response)


