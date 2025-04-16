# Set working directory
setwd("/Users/alessiarainone/Desktop/Populist-Narratives-on-Environmental-Issues")

# Load necessary libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(readr)

# Step 1: Load prompt and API key
analysis_prompt <- read_lines("02-Scripts/02-02-Analysis_Prompt.txt") %>% paste(collapse = " ")
api_key <- trimws(readLines("00-Planning/00_02_API/OpenAI/openai_key.txt"))

# Step 2: Load and prepare articles

# Real sample -------------------------------------------------------------
data <- read.csv("01-Data/random_sample_2500.csv", sep = ",", header = TRUE)

filtered_data <- data %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(str_detect(content, regex(paste(party_keywords, collapse = "|"), ignore_case = TRUE))) %>%
  mutate(wordcount = str_count(content, "\\S+")) %>%
  filter(wordcount >= 300, wordcount <= 2000, year >= 2014) %>%
  filter(!str_detect(rubric, regex("international", ignore_case = TRUE))) 

articles <- filtered_data %>%
  mutate(article_nr = 1:n(),
         content = as.character(content))

# Test sample----        
# selected_4 <- data %>%
#   mutate(id = as.character(id)) %>%
#   filter(id %in% c("36931585", "47543364", "46901385", "413604" ))
# 
# set.seed(1234)
# random_100 <- sample_n(filtered_data, 100) %>%
#   mutate(id = as.character(id))
# 
# articles <- bind_rows(random_100, selected_4) %>%
#   mutate(article_nr = 1:n(),
#          content = as.character(content))
# 

# Error sample ------------------------------------------------------------
# Run error sample again to see if it still doesn't work
data <- read.csv("03-Output/03-03-Data_Errors.csv", sep = ",", header = TRUE)
articles <- data %>%
  select(-chatgpt_response, -relevance, -party, -support, -discourse, - elite, -people) %>%
  mutate(article_nr = 1:n(),
  content = as.character(content))

# Analysis ----
# Step 3: Initialize response vector (slow)
chatgpt_responses <- vector("character", length = nrow(articles))

# Backoff function
exponential_backoff <- function(retries) {
  wait_time <- 2^retries
  message(paste("Waiting", wait_time, "seconds before retry..."))
  Sys.sleep(wait_time)
}

# Loop over articles
for (i in seq_len(nrow(articles))) {
  article_text <- articles$content[i]
  
  retries <- 0
  success <- FALSE
  
  while (!success && retries < 3) {  # max 3 tries
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
      response_content <- content(response, as = "parsed")
      if (!is.null(response_content$choices) && length(response_content$choices) > 0) {
        chatgpt_responses[i] <- response_content$choices[[1]]$message$content
        success <- TRUE
      } else {
        message("Empty response in case", i)
        success <- TRUE  # Optional: treat as success
      }
    }
    
    if (!success) {
      retries <- retries + 1
      message(paste("Retrying case", i, "attempt", retries))
      exponential_backoff(retries)  
    }
  }
  
  # Only delay if a valid result was received
  if (success) {
    Sys.sleep(2)
  }
  
  cat("Finished case", i, "\n")
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
write_csv(results_split, "03-Output/03-01-article_analysis_results_ERRORS.csv")

# Error analysis
data_output <- results_split
data_input <- read.csv("01-Data/random_sample_2500.csv", sep = ",", header = TRUE)

# Variable coding for data_output form Open AI's analysis
data_output_renamed <- data_output %>% 
  rename("relevance"="code_1", "party"="code_2", "support"="code_3", "discourse"="code_4",
         "elite"="code_5", "people"="code_6") %>%
  mutate(article_nr = 1:n())

data_input <- data_input %>%
  mutate(article_nr = 1:n())

# Merge
data_combined <- data_input %>% 
  inner_join(data_output_renamed, by = "article_nr")

errors <- data_combined %>%
  filter(is.na(party)) 


# 492 errors. But why?
# Comparison of classified cases vs. errors
# Summarize errors
summary_errors <- errors %>%
  summarise(
    `Mean Word Count` = mean(wordcount, na.rm = TRUE),
    `Mean Character Count` = mean(char_count, na.rm = TRUE),
    `Articles < 800 Words` = sum(wordcount < 800, na.rm = TRUE),
    `Articles ≥ 800 Words` = sum(wordcount >= 800, na.rm = TRUE),
    `Articles < 1000 Words` = sum(wordcount < 1000, na.rm = TRUE),
    `Articles ≥ 1000 Words` = sum(wordcount >= 1000, na.rm = TRUE),
    `Articles Before 2020` = sum(year < 2020, na.rm = TRUE),
    `Articles From 2020 Onward` = sum(year >= 2020, na.rm = TRUE),
    `Mean Article Number` = mean(article_nr, na.rm = TRUE)
  ) %>%
  t() %>%
  as.data.frame()

# Summarize valid articles
summary_correct <- data_combined %>%
  filter(!is.na(relevance)) %>%
  summarise(
    `Mean Word Count` = mean(wordcount, na.rm = TRUE),
    `Mean Character Count` = mean(char_count, na.rm = TRUE),
    `Articles < 800 Words` = sum(wordcount < 800, na.rm = TRUE),
    `Articles ≥ 800 Words` = sum(wordcount >= 800, na.rm = TRUE),
    `Articles < 1000 Words` = sum(wordcount < 1000, na.rm = TRUE),
    `Articles ≥ 1000 Words` = sum(wordcount >= 1000, na.rm = TRUE),
    `Articles Before 2020` = sum(year < 2020, na.rm = TRUE),
    `Articles From 2020 Onward` = sum(year >= 2020, na.rm = TRUE),
    `Mean Article Number` = mean(article_nr, na.rm = TRUE)
  ) %>%
  t() %>%
  as.data.frame()

# Combine summaries into one comparison table
comparison_table <- cbind(summary_errors, summary_correct)
colnames(comparison_table) <- c("Errors", "Valid Articles")
comparison_table <- tibble::rownames_to_column(comparison_table, "Metric")

# Display with gt
comparison_table %>%
  gt() %>%
  tab_header(
    title = "Comparison of Article Statistics: Errors vs. Valid Articles"
  )
