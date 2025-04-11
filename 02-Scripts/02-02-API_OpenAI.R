library(tidyverse)
library(httr)
library(readr)

# Set up WD
setwd("/Users/alessiarainone/Desktop/Populist-Narratives-on-Environmental-Issues")

analysis_prompt <- read_lines("02-Scripts/02-03-Analysis_Prompt.txt")
 
### Prompt & API Key
analysis_prompt <- read_lines("02-Scripts/02-03-Analysis_Prompt.txt") %>% paste(collapse = "\n")
APIkey <- readLines("00-Planning/00_02_API/OpenAI/openai_key.txt")

## Function to safely extract a numeric code from a line
get_safe_code <- function(lines, index) {
  if (length(lines) < index) return(99)
  val <- suppressWarnings(as.integer(lines[index]))
  if (is.na(val)) return(99)
  return(val)
}

## Function for analyzing a single article
analyze_article <- function(article_text, prompt, APIkey) {
  response <- tryCatch({
    httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      httr::content_type("application/json"),
      httr::add_headers(Authorization = paste("Bearer", APIkey)),
      body = list(
        model = "gpt-4o",
        temperature = 0, 
        messages = list(
          list(role = "system", content = prompt),
          list(role = "user", content = article_text)
        )
      ),
      encode = "json"
    )
  }, error = function(e) {
    message("ERROR during API call: ", e$message)
    return(NULL)
  })
  
  if (is.null(response)) {
    warning("No response received from API.")
    return(tibble(
      relevance = 99,
      party     = 99,
      support   = 99,
      discourse = 99,
      people    = 99,
      elite     = 99
    ))
  }
  
  result_content <- content(response, as = "parsed", type = "application/json")
  
  if (is.null(result_content$choices) || 
      is.null(result_content$choices[[1]]$message$content)) {
    warning("Unexpected API response format or missing content.")
    return(tibble(
      relevance = 99,
      party     = 99,
      support   = 99,
      discourse = 99,
      people    = 99,
      elite     = 99
    ))
  }
  
  result_text <- result_content$choices[[1]]$message$content
  
  result_lines <- strsplit(result_text, "\n")[[1]]
  result_lines <- trimws(result_lines)
  
  return(tibble(
    relevance = get_safe_code(result_lines, 1),
    party     = get_safe_code(result_lines, 2),
    support   = get_safe_code(result_lines, 3),
    discourse = get_safe_code(result_lines, 4),
    people    = get_safe_code(result_lines, 5),
    elite     = get_safe_code(result_lines, 6)
  ))
}

# Preparation of articles 
data <- read.csv("01-Data/random_sample_2000.csv", sep = ",", header = TRUE)
articles <- data %>%
  mutate(article_nr = 1:n())

articles <- articles %>%
  mutate(content=as.character(content))

articles_text <- articles$content
article_nr <- articles$article_nr

# Save results
results <- list()

# Conduct analysis
for (i in seq_along(articles_text)) {
  cat("🔎 Analyse article", i, "from", length(articles_text), "\n")
  
  article_result <- analyze_article(articles_text[i], analysis_prompt, APIkey)
  
  if (!is.null(article_result)) {
    article_result$article_nr <- article_nr[i]
    results[[i]] <- article_result
  } else {
    results[[i]] <- tibble(
      relevance = NA,
      party     = NA,
      support   = NA,
      discourse = NA,
      people    = NA,
      elite     = NA,
      article_nr = article_nr[i]
    )
  }
}

# Results Table
final_results <- bind_rows(results) %>%
  print()

# Save as CSV
write_csv(final_results, "03-Output/article_analysis_results_2000.csv")

# Look at results
final_results %>%
  count(relevance)
final_results %>%
  count(party) 

final_results %>%
  count(support) 

final_results %>%
  count(discourse)

final_results %>%
  count(people) 

final_results %>%
  count(elite)

# IT FAILED!

