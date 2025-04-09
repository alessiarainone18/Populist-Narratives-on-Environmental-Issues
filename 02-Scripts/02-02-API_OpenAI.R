library(tidyverse)
library(httr)
library(readr)

# Set up WD
setwd("/Users/alessiarainone/Desktop/Populist-Narratives-on-Environmental-Issues")

analysis_prompt <- read_lines("02-03-Analysis_Prompt.txt")
 
# Authentication
analysis_prompt <- read_lines("02-Scripts/02-03-Analysis_Prompt.txt") %>% paste(collapse = "\n")
APIkey <- readLines("00-Planning/00_02_API/OpenAI/openai_key.txt")

# Text generation endpoints
get_safe_code <- function(lines, index) {
  if (length(lines) < index) return(99)
  val <- suppressWarnings(as.integer(lines[index]))
  if (is.na(val)) return(99)
  return(val)
}

# Function for analysis
analyze_article <- function(article_text, prompt, APIkey) {
  response <- tryCatch({
    httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      content_type("application/json"),
      add_headers(Authorization = paste("Bearer", APIkey)),
      body = list(
        model = "gpt-4o",
        messages = list(
          list(role = "system", content = prompt),
          list(role = "user", content = article_text)
        )
      ),
      encode = "json"
    )
  }, error = function(e) {
    message("ERROR: ", e$message)
    return(NULL)
  })
  
  if (is.null(response)) return(NULL)
  
  result_text <- content(response)$choices[[1]]$message$content
  result_lines <- strsplit(result_text, "\n")[[1]]
  result_lines <- trimws(result_lines)
  
  # Extract the different variables
  relevance <- get_safe_code(result_lines, 1)
  party     <- get_safe_code(result_lines, 2)
  support   <- get_safe_code(result_lines, 3)
  discourse <- get_safe_code(result_lines, 4)
  people    <- get_safe_code(result_lines, 5)
  elite     <- get_safe_code(result_lines, 6)
  
  return(tibble(
    relevance = relevance,
    party     = party,
    support   = support,
    discourse = discourse,
    people    = people,
    elite     = elite
  ))
}

# Preparation of articles
test_articles <- test_sample %>%
  mutate(id = as.character(id)) %>%
  filter(id %in% c("45180862", "37568223", "45746754", "37790583", "38068220")) %>%
  mutate(article_nr = 1:n())

test_articles_text <- test_articles$content
test_article_nr <- test_articles$article_nr

# Save results
results <- list()

# Conduct analysis
for (i in seq_along(test_articles_text)) {
  cat("Analyze article", i, "of", length(test_articles_text), "\n")
  
  article_result <- analyze_article(test_articles_text[i], analysis_prompt, APIkey)
  
  if (!is.null(article_result)) {
    article_result$article_nr <- test_article_nr[i]
    results[[i]] <- article_result
  } else {
    results[[i]] <- tibble(
      relevance = NA,
      party     = NA,
      support   = NA,
      discourse = NA,
      people    = NA,
      elite     = NA,
      article_nr = test_article_nr[i]
    )
  }
}

# Results Table
final_results <- bind_rows(results) %>%
  print()

# Save as CSV
write_csv(final_results, "03-Output/article_analysis_results.csv")



