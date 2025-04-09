library(tidyverse)
library(httr)
library(readr)
library(stringr)
library(jsonlite)
library(dplyr)

setwd("/Users/alessiarainone/Desktop/Populist-Narratives-on-Environmental-Issues")
data <- read.csv("01-Data/random_sample.csv", sep = ",", header = TRUE)

# Save test data----
set.seed(1234)
test_sample <- sample_n(data, 50)
write.csv(test_sample, "test_sample.csv", row.names = FALSE)

### Load test data---- 
# test_sample <- read.csv("test_sample.csv", sep = ",", header= TRUE)
test_sample <- test_sample %>%
  select(-regional, -doctype, -doctype_description, -language, -subhead)

### Prompt & API Key
analysis_prompt <- read_lines("02-Scripts/02-03-Analysis_Prompt.txt") %>% paste(collapse = "\n")
APIkey <- readLines("00-Planning/00_02_API/OpenAI/openai_key.txt")

### Function to read in articles
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
    message("ERROR", e$message)
    return(NULL)
  })
  
  if (is.null(response)) return(NULL)
  
  result_text <- content(response)$choices[[1]]$message$content
  
  # Exactly 6 numbers
  result_lines <- strsplit(result_text, "\n")[[1]]
  result_lines <- trimws(result_lines)
  
  # Falls weniger als 6 Zeilen kommen, gib NA zurück
  if (length(result_lines) < 6) {
    warning("Uncomplete answer received")
    return(tibble(
      relevance = NA,
      party = NA,
      support = NA,
      discourse = NA,
      people = NA,
      elite = NA
    ))
  }
  
  # Rückgabe als DataFrame / tibble
  return(tibble(
    relevance = as.integer(result_lines[1]),
    party     = as.integer(result_lines[2]),
    support   = as.integer(result_lines[3]),
    discourse = as.integer(result_lines[4]),
    people    = as.integer(result_lines[5]),
    elite     = as.integer(result_lines[6])
  ))
}

results <- list()

for (i in seq_along(test_articles_text)) {
  cat("🔎 Analysiere Artikel", i, "von", length(test_articles_text), "\n")
  
  article_result <- analyze_article(test_articles_text[i], analysis_prompt, APIkey)
  
  # Artikelnummer hinzufügen
  article_result$article_nr <- test_articles$article_nr[i]
  
  results[[i]] <- article_result
}

# Ergebnisse zu einem DataFrame zusammenfügen
final_results <- bind_rows(results)

# Optional: nach Artikelnummer sortieren
final_results <- final_results %>% arrange(article_nr)

# Speichern als CSV
write_csv(final_results, "03-Output/article_analysis_results.csv")




# New Try -----------------------------------------------------------------
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
  
  # Einzelne Variablen sicher extrahieren
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


# Artikel vorbereiten (falls noch nicht geschehen)
test_articles <- test_sample %>%
  mutate(id = as.character(id)) %>%
  filter(id %in% c("45180862", "37568223", "45746754", "37790583", "38068220")) %>%
  mutate(article_nr = 1:n())

test_articles_text <- test_articles$content
test_article_nr <- test_articles$article_nr

# Ergebnisse speichern
results <- list()

# Analyse durchführen
for (i in seq_along(test_articles_text)) {
  cat("🔎 Analysiere Artikel", i, "von", length(test_articles_text), "\n")
  
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
write_csv(final_results, "03-Output/article_analysis_results_test_2.csv")


