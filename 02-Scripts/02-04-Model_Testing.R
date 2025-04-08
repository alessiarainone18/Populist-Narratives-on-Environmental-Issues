library(tidyverse)
library(httr)
library(readr)

setwd("/Users/alessiarainone/Desktop/Populist-Narratives-on-Environmental-Issues")
data <- read.csv("random_sample.csv", sep = ",", header = TRUE)

# Save test data----
set.seed(234)
test_sample <- sample_n(data, 20)
write.csv(test_sample, "test_sample.csv", row.names = FALSE)


### Load test data---- 
test_sample <- read.csv("test_sample.csv", sep = ",", header= TRUE)
test_sample <- test_sample %>%
  select(-regional, -doctype, -doctype_description, -language, -subhead)


# Did some tests in the ChatGPT Interface. Next step: Testing of API
### Testing of API ----
analysis_prompt <- read_lines("02-Scripts/02-03-Analysis_Prompt.txt") %>% paste(collapse = "\n") # to make sure it is without paragraphs

n_sample <- 10


# # Authentication
# APIkey <- readLines("00-Planning/00_02_API/OpenAI/openai_key.txt")   # place your API key in a .txt file
# bearer <- stringr::str_c("Authorization: Bearer ", APIkey)
# # 
# # Text generation endpoints
# r <- httr::POST(
#   url = "https://api.openai.com/v1/chat/completions", 
#   content_type("application/json"), 
#   add_headers(Authorization = paste("Bearer", APIkey, sep = " ")), 
#   body = list(
#     model = "gpt-4o", 
#     # messages is a list of lists
#     messages = list(
#       list(role = "system", 
#            content = analysis_prompt))
#   ), 
#   encode = "json"
# )
# content(r)
# 
# cat(content(r)$choices[[1]]$message$content)

test_sample <- read.csv("test_sample.csv", sep = ",", header = TRUE)
test_articles <- test_sample$content  # erwartet eine Spalte 'content' mit deinen Artikeln
test_articles 
### Load analysis prompt and API key ----
analysis_prompt <- read_lines("02-Scripts/02-03-Analysis_Prompt.txt") %>% paste(collapse = "\n")
APIkey <- readLines("00-Planning/00_02_API/OpenAI/openai_key.txt")

### Define function to analyze one article via OpenAI API ----
analyze_article <- function(article_text, prompt, APIkey) {
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    content_type_json(),
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
  
  result <- content(response, as = "parsed", type = "application/json")
  
  # Ergebnis zurückgeben, wenn erfolgreich – sonst Fehler anzeigen
  if (!is.null(result$choices) && length(result$choices) > 0) {
    return(result$choices[[1]]$message$content)
  } else {
    return(paste("Fehler bei Artikel:", article_text))
  }
}

### Analyze all articles ----
results <- vector("character", length(test_articles))

for (i in seq_along(test_articles)) {
  cat("📝 Verarbeite Artikel", i, "von", length(test_articles), "...\n")
  results[i] <- analyze_article(test_articles[i], analysis_prompt, APIkey)
}

### Save results to file ----
writeLines(results, "analysis_results.txt")

cat("✅ Analyse abgeschlossen. Ergebnisse gespeichert in 'analysis_results.txt'\n")



### Second Try----
### Load required packages ----
library(httr)
library(stringr)
library(readr)
library(jsonlite)
library(dplyr)

### Load test data ----
### Load test data ----
test_sample <- read.csv("test_sample.csv", sep = ",", header = TRUE)

### Zufällige Auswahl von 5 Artikeln ----
test_articles_2 <- test_sample[sample(1:nrow(test_sample), 5), ]  # 5 zufällige Artikel

# Artikeltext extrahieren
test_articles_text <- test_articles_2$content

### Optional: Artikelnummer hinzufügen
test_articles_2$article_nr <- 1:nrow(test_articles_2)

### Ergebnisse anzeigen ----
print(test_articles_2)
# Analyse-Prompt und API-Schlüssel laden
analysis_prompt <- read_lines("02-Scripts/02-03-Analysis_Prompt.txt") %>% paste(collapse = "\n")
APIkey <- readLines("00-Planning/00_02_API/OpenAI/openai_key.txt")

# Funktion zur Analyse eines Artikels
analyze_article <- function(article_text, prompt, APIkey) {
  response <- httr::POST(
    url = "https://api.openai.com/v1/chat/completions",
    content_type_json(),
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
  
  result <- content(response, as = "parsed", type = "application/json")
  
  if (!is.null(result$choices) && length(result$choices) > 0) {
    raw_text <- result$choices[[1]]$message$content
    
    # Extrahiere nur die Zahlen (es sollten genau 6 sein)
    codes <- str_extract_all(raw_text, "\\b\\d{1,2}\\b")[[1]]
    
    # Falls weniger als 6, NA auffüllen
    while (length(codes) < 6) {
      codes <- c(codes, NA)
    }
    
    return(data.frame(
      relevance = codes[1],
      party     = codes[2],
      support   = codes[3],
      discourse = codes[4],
      elite     = codes[5],
      people    = codes[6],
      stringsAsFactors = FALSE
    ))
    
  } else {
    return(data.frame(
      relevance = NA,
      party     = NA,
      support   = NA,
      discourse = NA,
      elite     = NA,
      people    = NA,
      stringsAsFactors = FALSE
    ))
  }
}

# Zufällige Auswahl von 5 Artikeln und deren Artikelnummern
test_sample <- read.csv("test_sample.csv", sep = ",", header = TRUE)
test_articles_2 <- test_sample[sample(1:nrow(test_sample), 5), ]
test_articles_text <- test_articles_2$content  # Artikeltexte extrahieren

# Ergebnisse speichern
results <- list()

# Schleife über alle Artikel
for (i in seq_along(test_articles_text)) {
  cat("🔎 Analysiere Artikel", i, "von", length(test_articles_text), "\n")
  article_result <- analyze_article(test_articles_text[i], analysis_prompt, APIkey)
  
  # Füge Artikelnummer zu den Ergebnissen hinzu
  article_result$article_id <- test_articles_2$article_nr[i]  # Artikelnummer hinzufügen
  
  results[[i]] <- article_result
}

# Ergebnisse zusammenführen und CSV schreiben
results_df <- bind_rows(results)
write.csv(results_df, "GPT_Coded_Articles.csv", row.names = FALSE)

cat("📝 Ergebnisse erfolgreich in 'GPT_Coded_Articles.csv' gespeichert!")
