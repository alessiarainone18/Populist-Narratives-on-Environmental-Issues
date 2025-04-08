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
test_sample <- sample_n(data, 20)
write.csv(test_sample, "test_sample.csv", row.names = FALSE)


### Load test data---- 
test_sample <- read.csv("test_sample.csv", sep = ",", header= TRUE)
test_sample <- test_sample %>%
  select(-regional, -doctype, -doctype_description, -language, -subhead)


# Did some tests in the ChatGPT Interface. Next step: Testing of API

### Load test data ----
# Analyse-Prompt und API-Schlüssel laden
analysis_prompt <- read_lines("02-Scripts/02-03-Analysis_Prompt.txt") %>% paste(collapse = "\n")
APIkey <- readLines("00-Planning/00_02_API/OpenAI/openai_key.txt")

# Funktion zur Analyse eines Artikels
analyze_article <- function(article_text, prompt, APIkey) {
  response <- tryCatch({
    httr::POST(
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
  }, error = function(e) {
    return(NULL)
  })
  
  if (is.null(response)) {
    return(data.frame(
      relevance = 99,
      party     = 99,
      support   = 99,
      discourse = 99,
      elite     = 99,
      people    = 99,
      stringsAsFactors = FALSE
    ))
  }
  
  # Antwort parsen
  result <- content(response, as = "parsed", type = "application/json")
  
  if (!is.null(result$choices) && length(result$choices) > 0) {
    raw_text <- result$choices[[1]]$message$content
    
    # Extrahiere nur die Zahlen (es sollten genau 6 sein)
    # Wir filtern alle Zahlen (einschließlich der Fälle wie 1, 2, 99)
    codes <- str_extract_all(raw_text, "\\b\\d{1,2}\\b")[[1]]
    
    # Falls weniger als 6 Codes gefunden wurden, mit `99` auffüllen
    while (length(codes) < 6) {
      codes <- c(codes, 99)
    }
    
    # Sicherstellen, dass nur die numerischen Codes zurückgegeben werden
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
    # Falls keine gültigen `choices` vorhanden sind, mit `99` auffüllen
    return(data.frame(
      relevance = 99,
      party     = 99,
      support   = 99,
      discourse = 99,
      elite     = 99,
      people    = 99,
      stringsAsFactors = FALSE
    ))
  }
}


# Zufällige Auswahl von 5 Artikeln und deren Artikelnummern
test_sample <- read.csv("test_sample.csv", sep = ",", header = TRUE)
test_articles_2 <- test_sample %>%
  filter(id %in% c(42459698, 51623332, 47895683, 37790583))

test_articles_text <- test_articles_2$content  

# Ergebnisse speichern
results <- list()

# Schleife über alle Artikel
for (i in seq_along(test_articles_text)) {
  cat("🔎 Analysiere Artikel", i, "von", length(test_articles_text), "\n")
  article_result <- analyze_article(test_articles_text[i], analysis_prompt, APIkey)
  
  # Füge Artikelnummer zu den Ergebnissen hinzu
  article_result$article_id <- test_articles_2$article_nr[i]  
  
  results[[i]] <- article_result
}

# Ergebnisse zusammenführen und CSV schreiben
results_df <- bind_rows(results)
write.csv(results_df, "GPT_Coded_Articles.csv", row.names = FALSE)




## New try----
library(httr)
library(jsonlite)
library(readr)
library(dplyr)
library(purrr)

# Prompt einlesen und vorbereiten
analysis_prompt <- read_lines("02-Scripts/02-03-Analysis_Prompt.txt") %>% paste(collapse = "\n")
APIkey <- readLines("00-Planning/00_02_API/OpenAI/openai_key.txt")

# Funktion zur Analyse eines Artikels
analyze_article <- function(article_text, prompt, APIkey) {
  response <- tryCatch({
    httr::POST(
      url = "https://api.openai.com/v1/chat/completions",
      content_type_json(),
      add_headers(Authorization = paste("Bearer", APIkey)),
      body = list(
        model = "gpt-4o",
        messages = list(
          list(role = "system", content = prompt),
          list(role = "user", content = article_text)
        ),
        temperature = 0
      ),
      encode = "json"
    )
  }, error = function(e) {
    message("❌ Fehler bei der Anfrage: ", e$message)
    return(NULL)
  })
  
  if (is.null(response)) return(NULL)
  
  result_text <- content(response)$choices[[1]]$message$content
  
  # Erwartet wird eine Antwort mit genau 6 Zahlen (jede auf neuer Zeile)
  result_lines <- strsplit(result_text, "\n")[[1]]
  result_lines <- trimws(result_lines)
  
  # Falls weniger als 6 Zeilen kommen, gib NA zurück
  if (length(result_lines) < 6) {
    warning("Unvollständige Antwort erhalten")
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

# Zufällige Auswahl von 5 Artikeln und deren Artikelnummern
test_sample <- read.csv("test_sample.csv", sep = ",", header = TRUE)
test_articles_2 <- test_sample %>%
  filter(id %in% c(42459698, 51623332, 47895683, 37790583))

test_articles_text <- test_articles_2$content  

# Test-Artikel vorbereiten
# Angenommen, test_articles_text enthält die Artikeltitel, test_articles_2$article_nr die IDs
results <- list()

for (i in seq_along(test_articles_text)) {
  cat("🔎 Analysiere Artikel", i, "von", length(test_articles_text), "\n")
  
  article_result <- analyze_article(test_articles_text[i], analysis_prompt, APIkey)
  
  # Artikelnummer hinzufügen
  article_result$article_id <- test_articles_2$article_nr[i]
  
  results[[i]] <- article_result
}

# Ergebnisse zu einem DataFrame zusammenfügen
final_results <- bind_rows(results)

# Optional: nach Artikelnummer sortieren
final_results <- final_results %>% arrange(article_id)

# Speichern als CSV
write_csv(final_results, "03-Output/article_analysis_results.csv")

cat("✅ Analyse abgeschlossen. Ergebnisse gespeichert unter '03-Results/article_analysis_results.csv'\n")

