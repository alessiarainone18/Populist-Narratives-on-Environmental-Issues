library(tidyverse)
library(quanteda)

# Load data
data <- read.delim("dataset_climate.tsv", sep = "\t", encoding = "UTF-8")
data <- data %>% mutate(year = year(as_datetime(pubtime)))

# Filter German articles
data_de <- data %>% filter(language == "de")
grouped_by_medium_de <- data_de %>% group_by(year, medium_code) %>% summarize(n = n())
grouped_by_year_de <- data_de %>% group_by(year) %>% summarize(n = n())

# Define political keywords
party_keywords <- c("Grüne", "GPS", "Junge Grüne Schweiz", "Grünliberale Partei", "GLP")

data_de$content <- as.character(data_de$content)
filtered_data <- data_de %>% 
  filter(grepl(paste(party_keywords, collapse = "|"), content, ignore.case = TRUE)) %>%
  mutate(wordcount = str_count(content, "\\S+")) %>%
  distinct(medium_code, content, .keep_all = TRUE) %>%
  filter(wordcount >= 200 & wordcount <= 2000, year >= 2014)

# Exclude local politics
local_terms <- c("Gemeinderat", "Stadtrat", "Ortsparlament", "Quartierverein", 
                 "Gemeindeversammlung", "Baukommission", "Ortsplanung")
filtered_national <- filtered_data %>% filter(!grepl(paste(local_terms, collapse = "|"), content, ignore.case = TRUE))

# Filter general and Swiss politics
general_terms <- c("Partei", "politisch", "Politik", "Parlament", "Gesetz", "Bundesrat")
swiss_terms <- c("Schweiz", "Schweizer", "eidgenössisch", "national", "kantonal", "Regierungsrat", "Grossrat", "Kantonsrat")
filtered_swiss <- filtered_national %>% 
  filter(grepl(paste(general_terms, collapse = "|"), content, ignore.case = TRUE)) %>%
  filter(grepl(paste(swiss_terms, collapse = "|"), content, ignore.case = TRUE)) %>%
  filter(rubric != "international", medium_code != "BAZ", medium_code != "NNBE")

# Plot Swiss reports per year and medium
grouped_by_medium_swiss <- filtered_swiss %>% group_by(year, medium_code) %>% summarize(n = n())
medium_labels_swiss <- medium_labels[c("ZWAO", "NZZO", "NNTA", "SRF", "BLIO")]

ggplot(grouped_by_medium_swiss, aes(x = year, y = n, color = medium_code, group = medium_code)) +
  geom_line() +
  labs(title = "Climate Change Reports in Swiss Politics", x = "Year", y = "Number of Records") +
  scale_x_continuous(breaks = 2014:2024) +
  scale_color_manual(values = medium_colors, labels = medium_labels_swiss) +
  theme_minimal()

cat("Number of filtered articles:", nrow(filtered_swiss), "\n")
print(filtered_swiss %>% summarise(wordcount_mean = mean(wordcount)))

# Tokenization and Stopword Removal
myCorpus <- corpus(filtered_swiss$content)
strwrap(as.character(myCorpus)[1])

tok_clean <- tokens(myCorpus, 
                    remove_punct = TRUE, remove_numbers = TRUE, 
                    remove_symbols = TRUE, split_hyphens = TRUE, 
                    remove_separators = TRUE) %>%
  tokens_remove(stopwords("german")) 

word_counts <- sapply(tok_clean, function(x) str_count(paste(x, collapse = " "), "\\S+"))
mean_tok <- mean(word_counts, na.rm = TRUE)
mean_tok