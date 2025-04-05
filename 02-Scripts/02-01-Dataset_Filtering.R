# Load packages
library(tidyverse)
library(quanteda)

# Set up WD
setwd("/Users/alessiarainone/Desktop/Data-Mining-Project_Climate-Change-Media-Attention/01-Data")

# Load data
data <- read.delim("dataset_environmentalpolitics.tsv", sep = "\t", encoding = "UTF-8")
data <- data %>% mutate(year = year(as_datetime(pubtime)))

# # Filter German articles since there are to few french in the database
# data_de <- data %>% filter(language == "de") %>%
#   filter(medium_code %in%  c("ZWAO", "NZZO", "NNTA", "SRF", "BLIO"))


# Define keywords to detect green parties
party_keywords <- c(
  # Grüne Parteien
  "Grüne", "GPS", "Junge Grüne Schweiz", "Grünen", "Grüne Partei", 
  
  # Konservative Parteien
  "Schweizerische Volkspartei", "SVP", "SVP Schweiz", "Schweizerische Volkspartei Schweiz", "Swiss People's Party")
  
data$content <- as.character(data$content)
filtered_data <- data %>% 
  filter(grepl(paste(party_keywords, collapse = "|"), content, ignore.case = TRUE)) %>%
  mutate(wordcount = str_count(content, "\\S+")) %>%
  distinct(medium_code, content, .keep_all = TRUE) %>%
  filter(wordcount >= 200 & wordcount <= 2000, year >= 2014)

# Draw a random sample of 5000
set.seed(123)
random_sample <- sample_n(filtered_data, 5000)

# Save sample
write.csv(random_sample, "random_sample.csv", row.names = FALSE)

# Plot Swiss reports per year and medium---
grouped_by_medium <- random_sample %>%
  group_by(year, medium_code) %>% 
  summarise(n = n(), .groups = "drop")
medium_labels <- c("ZWAO", "NZZO", "NNTA", "SRF", "BLIO")

ggplot(grouped_by_medium, aes(x = year, y = n, color = medium_code, group = medium_code)) +
  geom_line() + 
  labs(title = "Reports on SVP and Greens on environmental issues in Swiss Politics", 
       x = "Year", y = "Number of Records") +
  scale_x_continuous(breaks = 2009:2024) +  # Festlegen der x-Achsen-Beschriftungen
  scale_color_manual(values = c("blue", "green", "red", "purple", "orange"), 
                     labels = medium_labels) +  # Farbliche Zuweisung und Labels
  theme_minimal()  

print(random_sample %>%
        summarise(wordcount_mean = mean(wordcount)))

# Tokenization and stopword removal, as lots of unnecessary tokens
# which will make analysis by API more expensive
myCorpus <- corpus(random_sample$content)
strwrap(as.character(myCorpus)[1])

tok_clean <- tokens(myCorpus, 
                    remove_punct = TRUE, remove_numbers = TRUE, 
                    remove_symbols = TRUE, split_hyphens = TRUE, 
                    remove_separators = TRUE) %>%
  tokens_remove(stopwords("german")) 

word_counts <- sapply(tok_clean, function(x) str_count(paste(x, collapse = " "), "\\S+"))
mean_tok <- mean(word_counts, na.rm = TRUE)
mean_tok 


## TOTAL OF TOKENS: 2'550'000