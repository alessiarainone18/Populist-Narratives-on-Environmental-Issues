# Load packages
library(tidyverse)

# Load data
setwd("/Users/alessiarainone/Desktop/Populist-Narratives-on-Environmental-Issues")
data_output <- read.csv("03-Output/03-03-article_analysis_results_2500_final.csv", sep = ",", header = TRUE)
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
  left_join(data_output_renamed, by = "article_nr")

# Check id 
filtered <- data_combined %>%
  filter(id.x != id.y | is.na(id.y)) 

# Check duplicated ids
dupli_ids <- data_combined %>%
  count(id.x) %>%
  filter(n > 1) %>%
  pull(id.x)

duplicates <- data_combined %>%
  filter(id.x %in% dupli_ids)

duplicates %>% select(id.x, wordcount)

exception <- duplicates %>%
  filter(id.x==50592675) %>%
  print()

# There are duplicates! Not many, but this allows to check on AI coding. Apart from one article
# all duplicates where coded the same way. The exception makes id "50592675", in which both the Greens
# and the SVP were discussed. According to my own assessment of the article, the coding of party= SVP is taken
# into account from the SVP because it was predominant. In all other cases, the second appearance of the article was considered an “error”. 

data_combined_cleaned <- data_combined %>%
  filter(article_nr != 735) %>% 
  distinct(id.x, .keep_all = TRUE) %>%
  select(-id.y) %>% 
  rename("id" = "id.x")

# Check if it worked
dupli_ids_2 <- data_combined_cleaned %>%
  count(id) %>%
  filter(n > 1) %>%
  pull(id)

duplicates_2 <- data_combined_cleaned %>%
  filter(id %in% dupli_ids_2) %>% 
  print()

# 0 duplicates

## ERROR DIAGNOSIS---- 
errors <- data_combined_cleaned %>%
  filter(is.na(relevance)) 


# 492 errors. But why?
# Comparison of classified cases vs. errors

errors %>%
  summarise(
    mean_wordcount = mean(wordcount, na.rm = TRUE),
    mean_charcount = mean(char_count, na.rm = TRUE),
    under700 = sum(wordcount < 700, na.rm = TRUE),
    over700 = sum(wordcount > 700, na.rm = TRUE),
    under1000 = sum(wordcount < 1000, na.rm =TRUE),
    over1000 = sum(wordcount > 1000, na.rm =TRUE),
    mean_article_nr = mean(article_nr, na.rm = TRUE)
  )

data_correct <- data_combined_cleaned %>%
  filter(!is.na(relevance)) 

data_correct %>%
  summarise(
    mean_wordcount = mean(wordcount, na.rm = TRUE),
    mean_charcount = mean(char_count, na.rm = TRUE),
    under700 = sum(wordcount < 700, na.rm = TRUE),
    over700 = sum(wordcount > 700, na.rm = TRUE),
    under1000 = sum(wordcount < 1000, na.rm =TRUE),
    over1000 = sum(wordcount > 1000, na.rm =TRUE),
    mean_article_nr = mean(article_nr, na.rm = TRUE)
  )

# Errors have almost double as much wordcount and char_count. Probably too long articles to 
# analyze for API why it failed. Not clear if errors occurred more often in the end of the analysis,
# since average article_nr is similar in both cases. Cause is most probably the length of articles. 
# Important to be careful with generalization!

# Analysis with data_correct
data_svp <- data_correct %>%
  filter(party == 1) %>%
  select(id, party, discourse, people, elite)

data_jsvp <- data_correct %>%
  filter(party == 2) %>%
  select(id, party, discourse, people, elite)

data_green <- data_correct %>%
  filter(party == 3) %>%
  select(id, party, discourse, people, elite)

data_younggreen <- data_correct %>%
  filter(party == 4) %>%
  select(id, party, discourse, people, elite)

