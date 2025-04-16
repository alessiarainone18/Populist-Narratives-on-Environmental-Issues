# Load packages
library(tidyverse)
library(dplyr)
library(knitr)
library(stargazer)
library(gt)
library(openxlsx)

# Load data
setwd("/Users/alessiarainone/Desktop/Populist-Narratives-on-Environmental-Issues")
data_output <- read.csv("03-Output/03-01-article_analysis_results_FINAL2.csv", sep = ",", header = TRUE)
data_input <- read.csv("01-Data/random_sample_2500.csv", sep = ",", header = TRUE)

data_input <- data_input %>%
  distinct(id, .keep_all = TRUE) %>%
  filter(str_detect(content, regex(paste(party_keywords, collapse = "|"), ignore_case = TRUE))) %>%
  mutate(wordcount = str_count(content, "\\S+")) %>%
  filter(wordcount >= 300, wordcount <= 2000, year >= 2014) %>%
  filter(!str_detect(rubric, regex("international", ignore_case = TRUE))) 

# Variable coding for data_output form Open AI's analysis
data_output_renamed <- data_output %>% 
  rename("relevance"="code_1", "party"="code_2", "support"="code_3", "discourse"="code_4",
         "elite"="code_5", "people"="code_6") %>%
  mutate(article_nr = 1:n())

data_input <- data_input %>%
  mutate(article_nr = 1:n())

# Merge
data_combined_cleaned <- data_input %>% 
  left_join(data_output_renamed, by = "article_nr") %>%
  select(-id.y) %>% 
  rename("id" = "id.x")

write.csv(data_combined_cleaned, "01-Data/01-01-Data_Cleaned.csv", row.names = FALSE)


## ERROR DIAGNOSIS---- 
errors <- data_combined_cleaned %>%
  filter(is.na(relevance)) 

# 317 errors. Let's try to re-run analysis for the errors again!
##### ERROR DIAGNOSIS----
merged <- data_combined_cleaned %>%
  left_join(data_first, by = "id")

errors <- merged_recoded_cleaned  %>%
  filter(is.na(relevance)) 

write.csv(errors, "03-Output/03-03-Data_Errors.csv", row.names = FALSE)

#### After API has been running again: Load data-----
data_error <- read.csv("03-Output/03-01-article_analysis_results_ERRORS.csv", sep = ",", header = TRUE)

# Variable coding for data_output form Open AIs analysis
data_error_renamed <- data_error %>% 
  rename("relevance"="code_1", "party"="code_2", "support"="code_3", "discourse"="code_4",
         "elite"="code_5", "people"="code_6") %>%
  mutate(article_nr = 1:n()) %>%
  select(-article_nr)

data_combined_errors  <- data_input %>% 
  inner_join(data_error_renamed, by = "id")

# Merge
merged_recoded_cleaned <- bind_rows(data_correct, data_combined_errors)
write.csv(merged_recoded_cleaned, "01-Data/01-02-Data_Cleaned.csv", row.names = FALSE)


#### New error diagnosis-----
errors <- data_combined_errors   %>%
  filter(is.na(relevance)) 

summary_errors <- errors %>%
  summarise(
    `Mean Word Count` = mean(wordcount, na.rm = TRUE),
    `Mean Character Count` = mean(char_count, na.rm = TRUE),
    `Articles < 1000 Words` = sum(wordcount < 1000, na.rm = TRUE),
    `Articles ≥ 1000 Words` = sum(wordcount >= 1000, na.rm = TRUE),
    `Articles Before 2020` = sum(year < 2020, na.rm = TRUE),
    `Articles From 2020 Onward` = sum(year >= 2020, na.rm = TRUE)
  ) %>%
  t() %>%
  as.data.frame()

# Summarize valid articles
summary_correct <- merged_recoded_cleaned  %>%
  filter(!is.na(relevance)) %>%
  summarise(
    `Mean Word Count` = mean(wordcount, na.rm = TRUE),
    `Mean Character Count` = mean(char_count, na.rm = TRUE),
    `Articles < 1000 Words` = sum(wordcount < 1000, na.rm = TRUE),
    `Articles ≥ 1000 Words` = sum(wordcount >= 1000, na.rm = TRUE),
    `Articles Before 2020` = sum(year < 2020, na.rm = TRUE),
    `Articles From 2020 Onward` = sum(year >= 2020, na.rm = TRUE)
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

data_correct_merged <- merged_recoded_cleaned %>%
  filter(!is.na(relevance)) 

# Errors have almost double as much wordcount and char_count. Probably too long articles to 
# analyze for API why it failed. Not clear if errors occurred more often in the end of the analysis,
# since average article_nr is similar in both cases. Cause is most probably the length of articles. 
# Important to be careful with generalization!

#### Validate results with random sample of 20 ----
validation_sample <- sample_n(data_correct_merged, 20)
write.xlsx(validation_sample, "03-Output/03-04-Validation_Sample.xlsx", row.Names = FALSE)


# Small analysis with data_correct
data_svp <- data_correct_merged %>%
  filter(party == 1) %>%
  select(id, party, support, discourse, people, elite)

data_jsvp <- data_correct_merged %>%
  filter(party == 2) %>%
  select(id, party, support, discourse, people, elite)

data_green <- data_correct_merged %>%
  filter(party == 3) %>%
  select(id, party, support, discourse, people, elite)

data_younggreen <- data_correct_merged %>%
  filter(party == 4) %>%
  select(id, party, support, discourse, people, elite)


