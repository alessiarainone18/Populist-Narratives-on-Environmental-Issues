# Load packages
library(tidyverse)
library(dplyr)
library(knitr)
library(stargazer)
library(gt)
library(openxlsx)
library(FactoMineR)
library(factoextra)
library(dplyr)


# Load data
setwd("/Users/alessiarainone/Desktop/Populist-Narratives-on-Environmental-Issues")
data <- read.csv("01-Data/01-02-Data_Cleaned.csv", sep = ",", header = TRUE) # overall sample
valid_data <- read.csv("01-Data/01-03-Data_Cleaned_Valid.csv", sep = ",", header = TRUE) # only valid

valid_data_filtered <- valid_data %>%
  mutate(across(where(is.numeric), ~na_if(., 99))) %>%
  filter(relevance == 1)

# As there are too few cases in JSVP and Young Greens to make meaningful statements
# I will code them into the real party.
valid_data_filtered <- valid_data_filtered %>%
  mutate(
    party_group = case_when(
      party %in% c(1, 2) ~ "SVP",   # z.B. Grüne + Junge Grüne
      party %in% c(3, 4) ~ "Greens",   # z.B. SVP + Junge SVP
      TRUE ~ as.character(party)         
    )
  )

table(valid_data_filtered$party_group, valid_data_filtered$discourse)

valid_data_filtered %>%
  group_by(party_group) %>%
  summarise(mean_discourse = mean(discourse, na.rm = TRUE))


ggplot(valid_data_filtered, aes(x = party_group, y = discourse)) +
  geom_boxplot() +
  labs(title = "Discourse Style by party", y = "Discourse (1=pluralistic ... 5=populistic")

valid_data_filtered %>%
  filter(discourse %in% c(4, 5),
         !is.na(people)) %>%
  count(party_group, people, elite) %>%
  ggplot(aes(x = party_group, y = n, fill = factor(people))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Who is being addressed in populist discourse?",
       y = "Count", x = "Party", fill = "People")

valid_data_filtered %>%
  filter(discourse %in% c(4, 5),
         !is.na(elite)) %>%
  count(party_group, people, elite) %>%
  ggplot(aes(x = party_group, y = n, fill = factor(elite))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Who is seen as the Elite?",
       y = "Count", x = "Party", fill = "Elite")

chisq.test(table(valid_data_filtered$party_group, valid_data_filtered$discourse))

valid_data_filtered <- valid_data_filtered %>%
  mutate(populistic = ifelse(discourse %in% c(4, 5), 1, 0))

valid_data_filtered %>% count(support)
valid_data_filtered <- valid_data_filtered %>%
  mutate(
    support_dummy = case_when(
      support == 1 ~ 1,
      support == 2 ~ 0,
      support == 3 ~ NA_real_,
      TRUE ~ NA_real_ 
    ),
    populistic = ifelse(discourse %in% c(4, 5), 1, 0),
    pluralistic = ifelse(discourse %in% c(1, 2), 1, 0),
    
  )

# Logit Model
glm(populistic ~ party_group + support_dummy, data = valid_data_filtered, family = "binomial") %>%
  summary()

# Populist Discourse: Elite vs. People
pop_data <- valid_data_filtered %>%
  filter(discourse %in% c(4, 5),
         !is.na(people)|!is.na(elite)) %>%
  mutate(people = as.factor(people),
         elite = as.factor(elite))


# MCA 
mca_data <- valid_data_filtered %>%
  select(elite) 


