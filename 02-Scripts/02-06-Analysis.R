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
# I will code them into the "mother" party.
valid_data_filtered <- valid_data_filtered %>%
  mutate(
    party_group = case_when(
      party %in% c(1, 2) ~ "SVP",   # z.B. Grüne + Junge Grüne
      party %in% c(3, 4) ~ "Greens",   # z.B. SVP + Junge SVP
      TRUE ~ as.character(party)         
    )
  )

table(valid_data_filtered$party_group, valid_data_filtered$discourse)
chisq.test(table(valid_data_filtered$party_group, valid_data_filtered$discourse))

valid_data_filtered %>% filter(!is.na(discourse)) %>%
  group_by(party_group) %>%
  summarise(mean_discourse = mean(discourse, na.rm = TRUE))


# Boxplot
ggplot(valid_data_filtered %>% filter(!is.na(discourse)), aes(x = party_group, y = discourse)) +
  geom_boxplot(aes(fill = party_group), 
               color = "black", 
               outlier.shape = 16, 
               outlier.colour = "red", 
               outlier.size = 3) +
  labs(title = "Discourse Style by Party", 
       y = "Discourse (1=pluralistic ... 5=populistic)", 
       x = "Party Group") +
  theme_minimal() +
  theme(
    text = element_text(size = 12), # Setzt die Textgröße auf 12
    axis.text.x = element_text(angle = 45, hjust = 1), # Dreht die x-Achsen-Beschriftungen um 45 Grad
    axis.title = element_text(size = 14), # Größere Achsentitel
    plot.title = element_text(hjust = 0.5, size = 16), # Zentriert den Titel und setzt die Größe
    legend.position = "none" # Entfernt die Legende (falls nicht benötigt)
  )


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
logit_model <- glm(populistic ~ party_group + support_dummy, data = valid_data_filtered, family = "binomial") 
summary(logit_model)


stargazer(model,
          type = "text",
          title = "Logistic Regression: Populist Discourse",
          dep.var.labels = "Populistic (1 = populist)",
          covariate.labels = c("SVP", "Support"),
          omit.stat = c("ll", "aic"),
          no.space = TRUE)




### MCA 
# Prepare data
mca_data <- valid_data_filtered %>%
  filter(!is.na(support_dummy) & !is.na(party_group)) %>%  # Zeilen mit NA-Werten entfernen
  mutate(
    support_factor = case_when(
      support == 2 ~ "Oppose",  
      support == 1 ~ "Support",
      TRUE ~ NA_character_  
    ),
    
    discourse_factor = case_when(
      discourse == 1 ~ "very pluralistic",
      discourse == 2 ~ "rather pluralistic",
      discourse == 3 ~ "neutral",
      discourse == 4 ~ "rather populistic",
      discourse == 5 ~ "very populistic",
      TRUE ~ NA_character_  
    ),

    elite_factor = case_when(
      elite == 1 ~ "Elite = Government/politicians",
      elite == 2 ~ "Elite = Corporations/business",
      elite == 3 ~ "Elite = Media",
      elite == 4 ~ "Elite = Intellectuals",
      elite == 5 ~ "Elite = International organisations",
      elite == 6 ~ "Elite = NGOs",
      TRUE ~ NA_character_  
    ),
    
    people_factor = case_when(
      people == 1 ~ "People = Swiss population",
      people == 2 ~ "People = Workers",
      people == 3 ~ "People = Rural communities",
      people == 4 ~ "People = Traditionalists",
      people == 5 ~ "People = Small businesses",
      people == 6 ~ "People = Farmers",
      TRUE ~ NA_character_  
    )
  ) %>%
  select(party_group, support_factor, discourse_factor, elite_factor, people_factor) %>%
  mutate(across(everything(), as.factor))
 


# MCA 
mca_result <- MCA(mca_data, graph = FALSE)

# Plot
fviz_mca_biplot(mca_result,
                repel = TRUE,
                ggtheme = theme_minimal(),
                label = "all",
                habillage = mca_data$party_group) +
  scale_color_manual(values = c("SVP" = "lightblue", "Greens" = "lightgreen")) +
  theme(
    text = element_text(color = "black")  
  )


# People vs. Elite
mca_data %>%
  filter(discourse_factor %in% c("rather populistic", "very populistic"),
         !is.na(people_factor)) %>%
  count(party_group, people_factor, elite_factor) %>%
  ggplot(aes(x = party_group, y = n, fill = factor(people_factor))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Who is being addressed in populist discourse?",
       y = "Count", x = "Party", fill = "People")

mca_data %>%
  filter(discourse_factor %in% c("rather populistic", "very populistic"),
         !is.na(elite_factor)) %>%
  count(party_group, people_factor, elite_factor) %>%
  ggplot(aes(x = party_group, y = n, fill = factor(elite_factor))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Who is seen as the Elite?",
       y = "Count", x = "Party", fill = "Elite")

