library(tidyverse)
library(readr)
library(ggplot2)

df <- data %>%
  mutate(year = year(as_datetime(pubtime)))

grouped_per_medium <- df %>% 
  group_by(year, medium_code) %>%
  summarize(n = n()) 

grouped_per_year <- df %>% 
  group_by(year) %>% 
  summarize(n= n())


## pro jahr und medium
ggplot(grouped_per_medium, aes(x = year, y = n, color = medium_code, group = medium_code)) +
  geom_line() + 
  labs(title = "Number of reports on climate change per year and medium",
       x = "Year",
       y = "Number of Records") +
  theme_minimal()


# Angepasste Labels für 'medium_code'
medium_labels <- c(
  "NNBE" = "Berner Zeitung",
  "ZWAO" = "20 Minuten Online",
  "NZZO" = "NZZ.ch",
  "NNTA" = "Tagesanzeiger.ch",
  "SRF" = "SRF.ch",
  "BLIO" = "Blick Online",
  "BAZ" = "Basler Zeitung",
  "TLM" = "Le Matin",
  "HEU" = "24 Heures"
)

# Erstelle das Diagramm
ggplot(grouped_per_medium, aes(x = year, y = n, color = medium_code, group = medium_code)) +
  geom_line() + 
  labs(title = "Number of reports on climate change per year and medium",
       x = "Year",
       y = "Number of Records") +
  scale_x_continuous(breaks = 2009:2024) +  # Zeige Jahre 2015 bis 2024 auf der x-Achse an
  scale_color_manual(values = c("NNBE" = "blue", "ZWAO" = "green", "NZZO" = "red", 
                                "NNTA" = "purple", "SRF" = "orange", "BLIO" = "cyan",
                                "BAZ" = "pink", "TLM" = "darkblue", "HEU" = "yellow" ), 
                     labels = medium_labels) +  # Benutze benutzerdefinierte Farben und Labels für die Medien
  theme_minimal()

df_de <- df %>%
  filter(language=="de")

grouped_per_medium_de <- df_de %>% 
  group_by(year, medium_code) %>%
  summarize(n = n()) 

grouped_per_year_de <- df_de %>% 
  group_by(year) %>% 
  summarize(n= n())

# Angepasste Labels für 'medium_code'
medium_labels_de <- c(
  "NNBE" = "Berner Zeitung",
  "ZWAO" = "20 Minuten Online",
  "NZZO" = "NZZ.ch",
  "NNTA" = "Tagesanzeiger.ch",
  "SRF" = "SRF.ch",
  "BLIO" = "Blick Online",
  "BAZ" = "Basler Zeitung"
)

ggplot(grouped_per_medium_de, aes(x = year, y = n, color = medium_code, group = medium_code)) +
  geom_line() + 
  labs(title = "Number of reports on climate change per year and medium",
       x = "Year",
       y = "Number of Records") +
  scale_x_continuous(breaks = 2009:2024) +  # Zeige Jahre 2015 bis 2024 auf der x-Achse an
  scale_color_manual(values = c("NNBE" = "blue", "ZWAO" = "green", "NZZO" = "red", 
                                "NNTA" = "purple", "SRF" = "orange", "BLIO" = "cyan",
                                "BAZ" = "pink"), 
                     labels = medium_labels) +  # Benutze benutzerdefinierte Farben und Labels für die Medien
  theme_minimal()

