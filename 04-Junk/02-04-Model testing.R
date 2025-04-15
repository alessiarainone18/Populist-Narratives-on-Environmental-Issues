library(tidyverse)
setwd("/Users/alessiarainone/Desktop/Data-Mining-Project_Climate-Change-Media-Attention/01-Data")

data <- read.csv("01-Data/random_sample.csv", sep = ",", header = TRUE)

# Test 
set.seed(123)
test_sample <- sample_n(filtered_data, 10)
write.csv(random_sample, "01-Data/test_sample.csv", row.names = FALSE)

