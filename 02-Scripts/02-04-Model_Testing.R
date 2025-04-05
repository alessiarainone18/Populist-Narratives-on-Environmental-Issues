library(tidyverse)
setwd("/Users/alessiarainone/Desktop/Data-Mining-Project_Climate-Change-Media-Attention/01-Data")

data <- read.csv("random_sample.csv", sep = ",", header = TRUE)

# Save test data 
set.seed(123)
test_sample <- sample_n(filtered_data, 20)
write.csv(random_sample, "test_sample.csv", row.names = FALSE)


print(test_sample$content)

