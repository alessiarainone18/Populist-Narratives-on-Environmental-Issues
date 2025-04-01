### ML APPROACH ---- 
testsample_data <- sample_n(filtered_data, 500)
write.xlsx(testsample_data, file="testsample_data.xlsx", overwrite = TRUE, asTable = TRUE)

# Handcoding done in Excel
sample_coded <- read.xlsx("coded_testsample.xlsx", sheet="coded data")         
sample_coded$relevance=as.factor(sample_coded$relevance)

# Training set
set.seed(123)  
train_indices <- sample(nrow(sample_coded), 400)
train <- sample_coded[train_indices, ]
train$text <- train$content
test <- sample_coded[-train_indices, ]  
test$text <- test$content

myCorpusTrain <- corpus(train)

tok2 <- tokens(myCorpusTrain , remove_punct = TRUE, remove_numbers=TRUE, 
               remove_symbols = TRUE, 
               split_hyphens = TRUE, remove_separators = TRUE, remove_url=TRUE)
tok2 <- tokens_remove(tok2, stopwords("de"))

# remove the unicode symbols
tok2 <- tokens_remove(tok2, c("0*"))
tok2 <- tokens_wordstem (tok2)
Dfm_train <- dfm(tok2)

# Let's trim the dfm in order to keep only tokens that appear in 2 or more tweets (tweets are short texts!)
# and let's keep only features with at least 2 characters
Dfm_train <- dfm_trim(Dfm_train , min_docfreq = 2, verbose=TRUE)
Dfm_train  <- dfm_remove(Dfm_train , min_nchar = 2)
topfeatures(Dfm_train , 20)  # 20 top words

# TEST SET
myCorpusTest <- corpus(test)
tok <- tokens(myCorpusTest , remove_punct = TRUE, remove_numbers=TRUE, 
              remove_symbols = TRUE, 
              split_hyphens = TRUE, remove_separators = TRUE, remove_url=TRUE)
tok <- tokens_remove(tok, stopwords("de"))
tok <- tokens_remove(tok, c("0*"))
tok <- tokens_wordstem (tok)
Dfm_test <- dfm(tok)
Dfm_test<- dfm_trim(Dfm_test, min_docfreq = 2, verbose=TRUE)
Dfm_test<- dfm_remove(Dfm_test, min_nchar = 2)
topfeatures(Dfm_test , 20)  # 20 top words

setequal(featnames(Dfm_train), featnames(Dfm_test)) 
nfeat(Dfm_test)
nfeat(Dfm_train)
test_dfm  <- dfm_match(Dfm_test, features = featnames(Dfm_train))
nfeat(test_dfm)
setequal(featnames(Dfm_train), featnames(test_dfm))

train <- as(Dfm_train, "dgCMatrix") 
test <- as(test_dfm, "dgCMatrix") 

# Machine Learning Model to categorize relevance ---- 
# Bernoulli Naive Bayes model
system.time({
  NB <- bernoulli_naive_bayes(x = train, y = Dfm_train@docvars$relevance)
})

# Make predictions on the test set
predicted_nb <- predict(NB, test)

# Check prior probabilities
priors <- prop.table(table(Dfm_train@docvars$relevance))
print(priors)

# Generate the confusion matrix
conf_matrix <- table(Predicted = predicted_nb, Actual = Dfm_test@docvars$relevance)
print(conf_matrix) ## 


### Use on whole data---- 
# Preprocess your whole dataset (assuming 'myCorpusWholeData' is your full corpus)
filtered_data$text <- filtered_data$content
myCorpusWholeData <- corpus(filtered_data)  # Replace with your actual data
tok_whole <- tokens(myCorpusWholeData, remove_punct = TRUE, remove_numbers = TRUE, 
                    remove_symbols = TRUE, split_hyphens = TRUE, 
                    remove_separators = TRUE, remove_url = TRUE)
tok_whole <- tokens_remove(tok_whole, stopwords("en"))
tok_whole <- tokens_wordstem(tok_whole)

# Create the DFM for the whole data
Dfm_whole <- dfm(tok_whole)

# Trim and remove low-frequency words, similar to the training data
Dfm_whole <- dfm_trim(Dfm_whole, min_docfreq = 2, verbose = TRUE)
Dfm_whole <- dfm_remove(Dfm_whole, min_nchar = 2)

# Ensure that features in the whole data match with the training data
Dfm_whole_matched <- dfm_match(Dfm_whole, features = featnames(Dfm_train))
whole_data <- as(Dfm_whole_matched, "dgCMatrix")

# Use the trained model to predict the relevance of the whole dataset
predicted_whole_data <- predict(NB, whole_data)

# Save predictions in a new column in your original dataset (if desired)
filtered_data$predicted_relevance <- predicted_whole_data

## See how many articles are seen as relevant
filtered_data %>%
  count(predicted_relevance)

# Still 9291 articles (too many)

# Exclude local politics
# local_terms <- c("Gemeinderat", "Stadtrat", "Ortsparlament", "Quartierverein",
#                  "Gemeindeversammlung", "Baukommission", "Ortsplanung")

# Filter general on polictis and Switzerland
political_terms <- c(
  # Institutions
  "Bundesrat", "Parlament", "Nationalrat", "Ständerat", "Kantonsrat", "Gemeinderat",
  "Regierung", "Exekutive", "Legislative", "Judikative", "Volksabstimmung", "Referendum", "Initiative",
  
  # Political processes
  "Koalition", "Mehrheit", "Opposition", "Fraktion", "Gesetzesvorschlag", "Abstimmung", "Wahlen",
  "Kandidatur", "Stimmrecht", "Wahlkampf", "Volkswahl", "Listenverbindung", "Proporz", "Majorz",
  
  # Political Discourse
  "Kommission", "Motion", "Postulat", "Interpellation", "Petition", "Vernehmlassung", "Parlamentsdebatte",
  "Plenarsitzung", "Session", "Gesetzesentwurf", "Parlamentsbeschluss",
  
  # Political Roles
  "Parteipräsident", "Abgeordneter", "Mandat", "Staatssekretär", "Regierungsrat", "Kantonsregierung",
  "Bürgermeister", "Gemeindepräsident", "Landammann", "Oppositionsführer", "Stimmenzähler",
  
  # Political Topics
  "Klimapolitik", "Umweltgesetz", "Energiegesetz", "Subvention", "Verfassungsänderung",
  "Steuergesetz", "Verordnung", "Sozialpolitik", "Asylpolitik", "Wirtschaftspolitik")

# swiss_terms <- c("Schweiz", "Schweizer", "eidgenössisch", "national", "kantonal", "Regierungsrat", "Grossrat", "Kantonsrat")

filtered_pol <- filtered_data %>%
  filter(grepl(paste(political_terms, collapse = "|"), content, ignore.case = TRUE)) %>%
  # filter(grepl(paste(swiss_terms, collapse = "|"), content, ignore.case = TRUE)) %>%
  filter(rubric != c("international", "Ausland"), medium_code != "BAZ", medium_code != "NNBE")

filtered_pol %>%
  count(predicted_relevance)

# Check on random sample
check_sample <- sample_n(filtered_pol, 100)
check_sample %>%
  count(predicted_relevance)

table_result <- table(check_sample$head, check_sample$predicted_relevance)
table_result
