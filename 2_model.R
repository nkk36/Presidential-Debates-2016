# Setup ====

# Load packages
library(ldatuning)
library(NLP)
library(stringi)
library(stringr)
library(tm)
library(topicmodels)

# Load data ====

load("data/3_cleaned_corpus.RData")


# Convert to document term matrix ====

# Weighting options: weightTF, weightTfIdf, weightBin, weightSMART
dtm = DocumentTermMatrix(corpus, control = list(weighting = weightTf))

# Remove sparse terms if you want
#removeSparseTerms(x = dtm, sparse = 0.9)


# Run LDA ====

lda = LDA(x = dtm, k = 10, method = "VEM", control = NULL, model = NULL)

terms_lda = terms(lda, 10)
