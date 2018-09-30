# Setup ====

# Load packages
library(ldatuning)
library(magrittr) 
library(NLP)
library(slam)
library(stringi)
library(stringr)
library(tidytext) 
library(tidyverse) 
library(tm)
library(topicmodels)

# Load functions
source("R/custom_FindTopicsNumber_plot.R", chdir = T)

# Load data ====

load("data/3_cleaned_corpus.RData")


# Convert to document term matrix ====

# Weighting options: weightTF, weightTfIdf, weightBin, weightSMART
dtm = DocumentTermMatrix(corpus, control = list(weighting = weightTf))

# Remove sparse terms if you want
#removeSparseTerms(x = dtm, sparse = 0.9)

# Remove terms based on mean TFIDF ====

summary(col_sums(dtm))


term_tfidf = tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
summary(term_tfidf)

dtm = dtm[, term_tfidf >= 0.003 & term_tfidf <= .004]
dtm = dtm[row_sums(dtm) > 0,]
summary(col_sums(dtm))

dim(dtm)


# Run LDA ====

control_list_gibbs = list(burnin = 2500,
                          iter = 5000,
                          seed = 0:4,
                          nstart = 5, 
                          best = TRUE)

system.time(expr = topic_number <- FindTopicsNumber(dtm = dtm,
                                    topics = c(seq(from = 2, to = 9, by = 1), seq(10, 20, 2), seq(25, 50, 5)),
                                    metrics = c( "Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                                    method = "Gibbs",
                                    control = control_list_gibbs,
                                    mc.cores = 2,
                                    verbose = TRUE))

FindTopicsNumber_plot(topic_number)

custom_FindTopicsNumber_plot(topic_number, b = seq(0,50,5))

lda = LDA(x = dtm, k = 10, method = "VEM", control = NULL, model = NULL)


terms_lda = terms(lda, 10)
