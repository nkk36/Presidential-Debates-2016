# Setup ====

# Load packages
library(dplyr)
library(ldatuning)
library(magrittr) 
library(NLP)
library(quanteda)
library(slam)
library(stm)
library(stringi)
library(stringr)
library(tidytext) 
library(tidyverse) 
library(tm)
library(topicmodels)

# Load functions
source("R/custom_FindTopicsNumber_plot.R", chdir = T)
source("R/reorder_within.R", chdir = T)

# Load data ====

load("data/3_cleaned_corpus.RData")

df = read.csv("data/2_data_frame_documents.csv")


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


# LDA Tuning ====

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

custom_FindTopicsNumber_plot(topic_number, b = seq(0,50,5))

# Run LDA ====

lda = LDA(x = dtm, k = 13, method = "Gibbs", control = control_list_gibbs)

terms_lda = terms(lda, 5)

# LDA Evaluation ====

lemma_tm = lda %>%
  mutate(lda_gamma = map(.x=lda, 
                         .f=tidytext::tidy, 
                         matrix="gamma"))




# CTM ====



# STM ====

t = tidy(corpus)

processed = textProcessor(t$text, metadata = t)
out = prepDocuments(processed$documents, processed$vocab, processed$meta)
docs = out$documents
vocab = out$vocab
meta = out$meta

stm_no_covariate = stm(documents = out$documents, 
          vocab = out$vocab,
          K = 20, 
          prevalence =~ party,
          max.em.its = 75, 
          data = out$meta,
          init.type = "Spectral")

labelTopics(stm, c(6))

plot(stm_no_covariate, type = "summary", xlim = c(0, .3), labeltype = "frex")


td_beta <- tidy(stm_no_covariate)

td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free_y") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic",
       subtitle = "Different words are associated with different topics")

prep <- estimateEffect(1:20 ~ party, stm, meta = out$meta, uncertainty = "Global")

stm = stm(documents = out$documents, 
          vocab = out$vocab,
          K = 20, 
          prevalence =~ party,
          max.em.its = 75, 
          data = out$meta,
          init.type = "Spectral",
          content =~ party)

plot(prep, 
     covariate = "party", 
     topics = seq(1,20,1),
     model = stm, 
     method = "difference",
     cov.value1 = "Republican", 
     cov.value2 = "Democrat",
     xlab = "More Republican ... More Democrat",
     main = "Effect of Liberal vs. Conservative",
     xlim = c(-.5, .5), 
     labeltype = "lift")






