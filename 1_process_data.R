# Setup ====

# Load packages
library(dplyr)
library(flipTime)
library(NLP)
library(stringi)
library(stringr)
library(tm)


# Load data ====

load("data/0_scraped_data.RData")

# Concatenate all speech per speaker per debate ====
Document_List = list()


for (i in 1:length(Doc_List)){
  
  # Get date and title of debate
  title = trimws(str_split(string = Title_List[[i]], pattern = ":")[[1]][2])
  
  # For this analysis, we will not consider anything the moderators or audience member said.
  # We also won't try to attribute speech to any unknown speaker
  candidates = setdiff(unique(Speaker_List[[i]]), c("MODERATOR(S)", "AUDIENCE MEMBER:", "UNKNOWN"))
  candidate_speech = list()

  for (candidate in candidates){
    
    # Get all text spoken by candidate
    speech = Doc_List[[i]][which(Speaker_List[[i]] == candidate)]
    
    # Collapse all text spoken by candidate into a single text string
    speech = paste0(speech, collapse = " ")
    
    # Remove the colon in the name of the candidate
    candidate = gsub(pattern = ":", replacement = "", x = candidate, fixed = T)
    
    # Assign speech to list
    candidate_speech[[candidate]] = speech
    
  }
  
  # Assign all speeches to document list
  Document_List[[title]] = candidate_speech
 
}

remove(candidate, candidates, i, speech, candidate_speech, title)

# Remove unnecessary objects and write data to disc ====

remove(Doc_List, Speaker_List, Title_List)

save(Document_List, file = "data/1_concatenated_text.RData")

# Load data ====
load("data/1_concatenated_text.RData")
# Convert list to data frame of text documents ====

debates = names(Document_List)
k = 1
d = c()
db = c()
s = c()
t = c()

for (i in 1:length(Document_List)){
  
  speakers = names(Document_List[[debates[i]]])
  
  for (j in 1:length(speakers)){
    d[k] = str_split(string = debates[i], pattern = " - ")[[1]][2]#debates[i]
    db[k] = str_split(string = str_split(string = debates[i], pattern = " - ")[[1]][1], pattern = " Debate | Forum")[[1]][1]
    s[k] = speakers[j]
    t[k] = Document_List[[i]][[j]]
    k = k+1
  }
}

df = data.frame(date = d,
                debate = db,
                speaker = s,
                text = t)

df$doc_id = 1:nrow(df)

df = df %>%
  select(doc_id, text, date, debate, speaker) %>%
  mutate(party = ifelse(speaker %in% c("TRUMP", "PAUL", "CARSON", "RUBIO", "BUSH", "CRUZ", "CHRISTIE", "WALKER", 
                                       "HUCKABEE", "KASICH", "PERRY", "FIORINA", "PENCE"), "Republican", "Democrat"))



remove(Document_List, d, debates, i, j, k, s, speakers, t, db)


# Write data frame to disc ====

write.csv(df, file = "data/2_data_frame_documents.csv", row.names = FALSE)

# Read in data frame of documents ====

df = read.csv("data/2_data_frame_documents.csv", stringsAsFactors = FALSE)

df$date = AsDate(df$date)

# Convert data frame to DataFrameSource object ====

source_df = DataframeSource(x = df)
# Convert DataFrameSource to VCorpus object ====

corpus = VCorpus(x = source_df, 
                 readerControl = list(reader = readDataframe,
                                      language = "en"))

remove(source_df)
# Remove whitespace ====

corpus = tm_map(x = corpus, 
                FUN = stripWhitespace)

# Convert to lowercase ====

corpus = tm_map(x = corpus, 
                FUN = content_transformer(tolower))

# Remove any text in square brackets ====

corpus = tm_map(x = corpus,
                FUN = content_transformer(gsub), 
                pattern = "\\[[a-z]{0,}|[A-Z]{0,}|[0-9]{0,}|[;]{0,}|[-]{0,}|[.]{0,}|[,]{0,}\\]", 
                replacement = "")


# Remove stopwords ====

corpus = tm_map(x = corpus,
                FUN = removeWords,
                stopwords(kind = "en"))
                
# Stem text ====

corpus = tm_map(x = corpus,
                FUN = stemDocument)

# Remove punctuation ====

corpus = tm_map(x = corpus,
                FUN = content_transformer(gsub), 
                pattern = "[[:punct:] ]+", 
                replacement = " ")

# Assign metadata to each document ====

for (i in 1:length(corpus)){
  meta(corpus[[i]], tag = "author", type = "local") = df$speaker[i]
  meta(corpus[[i]], tag = "date", type = "local") = df$date[i]
  meta(corpus[[i]], tag = "debate", type = "local") = df$debate[i]
  meta(corpus[[i]], tag = "party", type = "local") = df$party[i]
}

remove(i)


# Save ====

save(corpus, file = "data/3_cleaned_corpus.RData")