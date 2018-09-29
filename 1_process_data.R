# Setup ====

# Load packages
library(stringi)
library(stringr)


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