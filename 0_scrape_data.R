# Setup ====

# Load packages
library(rvest)
library(stringi)
library(stringr)

# Set parameters and initialize outputs ====

# URLs to scrape
urls = c("http://www.presidency.ucsb.edu/ws/index.php?pid=110489",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=110756",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=110906",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=110908",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=111177",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=111395",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=111412",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=111472",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=111500",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=111634",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=111711",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=115148",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=110903",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=110910",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=111178",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=111409",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=123351",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=111471",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=111520",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=112718",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=112719",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=116995",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=119012",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=118971",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=119038",
         "http://www.presidency.ucsb.edu/ws/index.php?pid=119039")

# Initialize counter and outputs
count = 1
Doc_List = list()
Speaker_List = list()
Title_List = list()

# Read and parse HTML ====
for (url in urls) {

  website = read_html(x = url)
  
  data = website %>%
    html_node(".displaytext") %>%
    html_nodes("p") %>%
    html_text()
  
  title = website %>%
    html_nodes("meta") %>%
    html_attr(name = "content")
  
  title = title[[4]]
    
  
  # Get names of speaker ====
  
  names = c()
  
  for (i in 1:length(data)){
    text_extract = str_extract(string = data[[i]], pattern = "^[\\s]{0,1}[A-Z]+[\\s]*[A-Z]*[:]") # "[A-Z]+[:]"
    names[i] = text_extract
  }
  
  names = unique(names)
  
  
  
  # Determine who is speaking in each line ====
  
  speakers = c()
  
  for (i in 1:length(data)){
    text_extract = str_extract(string = data[[i]], pattern = "^[\\s]{0,1}[A-Z]+[\\s]*[A-Z]*[:]") # "[A-Z]+[:]"
    
    speaker = names[match(text_extract, names)]
    
    speakers[i] = speaker
  }
  
  # Get documents ====
  
  beginning_of_new_speaker = which(is.na(speakers) == FALSE)
  docs = c()
  text = ""
  m = 1
  
  for (i in 1:(length(beginning_of_new_speaker) - 1)){
    
    diff = beginning_of_new_speaker[i + 1] - beginning_of_new_speaker[i]
    
    if (diff == 1){
      text = paste(text, data[beginning_of_new_speaker[i]], sep = "")
      docs[m] = text
      text = ""
      m = m + 1
    } else {
      for (k in beginning_of_new_speaker[i]:(beginning_of_new_speaker[i + 1] - 1)){
        text = paste(text, data[k], sep = " ")
      }
      docs[m] = text
      m = m + 1
      text = ""
    }
    
  }

  
  # Get speakers of each document ====
  
  speakers = c()
  docs2 = c()
  
  for (i in 1:length(docs)){
    text_extract = str_extract(string = docs[i], pattern = "^[\\s]{0,1}[A-Z]+[\\s]*[A-Z]*[:]") # "^[\\s]{0,1}[A-Z]+[:]"
    
    if (trimws(text_extract) %in% c("MODERATOR:", "MODERATORS:", "ANNOUNCER:", "KELLY:", "BAIER:", "WALLACE:", "TAPPER:", 
                                    "HEWITT:", "BASH:", "QUINTANILLA:", "HARWOOD:", "QUICK:", "CRAMER:", "SANTELLI:",
                                    "EPPERSON:", "CAVUTO:", "BARTIROMO:", "BAKER:", "BLITZER:", "DICKERSON:", "STRASSEL:", 
                                    "GARRETT:", "HANNITY:", "CUOMO:", "DINAN:", "COOPER:", "LEMON:", "WILKINS:", "LOPEZ:", 
                                    "CORDES:", "COONEY:", "OBRADOVICH:", "RADDATZ:", "MUIR:", "MCELVEEN:", "HOLT:", "MITCHELL:", 
                                    "TODD:", "MADDOW:", "WOODRUFF:", "IFILL:", "RAMOS:", "SALINAS:", "TUMULTY:", "LOUIS:", 
                                    "QUIJANO:", "HAM:")){
      speakers[i] = "MODERATOR(S)"
    } else if (trimws(text_extract) %in% c("UNKNOWN:", "UNIDENTIFIABLE:", "MALE:")){
      speakers[i] = "UNKNOWN"
    } else if (trimws(text_extract) %in% c("UNIDENTIFIED FEMALE:", "UNIDENTIFIED MALE:", "QUESTION:",  "AUDIENCE:", 
                                           "SEAN COLLISON:", "JOY LASSEN:", "LASSEN:", "ALEXIS:", "ARNOLD WOODS:", 
                                           "BISHOP:", "BRETT ROSENGREN:", "COLLISON:", "CRAWFORD:", "DEBORAH PLUMMER:", 
                                           "DICK GOODSON:", "GOODSON:", "JENNA BISHOP:", "PLUMMER:", "ROSENGREN:", "SMITH:", 
                                           "AUDIENCE MEMBER:", "FRANCHESCA RAMSEY:", "BROWNLEE:", "FRANTA:", "WILKINS",
                                           "LEVESQUE:")){
      speakers[i] = "AUDIENCE MEMBER:"
    } else {
      speakers[i] = trimws(text_extract)
    }
    
    n = nchar(text_extract)
    
    docs2[i] = trimws(substr(docs[i], n + 1, nchar(docs[i])))
    
  }
  
  # Save and  remove unnecessary objects ====
  
  documents = docs2
  
  Doc_List[[count]] = documents
  Speaker_List[[count]] = speakers
  Title_List[[count]] = title
  count = count + 1
  
  remove(website, beginning_of_new_speaker, diff, docs, docs2, i, k, m, n,
         names, speaker, data, text, text_extract, documents, speakers, title)


}

# Save ====
save(Doc_List, Speaker_List, Title_List, file = "data/0_scraped_data.RData")
# Load data ====

load("data/0_scraped_data.RData")