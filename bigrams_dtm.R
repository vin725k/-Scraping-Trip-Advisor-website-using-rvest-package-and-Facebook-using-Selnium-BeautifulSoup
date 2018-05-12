text_bigrams = function(txt)
{
  
  require(stringr)
  
  #to make texts lowercase
  temp <-tolower(txt)
  
  temp <- bracketX(temp)
  temp <- stripWhitespace(temp)
  temp <- removePunctuation(temp)
  #install qdap package
  library(qdap)
  temp <- replace_abbreviation(temp)
  temp <- str_replace_all(temp,">", "") 
  temp <- str_replace_all(temp,"<", "")
  
  ### Print text without standard stop words
  temp <- removeWords(temp, stopwords("en"))
  temp  =  gsub("^\\s+|\\s+$", "", temp) 
  temp <- removeNumbers(temp)
  temp <- gsub("?(f|ht)tp(s?)://(.*)[.][a-z]+", "", temp)
  temp <- gsub("$","",temp)
  temp <- gsub("--","",temp)
  temp <- gsub("/","",temp)
  temp <-  gsub("\\W*\\b\\w\\b\\W*","",temp)
  temp  =  gsub("<.*?>", "", temp)
  require(tibble)
  textdf = data_frame(text = temp)
  tidy_text = textdf %>%   
    mutate(doc = row_number()) %>%
    unnest_tokens(bigram, text, 
                  token = "ngrams", n = 2) %>%
    group_by(doc)%>%
    count(bigram, sort=TRUE)
  bigrams_dtm <- tidy_text %>% cast_sparse(doc, bigram, n)
  return(bigrams_dtm)
}

