
createDTM = function(txt)
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
  temp  =  gsub("<.*?>", "", temp)               # regex for removing HTML tags
  text1_source <- VectorSource(temp)
  text1_corpus <- VCorpus(text1_source)
  dtm1 <- DocumentTermMatrix(text1_corpus)
  dtm1 <- as.matrix(dtm1)
  return(dtm1)
}
