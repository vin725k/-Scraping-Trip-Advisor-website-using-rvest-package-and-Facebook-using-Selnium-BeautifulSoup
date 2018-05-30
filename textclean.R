
cltxt = function(txt)
{
  
  require(stringr)
  
  #to make texts lowercase
  temp <-tolower(txt)
  
  temp <- bracketX(temp)
  temp <- stripWhitespace(temp)
  temp <- removePunctuation(temp)
  #install qdap package
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
  return(temp)
}