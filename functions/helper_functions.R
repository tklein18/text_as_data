# useful functions for working with text as data 

# dependencies --------------------------------
library(readtext)
library(quanteda)
library(dplyr)


# csv to corpus -------------------------------

csv_to_corpus = function(file, text_col){
  
  temp_text = readtext(file = file, text_field = text_col)
  
  temp_text$doc_id = seq.int(nrow(temp_text))
  
  temp_corpus = corpus(temp_text)
  
  return(temp_corpus)
  
}



# corpus to dfm --------------------------------


corp_to_dfm = function(corpus, stopwords = quanteda::stopwords('english'), dict = NULL, thesaur = NULL, stem = F){
  
  if(!is.null(dict) & !is.null(thesaur)) {
    return(print("you can't provide a dictionairy and a thesaurus"))
    } else if(!is.null(dict)){
    
    temp_dict = quanteda::dictionary(dict)
    
    temp_dfm = quanteda::tokens(
      corpus, 
      remove_punct = T, 
      remove_symbols = T, 
      remove_url = T
    ) %>%
      dfm(
        tolower = T, 
        remove_padding = T, 
        stem = stem
      ) %>% 
      dfm_remove(
        pattern = stopwords
      ) %>%
      dfm_lookup(
        dictionary = temp_dict, 
        exclusive = T,
        capkeys = F
      )
    
    return(temp_dfm)
    
    } else if(!is.null(thesaur)){
    
      temp_thesaur = quanteda::dictionary(thesaur)
      
      temp_dfm = quanteda::tokens(
        corpus, 
        remove_punct = T, 
        remove_symbols = T, 
        remove_url = T
      ) %>%
        dfm(
          tolower = T, 
          remove_padding = T,
          stem = stem
        ) %>% 
        dfm_remove(
          pattern = stopwords
        ) %>%
        dfm_lookup(
          dictionary = temp_thesaur, 
          exclusive = F,
          capkeys = F
        )
      
      return(temp_dfm)
      
    } else {
    
      temp_dfm = quanteda::tokens(
        corpus, 
        remove_punct = T, 
        remove_symbols = T, 
        remove_url = T
      ) %>%
        dfm(
          tolower = T, 
          remove_padding = T,
          stem = stem
        ) %>% 
        dfm_remove(
          pattern = stopwords
        )
      
      return(temp_dfm)
      
      
  }
  
  
  
  
  
}






