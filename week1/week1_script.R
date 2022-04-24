# text as data week 1 script


# working directory 
setwd('/Users/tklein/Desktop/Desktop_tpk/JHU_Classes/text_as_data/week1/')


# 1. libraries ------------------------------
library(tidyverse)
library(quanteda)
library(readtext)
library(quanteda.textplots)




# 2. reading in tweets -------------------------

nuc_tweets <- readtext('sentiment_nuclear_power _1_.csv', text_field = 'tweet_text')




# 3. creating corpus ------------------------------------

nuc_corpus <- corpus(nuc_tweets)


# summary of corpus

summary(nuc_corpus)



# viewing a text from the corpus

texts(nuc_corpus)[10]




# 4. getting total number of tokens ----------------------

ntoken(nuc_corpus) %>% sum()

# there are 4,423 total tokens in the nuclear tweets. 


# getting total number of tokens in the 5th tweet

ntoken(nuc_corpus[5])

# there are 32 tokens in the 5th tweet. 


# document frequency matrix ---------------------


dfm_nuclear <-  dfm(nuc_corpus)


# 5. create a word cloud ----------------------------


# a seed is used in quanteda and R to make random number generation reproducible. 
# whenever a function uses random number generation, if a seed is set it will follow
# the same random process every time. this allows for the random process to be regenerated 
# at different times, and on different machines. In quanteda this can be used when generating word clouds, 
# since a random process is used for the order and color (see random_order and random_color). 


set.seed(42)

textplot_wordcloud(dfm_nuclear)




set.seed(40)

textplot_wordcloud(dfm_nuclear)



corpus_subset(nuc_corpus, sentiment == 'Positive') %>% 
  dfm() %>% 
  textplot_wordcloud()




# 6. top 25 most frequent words ---------------------------------



dfm_nuclear %>% 
  topfeatures(25)









