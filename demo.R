library(tidyverse)
library(tidytext)
data <- read.csv('t_web.csv')%>%
  mutate(field.4 = as.character(field.4))
chapter23 <- data %>%
  filter(field.1 == 23)
words <- chapter23 %>%
  unnest_tokens(word,field.4)

stopword_count<- words%>%
  count(word) %>%
  anti_join(stop_words)%>%
  arrange(desc(n))
wordcount <-words %>%
  count(word)%>%
  arrange(desc(n))


wordcount %>%
  top_n(25) %>%
  ggplot(aes(x=fct_reorder(word,n),y=n)) + geom_bar(stat='identity') + 
  coord_flip() + theme_bw()+
  labs(title='Top 25 Words',
       x='Count',
       y= 'Word')


stopword_count %>%
  top_n(25) %>%
  ggplot(aes(x=fct_reorder(word,n),y=n)) + geom_bar(stat='identity') + 
  coord_flip() + theme_bw()+
  labs(title='Top 25 Words',
       x='Count',
       y= 'Word')

customWords <- c('yahweh','god','israel','egypt')
#########TFIDF
library(stringr)
library(text2vec)
word_field2 <- words%>%
  filter(!word %in% customWords)%>%
  count(field.2,word) %>%
  anti_join(stop_words)%>%
  arrange(desc(n)) 

wordTFIDF <- word_field2 %>%
  bind_tf_idf(word,field.2,n) %>%
  group_by(field.2) %>%
  arrange(desc(tf_idf)) %>%
  slice(1:15) %>% # get top 15 words in terms of tf-idf
  ungroup()

#####################
chapter19 <- data %>%
  filter(field.1 == 19)

chapter19_combine <- chapter19 %>% group_by(field.2)%>% summarise( new =  paste(unlist(field.4), collapse =" "))
library(stringr)
library(text2vec)

# select 500 rows for faster running times

prep_fun = function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ")
}

chapter19_combine$new_clean = prep_fun(chapter19_combine$new)


it = itoken(chapter19_combine$new_clean, progressbar = FALSE)
v = create_vocabulary(it) %>% prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v)



dtm = create_dtm(it, vectorizer)
tfidf = TfIdf$new()
dtm_tfidf = fit_transform(dtm, tfidf)


d1_d2_tfidf_cos_sim = sim2(x = dtm_tfidf, method = "cosine", norm = "l2")
d1_d2_tfidf_cos_sim
######################## whole book!
data_combine <- data %>% group_by(field.1)%>% summarise( new =  paste(unlist(field.4), collapse =" "))
library(stringr)
library(text2vec)

# select 500 rows for faster running times

prep_fun = function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ")
}

data_combine$new_clean = prep_fun(data_combine$new)


it = itoken(data_combine$new_clean, progressbar = FALSE)
v = create_vocabulary(it) %>% prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
vectorizer = vocab_vectorizer(v)



dtm = create_dtm(it, vectorizer)
tfidf = TfIdf$new()
dtm_tfidf = fit_transform(dtm, tfidf)


d1_d2_tfidf_cos_sim = sim2(x = dtm_tfidf, method = "cosine", norm = "l2")
sort(d1_d2_tfidf_cos_sim[5,])
