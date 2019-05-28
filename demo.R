library(tidyverse)
library(tidytext)
library(LDAvis)
data <- read.csv('bible.csv')%>%
  mutate(text = as.character(text))
chapter23 <- data %>%
  filter(chapter == 23)
words <- chapter23 %>%
  unnest_tokens(word,text)

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
data_combine <- data %>% group_by(Book)%>% summarise( new =  paste(unlist(text), collapse =" "))
library(stringr)
library(text2vec)

# select 500 rows for faster running times

prep_fun = function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    str_replace_all(paste(stopwords('en'), collapse = '\\b|\\b'),' ')%>%
    str_replace_all("\\W*\\b\\w\\b\\W*",' ')%>%
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ")%>%
    str_replace_all("[0-9]+", " ")%>%
    str_replace_all("\\bdavid\\b", " ")%>%
    str_replace_all("\\bjesus\\b", " ")%>%
    str_replace_all("\\bmoses\\b", " ")%>%
    str_replace_all("\\bchrist\\b", " ")%>%
    str_replace_all("\\bsaul\\b", " ")%>%
    str_replace_all("\\bjacob\\b", " ")%>%
    str_replace_all("\\bsolomon\\b", " ")%>%
    str_replace_all("\\baaron\\b", " ")%>%
    str_replace_all("\\bjoshua\\b", " ")%>%
    str_replace_all("\\bjoseph\\b", " ")%>%
    str_replace_all("\\babraham\\b", " ")
}
data_combine$new_clean = prep_fun(data_combine$new)


it = itoken(data_combine$new_clean, progressbar = FALSE)
v = create_vocabulary(it) %>% prune_vocabulary(doc_proportion_max = 0.5, term_count_min = 1)
vectorizer = vocab_vectorizer(v)



dtm = create_dtm(it, vectorizer)
tfidf = TfIdf$new()
dtm_tfidf = fit_transform(dtm, tfidf)


d1_d2_tfidf_cos_sim = sim2(x = dtm_tfidf, method = "cosine", norm = "l2")
sort(d1_d2_tfidf_cos_sim[33,],decreasing = TRUE)

######################## whole book!

data_combine <- data %>% group_by(Book)%>% summarise( new =  paste(unlist(text), collapse =" "))
library(stopwords)
stopwords_regex = paste(stopwords('en'), collapse = '\\b|\\b')
stopwords_regex = paste0('\\b', stopwords_regex, '\\b')
data_combine$new = stringr::str_replace_all(data_combine$new, stopwords_regex, '')
library(stringr)
library(text2vec)

# select 500 rows for faster running times

prep_fun = function(x) {
  x %>% 
    # make text lower case
    str_to_lower %>% 
    str_replace_all(paste(stopwords('en'), collapse = '\\b|\\b'),' ')%>%
    str_replace_all("\\W*\\b\\w\\b\\W*",' ')%>%
    # remove non-alphanumeric symbols
    str_replace_all("[^[:alnum:]]", " ") %>% 
    # collapse multiple spaces
    str_replace_all("\\s+", " ")%>%
    str_replace_all("[0-9]+", " ")%>%
    str_replace_all("\\bgt\\b", " ")%>%
    str_replace_all("\\bhour\\b", " ")
}

data_combine$new_clean = prep_fun(data_combine$new)

row.names(data_combine)<-data_combine$Book

it = itoken(data_combine$new_clean, progressbar = FALSE)
v = create_vocabulary(it) %>% prune_vocabulary(doc_proportion_max = 0.2, term_count_min = 1)
vectorizer = vocab_vectorizer(v)



dtm = create_dtm(it, vectorizer)
dtm@Dimnames[[1]] <- as.character(data_combine$Book)
tfidf = TfIdf$new()
dtm_tfidf = fit_transform(dtm, tfidf)


d1_d2_tfidf_cos_sim = sim2(x = dtm_tfidf, method = "cosine", norm = "l2")
####LSA
lsa = LSA$new(n_topics = 100)
dtm_tfidf_lsa = fit_transform(dtm_tfidf, lsa)
d1_d2_tfidf_cos_sim = sim2(x = dtm_tfidf_lsa, method = "cosine", norm = "l2")
d1_d2_tfidf_cos_sim[1:2, 1:5]

####LDA
set.seed(1234)
lda_model = LDA$new(n_topics = 7)
doc_topic_distr = lda_model$fit_transform(dtm, n_iter = 100)
lda_model$plot()
domTopic <- max.col(doc_topic_distr, ties.method = "first")
a<- data.frame( Book = data_combine$Book, topic = domTopic)


######Clustering
library(textmineR)
tf_mat <- TermDocFreq(dtm)
tfidf <- t(dtm[ , tf_mat$term ]) * tf_mat$idf

tfidf <- t(tfidf)
csim <- tfidf / sqrt(rowSums(tfidf * tfidf))

csim <- csim %*% t(csim)
cdist <- as.dist(1 - csim)
hc <- hclust(cdist, "ward.D")

clustering <- cutree(hc, 7)

plot(hc, main = "Hierarchical clustering of 100 NIH grant abstracts",
     ylab = "", xlab = "", yaxt = "n")

rect.hclust(hc, 7, border = "red")


p_words <- colSums(dtm) / sum(dtm)

cluster_words <- lapply(unique(clustering), function(x){
  rows <- dtm[ clustering == x , ]
  
  # for memory's sake, drop all words that don't appear in the cluster
  rows <- rows[ , colSums(rows) > 0 ]
  
  colSums(rows) / sum(rows) - p_words[ colnames(rows) ]
})
cluster_summary <- data.frame(cluster = unique(clustering),
                              size = as.numeric(table(clustering)),
                              top_words = sapply(cluster_words, function(d){
                                paste(
                                  names(d)[ order(d, decreasing = TRUE) ][ 1:5 ], 
                                  collapse = ", ")
                              }),
                              stringsAsFactors = FALSE)
cluster_summary


wordcloud::wordcloud(words = names(cluster_words[[ 5 ]]), 
                     freq = cluster_words[[ 5 ]], 
                     max.words = 50, 
                     random.order = FALSE, 
                     colors = c("red", "yellow", "blue"),
                     main = "Top words in cluster 100")
