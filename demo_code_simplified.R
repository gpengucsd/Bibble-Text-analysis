
# load package ------------------------------------------------------------

library(stringr)
library(text2vec)
library(tidyverse)
library(tidytext)


# read data ---------------------------------------------------------------

data <- read.csv('data/bible.csv')

## prepare the whole book to text
data_combine <- data %>% 
  group_by(book) %>% 
  summarise(new=  paste(unlist(text), collapse =" "))

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

# word_to_vec -------------------------------------------------------------

it <- itoken(data_combine$new_clean, progressbar = FALSE)
v <- create_vocabulary(it) %>% 
  prune_vocabulary(doc_proportion_max = 0.1, term_count_min = 5)
vectorizer <- vocab_vectorizer(v)

dtm <- create_dtm(it, vectorizer)
tfidf <- TfIdf$new()
dtm_tfidf <- fit_transform(dtm, tfidf)


d1_d2_tfidf_cos_sim = sim2(x = dtm_tfidf, method = "cosine", norm = "l2")

## how book 5 similar to other books
sort(d1_d2_tfidf_cos_sim[5,])
