library(tidytext)
library(tidyverse)
library('dplyr')
library(ggplot2)
library(scales)
library(plotly)


bible <- read_csv("bible.csv")


bibleTidy <- bible %>%
  select(book,chapter,text) %>%
  unnest_tokens(word,text)%>%
  filter(!word %in% c("god"))


sentRev <- bibleTidy %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(book) %>%
  summarize(sentiment = sum(score)) %>%
  left_join(bibleLength,by='book') %>%
  mutate(aveSentiment = sentiment/bibleLength)




sentRev %>%
  group_by(book) %>%
  summarize(meanSent = mean(aveSentiment)) %>%
  ggplot(aes(x=book,y=meanSent,color=book)) + geom_point(size=5,show.legend = F) + 
  geom_hline(aes(yintercept=0)) +
  labs(title='Average Sentiment by book',
       x = 'book',
       y = 'Average Sentiment')



test <- sentRev %>%
  group_by(book) %>%
  summarize(meanSent = mean(aveSentiment))


plot_ly(data = test, x = ~book, y = ~meanSent,color = ~meanSent,colors = "Accent",
        marker = list(size = 10)) 
