#Task-04

## PREPARING THE DATA FOR ANALYSIS
# importing the given data
TwitterData <- read.csv("C://Users//HP//Desktop//Prodigy//Task4//twitter_training.csv")
View(TwitterData)
colnames(TwitterData) <- c('Tweet_ID', 'Topic', 'Sentiment', 'Tweet')
#checking for any missing values
which(is.na(TwitterData))

#ANALYSIS
#importing required libraries
library("ggplot2")
library("tidytext")
library("wordcloud")
library("dplyr")
library("reshape2")

#Splitting the tweets into individual words
twitter_tokens <- TwitterData %>% unnest_tokens(word, Tweet)
#performing sentiment analysis using Bing Lexicon
sentiment_bing <- twitter_tokens%>% 
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(Sentiment, word, sort = TRUE)
#View the results
head(sentiment_bing)

#creating word cloud
word_counts <- twitter_tokens %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
#Plotting the word cloud
set.seed(19)
wordcloud(words = word_counts$word, freq = word_counts$n,
          max.words = 100, colors = brewer.pal(8, 'Dark2'))

#Bar Plot of Sentiment distribution
sentiment_distribution <- twitter_tokens %>% 
  inner_join(get_sentiments("bing")) %>%
  count(sentiment)
ggplot(sentiment_distribution, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = 'identity') +
  labs(title = "Sentiment Distribution",
       x = "Sentiment", y = "Count") +
  theme_minimal()

  