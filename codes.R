library(dplyr)
library(tidytext)
library(ggplot2)

setwd('/Users/liuxiaotian/Documents/P136558_LiuXiaotian_DataManagementAssignment1')
data <- read.csv('Womens Clothing E-Commerce Reviews.csv')
str(data)
set.seed(123) # for reproducibility
sample_data <- data[sample(nrow(data), 500), ]  # Adjust the number as needed
str(sample_data)

summary(sample_data)
unique(sample_data$Rating)
unique(sample_data$Recommended.IND)
unique(sample_data$Division.Name)
unique(sample_data$Department.Name)
unique(sample_data$Class.Name)

hist(sample_data$Age, main = "Distribution of Age", xlab = "Age")

barplot(table(sample_data$Rating), main = "Count of Ratings", xlab = "Rating")
table(sample_data$Department.Name)

average_rating <- sample_data %>%
  group_by(Clothing.ID) %>%
  summarise(avg_rating = mean(Rating))
sorted_ratings <- average_rating %>%
  arrange(desc(avg_rating))
top_items <- head(sorted_ratings, 10)
average_rating <- average_rating %>%
  mutate(rating_range = cut(avg_rating, breaks = seq(0, 5, by = 1), labels = FALSE))
average_rating$rating_range <- factor(average_rating$rating_range)
ggplot(average_rating, aes(x = reorder(Clothing.ID, avg_rating), y = avg_rating, fill = rating_range)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("1" = "red", "2" = "orange", "3" = "yellow", "4" = "green", "5" = "blue"),
                    name = "Rating Range") +
  labs(title = "Average Ratings for Each Clothing ID",
       x = "Clothing ID",
       y = "Average Rating") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

library(tm)
library(tokenizers)
library(wordcloud)

review_text <- sample_data$Review.Text
review_text <- tolower(review_text)
review_text <- gsub("[^a-zA-Z\\s]", "", review_text)
tokens <- strsplit(review_text, "\\s+")
corpus <- Corpus(VectorSource(sample_data$Review.Text))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, function(x) unlist(tokenize_words(x)))
dtm <- DocumentTermMatrix(corpus)
mat <- as.matrix(dtm)
word_freq <- colSums(mat)
sorted_freq <- sort(word_freq, decreasing = TRUE)
top_words <- data.frame(word = names(sorted_freq)[1:50], freq = sorted_freq[1:50])

barplot(top_words$freq, names.arg = top_words$word, las = 2, 
        main = "Top 50 Words by Frequency", xlab = "Word", ylab = "Frequency", col = "skyblue")
wordcloud(words = top_words$word, freq = top_words$freq, scale = c(3, 0.5), 
          min.freq = 10, random.order = FALSE, colors = brewer.pal(8, "Dark2"))

library(tidyr)

tokens1 <- sample_data$Review.Text %>%
  tolower() %>%
  removePunctuation() %>%
  removeNumbers() %>%
  strsplit("\\s+") %>%
  unlist()
review_tokens <- data.frame(word = tokens1)
sentiment_scores <- review_tokens %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment_score = positive - negative)
sorted_sentiment <- sentiment_scores %>% arrange(desc(sentiment_score))
head(sorted_sentiment,50)
tail(sorted_sentiment,50)
str(sentiment_scores)
summary(sentiment_scores)

barplot(sorted_sentiment$sentiment_score, names.arg = sorted_sentiment$word, las = 2, 
        main = "Sentiment Scores by Word", xlab = "Word", ylab = "Sentiment Score", col = "skyblue")



