# title: "MovieLens Recommendation System - Capstone"
# author: "Martinez Maldonado Matías"

if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(caret)) install.packages("caret")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(lubridate)) install.packages("lubridate")

# Libraries we need
library(tidyverse)
library(caret)
library(lubridate)
library(stringr)

# Data Loading
# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Function that return the RMSE value
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}
#################################################################################################
                                         # Pre-Proccesing data
#################################################################################################

# Creating a long version of both validation and train data set separating genres per row

edx_temp <- edx %>% separate_rows(genres, sep = "\\|", convert = T)


final_holdout_test <- final_holdout_test %>% separate_rows(genres, sep = "\\|", convert = T)

# Convert timestamp predictor into a human most readable format
edx_temp$year <- edx_temp$timestamp %>% as_datetime() %>% year()
edx_temp$month <- edx_temp$timestamp %>% as_datetime() %>% month()

#Extract the release date from title to a new predictor
edx_temp <- edx_temp %>% mutate(release_date = title %>% str_extract_all("\\([0-9]{4}\\)") %>%
                 str_extract("[0-9]{4}") %>% as.numeric(),
               title = title %>% str_remove("\\([0-9]{4}\\)")%>% str_trim("right"))

# Doing the same with the validation dataset in one step
final_holdout_test <- final_holdout_test %>% mutate(release_date = title %>% str_extract_all("\\([0-9]{4}\\)") %>%
                               str_extract("[0-9]{4}") %>% as.numeric(),
                             title = title %>% str_remove("\\([0-9]{4}\\)")%>% str_trim("right"),
                             year = timestamp %>% as_datetime() %>% year(),
                             month = timestamp %>% as_datetime() %>% month())

edx_temp <- edx_temp %>% select(-timestamp)
final_holdout_test <- final_holdout_test %>% select(-timestamp)
#################################################################################################
                                         # Analizing data
#################################################################################################

# Data Structure
str(edx_temp) 

# Data Summary
summary(edx_temp)

# Movies rated vs Users
edx %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId)) 

# Frequency Rate 
edx_temp %>% select(rating, title) %>% group_by(rating) %>%
  summarize(count = n()) %>%
  arrange(desc(count))


# Frequency of Ratings
max_nratings <- edx_temp %>% select(rating, title) %>% group_by(rating) %>%
  summarize(count = n()) %>% max()
edx_temp %>% select(rating, title) %>% group_by(rating) %>%
  summarize(count = n()) %>% ggplot(aes(rating, count, fill = count)) +
  geom_col() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  ggtitle("Frequency of ratings") +
  scale_y_continuous("Frequency",
                     breaks = c(0, 1000000, 2000000, max_nratings),
                     labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_x_continuous("Rate Value",
                     limits = c(0,5.5), 
                     breaks = c(0, 1, 2, 3, 4, 5),
                     labels = c("0", "1", "2", "3", "4", "5"))

# Release Date Distribution
edx_temp %>% group_by(release_date) %>% summarize(count_rating = n()) %>% 
  ggplot(aes(release_date, count_rating)) +
  geom_line()

# Average Rating by Release Date
edx_temp %>% group_by(release_date) %>%
  summarise(rating = mean(rating)) %>%
  ggplot(aes(release_date, rating)) +
  geom_smooth() +
  labs(x = "Release Date", y = "Average Rating") 

# Most 20 rated movies
edx_temp %>% group_by(movieId, title) %>%
  summarize(count_rates = n()) %>%
  arrange(desc(count_rates)) %>% head(20) %>%
  ggplot(aes(reorder(title, count_rates, decreasing = TRUE), count_rates)) +
  geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  labs(title = "Ratings Frequency Distribution - TOP 20 Movies",
       x = "Title", y = "Frequency")

# Top 20 Less Rated Movies
edx_temp %>% group_by(movieId, title) %>%
  summarize(count_rates = n()) %>%
  arrange(count_rates) %>% head(20) %>%
  ggplot(aes(reorder(title, count_rates, decreasing = TRUE), count_rates)) +
  geom_col() + theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  labs(title = "Ratings Frequency Distribution - TOP 20 Less Rated Movies",
       x = "Title", y = "Frequency")

# Trend Movie Ratings
edx_temp %>% 
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

# Number of rating for each movie genres
edx_temp %>% 
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>% ggplot(aes(genres, count, fill = genres)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Number of Rating for Each Genre") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90)) 


# Number of Ratings for Each Genre 
edx_temp %>% 
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>% ggplot(aes(genres, count, fill = genres)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Number of Ratings for Each Genre", x = "Genres", y = "Frequency" ) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90)) 

#Number of Ratings for Each Genre 
edx_temp %>% 
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) 

# Median Ratings by Genre
edx_temp %>% 
  group_by(genres) %>%
  summarize(median = median(rating)) %>%
  ggplot(aes(genres, median, fill = genres)) + 
  geom_bar(stat = "identity") + 
  labs(title = "Number of Rating for Each Genre", x = "Genres", y = "Median") +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90))


# Mean Rating by Genre (genres with more than 1000 ratings)
edx_temp %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#################################################################################################
                                     # Creating the model
#################################################################################################
# Create data partition
set.seed(1, sample.kind="Rounding")
index_test <- createDataPartition(edx_temp$rating, times = 1, p=.25, list=FALSE)

train_set <- edx_temp %>% slice(-index_test)
temp <- edx_temp %>% slice(index_test) 

test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set) 
train_set <- rbind(train_set, removed)

rm(index_test, temp, removed) 

# Let's start with a naive approach 
mu <- mean(train_set$rating)

naive_rmse <- RMSE(test_set$rating, mu)

results <- tibble(method = "Just the average", RMSE = naive_rmse)

# Movie effect method
movie_avgs <- train_set %>% group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))

movie_effect <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

rmse_movie_effect <- RMSE(test_set$rating, movie_effect)

results <- results %>% add_row(method="Movie Effect Model", RMSE=rmse_movie_effect)

# Movie + User effect method

user_avgs <- train_set %>% left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i ))

user_effect <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by = 'userId') %>% 
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

rmse_user_effect <- RMSE(test_set$rating, user_effect)

results <- results %>% add_row(method="Movie + User Effect Model", RMSE = rmse_user_effect)

# Movie + User + Genre effect method

genres_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

genres_effect <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by = 'userId') %>% 
  left_join(genres_avgs, by = 'genres') %>% 
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

rmse_genres_effect <- RMSE(test_set$rating, genres_effect)

results <- results %>% add_row(method = "Movie + User + Genres Effect Model", RMSE = rmse_genres_effect)

# Movie + User + Genres + Release Date effect method

release_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  left_join(genres_avgs, by = "genres") %>%
  group_by(release_date) %>%
  summarize(b_r = mean(rating - mu - b_i - b_u - b_g ))

release_effect <- test_set %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by = 'userId') %>% 
  left_join(genres_avgs, by = "genres") %>%
  left_join(release_avgs, by = 'release_date') %>% 
  mutate(pred = mu+ b_i + b_u + b_g + b_r) %>%
  pull(pred)

rmse_release_effect <- RMSE(test_set$rating, release_effect)

results <- results %>% add_row(method = "Movie + User + Genres +  Release Date Effect Model", RMSE = rmse_release_effect)

# Regularization

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- train_set %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by = 'userId') %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - mu - b_i - b_u )/ (n()+ l))
  
  b_r <- train_set %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by = 'userId') %>%
    left_join(b_g,by = "genres") %>%
    group_by(release_date) %>%
    summarize(b_r = sum(rating - mu - b_i - b_u - b_g )/ (n()+ l))
  
  
  predicted_ratings <- test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_r, by = "release_date") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_r) %>%
    .$pred
  
  return(RMSE(predicted_ratings, test_set$rating))
})

optimal_lambda <- lambdas[which.min(rmses)]

data.frame(lambdas, rmses) %>%
  ggplot(aes(lambdas, rmses)) +
  geom_point() +
  geom_hline(yintercept = min(rmses), linetype='dotted', col = "red") +
  annotate("text", x = optimal_lambda, y = min(rmses), label = optimal_lambda, vjust = -1, color = "red") +
  labs(x = "Lambda", y = "RMSE", caption = "Source: train dataset") 


results <- results %>% add_row(method= 'Regularized Movie + User + Genres + Release Date Effect Model', RMSE = min(rmses))

#################################################################################################
                                      # Final Test
#################################################################################################

# Final test
  
final_b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+ optimal_lambda))

final_b_u <- train_set %>% 
  left_join(final_b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - mu - b_i)/(n()+ optimal_lambda))

final_b_g <- train_set %>% 
  left_join(final_b_i, by="movieId") %>%
  left_join(final_b_u, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - mu - b_i - b_u)/(n()+ optimal_lambda))

final_b_r <- train_set %>% 
  left_join(final_b_i, by="movieId") %>%
  left_join(final_b_u, by = "userId") %>%
  left_join(final_b_g, by = "genres") %>%
  group_by(release_date) %>%
  summarize(b_r = sum(rating - mu - b_i - b_u - b_g)/(n()+ optimal_lambda))

final_rmse <- final_holdout_test %>% 
  left_join(final_b_i, by = "movieId") %>%
  left_join(final_b_u, by = "userId") %>%
  left_join(final_b_g, by = "genres") %>%
  left_join(final_b_r, by = "release_date") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_r) %>% 
  .$pred


# Final Result
RMSE(final_holdout_test$rating, final_rmse)


